namespace BiodiversityCoder.Core

open Elmish
open Bolero
open Bolero.Html
open FieldDataTypes

/// Scenarios represent common work patterns of nodes and relations, which
/// may be partly automated through the automation function.
module Scenarios =

    /// A master DU representing all possible scenarios configured
    /// in the system.
    type Scenario =
        | WoodRing of WoodRingScenario

    /// Special case of wood (tree/shrub) ring chronologies, which
    /// only require minimal details of space and time for a single taxon.
    and WoodRingScenario = {
        [<Name("Should taxa be created if they don't exist in the graph?")>]
        [<Help("If false, will require you to create the proxy and taxon nodes in the 'Population' tab. If true, will automatically create these nodes if they don't exist.")>]
        CreateTaxon: bool
        [<Name("Site name")>]
        SiteName: Text.ShortText
        Location: Geography.SamplingLocation
        [<Name("Earliest year")>]
        [<Help("This should be the earliest age of the plants, rather than the start of a dendro-chronology. Note the oldest plant is usually older than the start of the chronology.")>]
        EarliestYear: float<OldDate.AD>
        [<Name("Latest year")>]
        LatestYear: float<OldDate.AD>
        [<Name("Year in which the wood disc or increment core was collected")>]
        CollectionDate: float<OldDate.AD>
        [<Name("Taxonomic rank of identification")>]
        Taxon: WoodTaxon
    } with 
        static member Title = "Wood ring chronology"
        static member Description = "For sources that construct standard tree or shrub ring chronologies, use this scenario to quickly enter details for a particular site."
        
    and WoodTaxon =
        | Genus of genus:Text.ShortText
        | Species of generic:Text.ShortText * specific:Text.ShortText * authorship:Text.ShortText
        | Subspecies of generic:Text.ShortText * specific:Text.ShortText * subspecific:Text.ShortText * authorship:Text.ShortText

    let scenarioGen<'a> model dispatch =
        ViewGen.makeNodeForm<'a> model [] dispatch

    let tryMakeScenario t (n:obj) =
        try
            match t with
            | (t: System.Type) when t = typeof<WoodRingScenario> -> n :?> WoodRingScenario |> Ok
            | _ -> Error <| sprintf "Not a known node type (%s)" t.Name
        with | e -> Error <| sprintf "Failed to make a node: %s" e.Message


    module Automators =

        open GraphStructure
        open Exposure.StudyTimeline
        open Population

        let private removeUnit (x:float<_>) = float x |> int

        let automateTreeRing (vm:WoodRingScenario) sourceNode graph =

            let timelineNode =
                ExposureNode <|
                Exposure.ExposureNode.TimelineNode(
                    Continuous <| Regular (1.<OldDate.calYearBP>, WoodAnatomicalFeatures))

            let individualDate =
                ExposureNode <|
                Exposure.ExposureNode.DateNode {
                    Date = OldDate.OldDatingMethod.CollectionDate <| vm.CollectionDate
                    MaterialDated = Text.createShort "wood increment" |> Result.forceOk
                    SampleDepth = None
                    Discarded = false
                    MeasurementError = OldDate.MeasurementError.NoDatingErrorSpecified
                }
            
            let contextNode = PopulationNode <| ContextNode {
                Name = vm.SiteName
                SamplingLocation = vm.Location
                SampleOrigin = Population.Context.LivingOrganism
                SampleLocationDescription = None
            }

            result {
                // Find existing nodes to relate to:
                let! startDateNode = Storage.atomByKey (Graph.UniqueKey.FriendlyKey("calyearnode", sprintf "%iybp" <| 1950 - (removeUnit vm.EarliestYear))) graph |> Result.ofOption ""
                let! endDateNode = Storage.atomByKey (Graph.UniqueKey.FriendlyKey("calyearnode", sprintf "%iybp" <|1950 - (removeUnit vm.LatestYear))) graph |> Result.ofOption ""
                let! collectionDateNode = Storage.atomByKey (Graph.UniqueKey.FriendlyKey("calyearnode", sprintf "%iybp" <|1950 - (removeUnit vm.CollectionDate))) graph |> Result.ofOption ""
                let! measureNode = Storage.atomByKey (Graph.UniqueKey.FriendlyKey("biodiversitydimensionnode", "presence")) graph |> Result.ofOption ""
                let! proxyTaxon = 
                    match vm.Taxon with
                    | Genus g -> sprintf "%s sp." g.Value |> Text.createShort |> Result.lift BioticProxies.ContemporaneousWholeOrganism
                    | Species (g,s,auth) -> (sprintf "%s %s %s" g.Value s.Value auth.Value) |> Text.createShort |> Result.lift BioticProxies.ContemporaneousWholeOrganism
                    | Subspecies (g,s,ssp,auth) -> (sprintf "%s %s ssp. %s" g.Value s.Value ssp.Value) |> Text.createShort |> Result.lift BioticProxies.ContemporaneousWholeOrganism
                let! existingTaxonNode = 
                    match vm.Taxon with
                    | Genus g -> makeUniqueKey(PopulationNode (TaxonomyNode (Taxonomy.TaxonNode.Genus g)))
                    | Species (g,s,auth) -> makeUniqueKey(PopulationNode (TaxonomyNode (Taxonomy.TaxonNode.Species (g,s,auth))))
                    | Subspecies (g,s,ssp,auth) -> makeUniqueKey(PopulationNode (TaxonomyNode (Taxonomy.TaxonNode.Subspecies (g,s,ssp,auth))))
                    |> fun k -> Storage.atomByKey k graph 
                    |> Result.ofOption (sprintf "Cannot find taxon. Create %A first in BiodiversityCoder." vm.Taxon)                    
                let! existingTaxon =
                    match fst existingTaxonNode |> snd with
                    | Node.PopulationNode p ->
                        match p with
                        | TaxonomyNode t -> Ok t
                        | _ -> Error "Not a taxon node"
                    | _ -> Error "Not a taxon node"

                // Add new validated structure into graph:
                let! newGraph = 
                    graph
                    |> fun g -> Storage.addNodes g [ 
                        timelineNode
                        individualDate
                        contextNode ]
                    |> Result.bind(fun (g, addedNodes) -> 
                        let timelineNode = addedNodes |> Seq.find(fun s -> (s |> fst |> snd).NodeType() = "IndividualTimelineNode")
                        let dateNode = addedNodes |> Seq.find(fun s -> (s |> fst |> snd).NodeType() = "IndividualDateNode")
                        let contextNode = addedNodes |> Seq.find(fun s -> (s |> fst |> snd).NodeType() = "ContextNode")
                        
                        Storage.addRelation sourceNode timelineNode (ProposedRelation.Source Sources.SourceRelation.HasTemporalExtent) g
                        |> Result.bind(fun g -> Storage.addRelation timelineNode startDateNode (ProposedRelation.Exposure Exposure.ExposureRelation.ExtentEarliest) g )
                        |> Result.bind(fun g -> Storage.addRelation timelineNode endDateNode (ProposedRelation.Exposure Exposure.ExposureRelation.ExtentLatest) g )
                        |> Result.bind(fun g -> Storage.addRelation dateNode collectionDateNode (ProposedRelation.Exposure <| Exposure.ExposureRelation.TimeEstimate (OldDate.OldDateSimple.HistoryYearAD ((float vm.CollectionDate) * 1.<OldDate.AD>))) g )
                        |> Result.bind(fun g -> Storage.addRelation timelineNode dateNode (ProposedRelation.Exposure Exposure.ExposureRelation.ConstructedWithDate) g )
                        |> Result.bind(fun g -> Storage.addRelation timelineNode contextNode (ProposedRelation.Exposure Exposure.ExposureRelation.IsLocatedAt) g )
                        |> Result.bind(fun g -> 
                            Storage.addProxiedTaxon
                                proxyTaxon
                                existingTaxon
                                []
                                BioticProxies.InferenceMethodNode.Implicit
                                g
                        )
                        |> Result.bind(fun (g, proxiedKey) -> 
                            Storage.addRelationByKey g (timelineNode |> fst |> fst) proxiedKey (ProposedRelation.Exposure Exposure.ExposureRelation.HasProxyInfo)
                            |> Result.lift(fun r -> r, proxiedKey))
                        |> Result.bind(fun (g, proxiedKey) -> 
                            Storage.addRelationByKey g proxiedKey (measureNode |> fst |> fst) (ProposedRelation.Population <| Population.PopulationRelation.MeasuredBy) )
                    )
                return! Ok newGraph
            }
