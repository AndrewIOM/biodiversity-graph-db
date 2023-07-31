namespace BiodiversityCoder.Core

open Elmish
open Bolero
open Bolero.Html
open FieldDataTypes

module NodeSelection =

    let private removeUnit (x:float<_>) = float x

    let holoceneCalYear value =
        let v = 
            match value with
            | FieldDataTypes.OldDate.OldDateSimple.CalYrBP (f, _) -> removeUnit f
            | FieldDataTypes.OldDate.OldDateSimple.HistoryYearAD f -> 1950. - removeUnit f
            | FieldDataTypes.OldDate.OldDateSimple.HistoryYearBC f -> removeUnit f + 1950.
            | FieldDataTypes.OldDate.OldDateSimple.BP f -> removeUnit f
        System.Math.Round v |> int


    /// Lookup time to find if there is a calendar year node corresponding to that year in the
    /// graph database.
    let trySelectTimeNode (graph:Storage.FileBasedGraph<GraphStructure.Node,GraphStructure.Relation>) (value:FieldDataTypes.OldDate.OldDateSimple) =
        let nearestYear = holoceneCalYear value
        let key = Graph.UniqueKey.FriendlyKey("calyearnode",sprintf "%iybp" nearestYear)
        graph.Nodes<Exposure.TemporalIndex.CalYearNode>()
        |> Option.bind(fun n -> if Map.containsKey key n then Some (key, value) else None)

    let trySelectTimeNodeByYear (graph:Storage.FileBasedGraph<GraphStructure.Node,GraphStructure.Relation>) nearestYear =
        let key = Graph.UniqueKey.FriendlyKey("calyearnode",sprintf "%iybp" nearestYear)
        graph.Nodes<Exposure.TemporalIndex.CalYearNode>()
        |> Option.bind(fun n -> if Map.containsKey key n then Some key else None)

    /// Selects either a year in the Holocene, or a pre-Holocene qualitative label.
    /// Third in triple indicates if date is 'out of scope'.
    let trySelectTime (graph:Storage.FileBasedGraph<GraphStructure.Node,GraphStructure.Relation>) (value:FieldDataTypes.OldDate.OldDateSimple) =
        let nearestYear = holoceneCalYear value
        if nearestYear <= 11650 then trySelectTimeNode graph value |> Option.map(fun (a,b) -> (a,b,false))
        else Some (Graph.FriendlyKey("qualitativelabeloutofscopenode", "pre-holocene_by_global stratotype section and point"), value, true)

/// Scenarios represent common work patterns of nodes and relations, which
/// may be partly automated through the automation function.
module Scenarios =

    /// A master DU representing all possible scenarios configured
    /// in the system.
    type Scenario =
        | WoodRing of WoodRingScenario
        | SiteOnlyEntry of SiteOnlyScenario

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

    and SiteOnlyScenario = {
        // Space
        [<Name("Site name")>]
        SiteName: Text.ShortText
        [<Help("Enter the location for the specific timeline in the granularity specified in the text. Locations may be specified as a spatial point ('Site') or polygon ('Area'), or as political units. If using a point ('site'), you may enter coordinates in decimal degrees (DD) using 'Site' or in DMS (format: 40°26'46\"N,79°58'56\"W) using 'SiteDMS'. If using political units, attempt to ensure that the unit conforms to those in GADM https://www.gadm.org/maps.html. If entering a polygon, enter your polygon in WKT format (e.g. POLYGON((26.41 41.79,43.11 41.79,43.11 32.87,26.41 32.87,26.41 41.79)). You can make WKT format polygons on this web page: http://arthur-e.github.io/Wicket/sandbox-gmaps3.html")>]
        SamplingLocation: Geography.SamplingLocation
        [<Help("The parent material from which the 'Outcome' (biodiversity measure) has been measured. Example: pollen from a midden is subfossil.")>]
        SampleOrigin: Population.Context.SampleOrigin
        [<Help("An optional free-form description of the characteristics of the location of the time-series. Example: lake surrounded by Salix and Betula tall shrubs.")>]
        SampleLocationDescription: Text.Text option
        // Time
        [<Name("Earliest year")>]
        [<Help("If a qualitative time period is used, please use the standard form.")>]
        EarliestYear: OldDate.OldDateSimple
        [<Name("Earliest year uncertainty")>]
        EarliestYearUncertainty: OldDate.MeasurementError
        [<Name("Latest year")>]
        LatestYear: OldDate.OldDateSimple
        [<Name("Latest year uncertainty")>]
        LatestYearUncertainty: OldDate.MeasurementError
        [<Name("Timeline characteristics")>]
        Timeline: Exposure.StudyTimeline.IndividualTimelineNode
        // Taxon
        ProxyCategories: Population.BioticProxies.BioticProxyCategoryNode list

    } with 
        static member Title = "Entry for single timeline"
        static member Description = "Leaves the following details for adding later: individual taxon records, individual dates."

    let scenarioGen<'a> model dispatch =
        ViewGen.makeNodeForm<'a> model [] dispatch

    module Automators =

        open GraphStructure
        open Exposure.StudyTimeline
        open Population

        let private removeUnit (x:float<_>) = float x |> int

        let notEmpty l =
            match List.length l with
            | 0 -> Error "Required list is empty"
            | _ -> Ok l

        let automateSimpleSite (vm:SiteOnlyScenario) sourceNode graph =

            let contextNode = PopulationNode <| ContextNode {
                Name = vm.SiteName
                SamplingLocation = vm.SamplingLocation
                SampleOrigin = vm.SampleOrigin
                SampleLocationDescription = vm.SampleLocationDescription
            }

            result {
                let! proxyTypes = notEmpty vm.ProxyCategories

                // Find existing nodes to relate to:
                let! startDateNode, startDateRel, (startDateActual: OldDate.OldDateSimple) = 
                    NodeSelection.trySelectTime graph vm.EarliestYear
                    |> Option.bind(fun (k: Graph.UniqueKey,v,outOfScope) -> 
                        Storage.atomByKey k graph
                        |> Option.map(fun n ->
                            if outOfScope then n, Exposure.ExposureRelation.ExtentEarliestOutOfScope v, v else n, Exposure.ExposureRelation.ExtentEarliest, v))
                    |> Result.ofOption "Earliest year could not be connected to the internal time series."
                let! endDateNode, endDateActual = 
                    NodeSelection.trySelectTimeNode graph vm.LatestYear
                    |> Option.bind(fun (k,v) -> Storage.atomByKey k graph |> Option.map(fun i -> i,v))
                    |> Result.ofOption "Earliest year could not be connected to the internal time series."
                let proxyNodes = proxyTypes  |> List.map(BioticProxyCategoryNode >> PopulationNode)

                // Uncertainty timeline nodes
                let addDateUncertainties timelineNode g =
                    match vm.EarliestYearUncertainty with
                    | OldDate.MeasurementError.NoDatingErrorSpecified -> Ok g
                    | OldDate.MeasurementError.DatingErrorPlusMinus error ->
                        let earlyEarly = NodeSelection.holoceneCalYear startDateActual + removeUnit error |> NodeSelection.trySelectTimeNodeByYear g
                        let earlyLate = NodeSelection.holoceneCalYear startDateActual - removeUnit error |> NodeSelection.trySelectTimeNodeByYear g
                        if earlyEarly.IsNone || earlyLate.IsNone then Error "Could not assign the uncertainty value for the earliest year."
                        else
                            Storage.addRelationByKey g (timelineNode |> fst |> fst) earlyEarly.Value (ProposedRelation.Exposure Exposure.ExposureRelation.ExtentEarliestUncertainty)
                            |> Result.bind(fun g -> Storage.addRelationByKey g (timelineNode |> fst |> fst) earlyLate.Value (ProposedRelation.Exposure Exposure.ExposureRelation.ExtentEarliestUncertainty))
                let addDateUncertaintiesLate timelineNode g =
                    match vm.LatestYearUncertainty with
                    | OldDate.MeasurementError.NoDatingErrorSpecified -> Ok g
                    | OldDate.MeasurementError.DatingErrorPlusMinus error ->
                        let lateEarly = NodeSelection.holoceneCalYear endDateActual + removeUnit error |> NodeSelection.trySelectTimeNodeByYear g
                        let lateLate = NodeSelection.holoceneCalYear endDateActual - removeUnit error |> NodeSelection.trySelectTimeNodeByYear g
                        if lateEarly.IsNone || lateLate.IsNone then Error "Could not assign the uncertainty value for the earliest year."
                        else
                            Storage.addRelationByKey g (timelineNode |> fst |> fst) lateEarly.Value (ProposedRelation.Exposure Exposure.ExposureRelation.ExtentLatestUncertainty)
                            |> Result.bind(fun g -> Storage.addRelationByKey g (timelineNode |> fst |> fst) lateLate.Value (ProposedRelation.Exposure Exposure.ExposureRelation.ExtentLatestUncertainty))

                // Add new validated structure into graph:
                let! newGraph = 
                    graph
                    |> fun g -> Storage.addNodes g [ 
                        vm.Timeline |> Exposure.ExposureNode.TimelineNode |> ExposureNode
                        contextNode ]
                    |> Result.bind(fun (g, addedNodes) -> 
                        let timelineNode = addedNodes |> Seq.find(fun s -> (s |> fst |> snd).NodeType() = "IndividualTimelineNode")
                        let contextNode = addedNodes |> Seq.find(fun s -> (s |> fst |> snd).NodeType() = "ContextNode")
                        Storage.addRelation sourceNode timelineNode (ProposedRelation.Source Sources.SourceRelation.HasTemporalExtent) g
                        |> Result.bind(fun g -> Storage.addRelation timelineNode startDateNode (ProposedRelation.Exposure startDateRel) g )
                        |> Result.bind(fun g -> Storage.addRelation timelineNode endDateNode (ProposedRelation.Exposure Exposure.ExposureRelation.ExtentLatest) g )
                        |> Result.bind(fun g -> addDateUncertainties timelineNode g)
                        |> Result.bind(fun g -> addDateUncertaintiesLate timelineNode g)
                        |> Result.bind(fun g -> Storage.addRelation timelineNode contextNode (ProposedRelation.Exposure Exposure.ExposureRelation.IsLocatedAt) g )
                        |> Result.bind(fun g -> Storage.addOrSkipNodes g proxyNodes)
                        |> Result.bind(fun (g,_) ->
                            List.fold(fun state proxy ->
                                state |> Result.bind(fun gr ->
                                    Storage.addRelationByKey gr (timelineNode |> fst |> fst) (makeUniqueKey proxy) (ProposedRelation.Exposure Exposure.ExposureRelation.HasProxyCategory)
                                )) (Ok g) proxyNodes
                        )
                    )
                
                return! Ok newGraph
            }

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
