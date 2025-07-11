namespace BiodiversityCoder.Core

open Elmish
open Bolero
open Bolero.Html

module GraphVisualisation =

    open Cyjs.NET
    open Cyjs.NET.Elements

    let nodes (graph:Graph.Graph<GraphStructure.Node,GraphStructure.Relation>) =
        graph |> Seq.map(fun atom ->
            let label = (atom |> fst |> snd).ToString()
            node ((atom |> fst |> snd).DisplayName()) [ CyParam.label label ] )

    let relations (graph:Graph.Graph<GraphStructure.Node,GraphStructure.Relation>) =
        graph |> Seq.collect(fun atom ->
            atom |> snd |> Seq.map(fun (source,sink,_,_) -> source, sink))
        |> Seq.mapi(fun i (source,sink) -> edge (string i) (string source) (string sink) [])

    let view graph =
        CyGraph.initEmpty ()
        |> CyGraph.withElements (nodes graph)
        |> CyGraph.withElements (relations graph)
        |> CyGraph.withStyle "node"     
                [
                    CyParam.content =. CyParam.label
                    CyParam.color "#A00975"
                ]
        |> CyGraph.withSize(800, 400)
        |> HTML.toCytoHTML


module App =

    open System.Threading.Tasks

    let sections = 
        [
            "source-primary-or-secondary", "Source: Primary of Secondary?"
            "exposure", "Timelines their spatial contexts (Exposure and Population-Context)"
            "outcome", "Biodiversity outcomes (Population and Outcomes)"
        ] |> Map.ofList

    type Page =
        | Extract
        | AppendData
        | Sources
        | Population
        | Exposure
        | Outcome
        | Scenario of ScenarioPage
        | Statistics
        | Settings

    and ScenarioPage =
        | WoodRing
        | SimpleEntry

    type Model =
        {
            Page: Page
            Graph: Storage.FileBasedGraph<GraphStructure.Node,GraphStructure.Relation> option
            Statistics: Statistics option
            Import: string
            Message: (MessageType * string) option
            FolderLocation: string
            NodeCreationViewModels: Map<string, NodeViewModel>
            NodeCreationValidationErrors: Map<string, (string * string) list>
            NodeCreationRelations: Map<string, string * Map<string * GraphStructure.ProposedRelation, Graph.UniqueKey list>> // type, selected toggle * Map<relation, node IDs>
            RelationCreationViewModels: Map<string * string, Map<string,NodeViewModel * GraphStructure.ProposedRelation option>> // proposed is the computed relation. With this, can attempt to link to existing node.
            SelectedSource: SelectedSource option
            TaxonLookup: TaxonomicLookupModel
            LeaveFieldsFilled: bool
            HideExcludedSources: bool
            SourceFilter: string
            CrossRefLookupModel: CrossRefLookupModel
        }

    and MessageType =
        | ErrorMessage
        | SuccessMessage
        | InfoMessage

    and Statistics = {
        TimeGenerated: System.DateTime
        IncludedSources: int
        ExcludedSources: int
        CompleteSources: int
        SecondarySources: int
        PrimarySources: int
        ScreenedNotCoded: int
    }

    and TaxonomicLookupModel = {
        Rank: string
        Family: string
        Genus: string
        Species: string
        Authorship: string
        Result: (Population.Taxonomy.TaxonNode * list<Population.PopulationNodeRelation>) option
    }
    
    and CrossRefLookupModel = {
        SearchTerm: string
        Match: Sources.Source option
    }

    and SelectedSource = {
        MarkedPrimary: IsPrimarySource
        AddingNewSource: bool
        SelectedSource: Graph.Atom<GraphStructure.Node,GraphStructure.Relation>
        ProposedLink: Graph.UniqueKey option
        ProposedDatabaseLink: ProposedDatabaseLink option
        LinksToPrimarySources: (Graph.UniqueKey * string) option
        Screening: NodeViewModel
        AddBioticHyperedge: Map<Graph.UniqueKey, Graph.UniqueKey option * Graph.UniqueKey option * (Graph.UniqueKey list) option * Graph.UniqueKey option>
        AddBioticHyperedgeBatch: Map<Graph.UniqueKey, BioticHyperedgeBatch>
        ProblematicSection: string
        ProblematicSectionReason: string
        SelectedTimeline: Graph.UniqueKey option
    }

    and BioticHyperedgeBatch = {
        Group: Population.BioticProxies.MicrofossilGroup
        InferenceMethod: Graph.UniqueKey option
        Outcome: Graph.UniqueKey option
        UsePlaceholderTaxon: bool
        UsePlaceholderInfer: bool
        LinkedTaxa: Map<int, (Graph.UniqueKey * Graph.UniqueKey * (Graph.UniqueKey list) * Graph.UniqueKey)>
        UnlinkedTaxa: Map<int,string * string>
    }

    and ProposedDatabaseLink = {
        Id: Graph.UniqueKey option
        AccessDate: string
        AccessMethod: string
        AccessDetails: string
    }

    and IsPrimarySource = Primary | Secondary | Unknown
    
    and EligbilityCriteria =
        | Include
        | Exclude of because:Sources.ExclusionReason * notes:FieldDataTypes.Text.Text

    let initModel =
        { RelationCreationViewModels = Map.empty; SourceFilter = ""; HideExcludedSources = true; LeaveFieldsFilled = false; Statistics = None; FolderLocation = ""; TaxonLookup = { Rank = "Genus"; Family = ""; Genus = ""; Species = ""; Authorship = ""; Result = None }; NodeCreationRelations = Map.empty; SelectedSource = None; Graph = None; Import = ""; Message = None; Page = Extract; NodeCreationViewModels = Map.empty; NodeCreationValidationErrors = Map.empty
          CrossRefLookupModel = { SearchTerm = ""; Match = None } }, Cmd.none

    type Message =
        | SetPage of Page
        | DismissMessage
        | SelectFolder
        | SetFolderManually of string
        | SelectedFolder of string
        | ResetApplication
        | ChangeImportText of string
        | ImportBibtex
        | ImportColandr
        | GenStatistics
        | ToggleLeaveFilled
        | ToggleHideExcluded
        | FilterSourceList of string
        | FormMessage of FormMessage
        | SelectSource of key:Graph.UniqueKey
        | SelectTimeline of key: Graph.UniqueKey
        | LookupTaxon of LookupTaxonMessage
        | ChangeProxiedTaxonVm of timeline:Graph.UniqueKey * proxy:Graph.UniqueKey option * inference:Graph.UniqueKey option * taxon:(Graph.UniqueKey list) option * measure:Graph.UniqueKey option
        | SubmitProxiedTaxon of timeline:Graph.UniqueKey
        | UseBatchModeForProxiedTaxon of timeline: Graph.UniqueKey * bool
        | SetBatchModeAutofill of timeline:Graph.UniqueKey * Population.BioticProxies.MicrofossilGroup * Graph.UniqueKey option * Graph.UniqueKey option * bool * bool
        | ChangeBatchUnverifiedProxiedTaxon of timeline:Graph.UniqueKey * int * string * string
        | ChangeBatchVerifiedTaxon of timeline:Graph.UniqueKey * int * (Graph.UniqueKey * Graph.UniqueKey * (Graph.UniqueKey list) * Graph.UniqueKey)
        | RemoveBatchVerifiedTaxon of timeline:Graph.UniqueKey * int
        | RemoveBatchUnverifiedTaxon of timeline:Graph.UniqueKey * int
        | ValidateOrConfirmBatch of timeline:Graph.UniqueKey
        | MarkPrimary of IsPrimarySource
        | ToggleConnectNewOrExistingSource
        | ChangeProposedSourceLink of Graph.UniqueKey option
        | ChangeProposedSourceDatabaseLink of ProposedDatabaseLink option
        | CompleteSection of string
        | ChangeCodingProblem of string * string
        | SubmitCodingProblem
        | EnterCrossRefSearch of string
        | SearchCrossRef of string
        | ClearCrossRef
        | AddCrossRefMatch

    and LookupTaxonMessage =
        | ChangeFormFields of TaxonomicLookupModel
        | RunLookup
        | SaveTaxonResult

    let isBatchMode timeline cont (model:Model) =
        match model.SelectedSource with
        | None -> { model with Message = Some (ErrorMessage, "Source was not selected.") }, Cmd.none
        | Some source ->
            match source.AddBioticHyperedgeBatch |> Map.tryFind timeline with
            | None -> { model with Message = Some (ErrorMessage, "Not in batch mode.") }, Cmd.none
            | Some batch -> cont (source,batch)

    let commitProxiedTaxon (proxyId: Graph.UniqueKey) (inferId:Graph.UniqueKey) (taxaIds:Graph.UniqueKey list) (measureId:Graph.UniqueKey) timelineId g = result {
        // Unique keys to nodes
        let! proxy = 
            Storage.atomByKey proxyId g 
            |> Result.ofOption "Could not find proxy node"
            |> Result.bind(fun ((i,n),adj) ->
                match n with
                | GraphStructure.Node.PopulationNode p ->
                    match p with
                    | GraphStructure.BioticProxyNode x -> Ok x
                    | _ -> Error "Not a biotic proxy node"
                | _ -> Error "Not a biotic proxy node" )
        let! infer = 
            Storage.atomByKey inferId g 
            |> Result.ofOption "Could not find inference node"
            |> Result.bind(fun ((i,n),adj) ->
                match n with
                | GraphStructure.Node.PopulationNode p ->
                    match p with
                    | GraphStructure.InferenceMethodNode x -> Ok x
                    | _ -> Error "Not an inference method node"
                | _ -> Error "Not an inference method node" )
        let! taxa = 
            if taxaIds.Length = 0 then Error "Must specify at least one taxon"
            else
                taxaIds |> List.map(fun taxonId ->
                    Storage.atomByKey taxonId g 
                    |> Result.ofOption "Could not find taxon node"
                    |> Result.bind(fun ((i,n),adj) ->
                        match n with
                        | GraphStructure.Node.PopulationNode p ->
                            match p with
                            | GraphStructure.TaxonomyNode x -> Ok x
                            | _ -> Error "Not a taxonomy node"
                        | _ -> Error "Not a taxonomy node" )
                ) |> Result.ofList

        let! updatedGraph, hyperEdgeId = Storage.addProxiedTaxon proxy taxa.Head taxa.Tail infer [] g
        
        // Save (1) outcome measure from hyperedge to outcome node, and (2) relation from timeline to hyperedge.
        let! timeline = Storage.atomByKey timelineId updatedGraph |> Result.ofOption "Could not find timeline"
        let! hyperedge = Storage.atomByKey hyperEdgeId updatedGraph |> Result.ofOption "Could not find new hyperedge"
        let! outcomeNode = Storage.atomByKey measureId updatedGraph |> Result.ofOption "Could not find new hyperedge"
        let! updatedGraphWithRelations = 
            Storage.addRelation hyperedge outcomeNode (GraphStructure.ProposedRelation.Population Population.PopulationRelation.MeasuredBy) updatedGraph
            |> Result.bind (Storage.addRelation timeline hyperedge (GraphStructure.ProposedRelation.Exposure Exposure.ExposureRelation.HasProxyInfo))
        return updatedGraphWithRelations
    }


    let update (openFolder:System.Threading.CancellationToken -> Task<string>) message model =
        match message with
        | SetPage page -> { model with Page = page }, Cmd.none
        | DismissMessage -> { model with Message = None }, Cmd.none
        | ToggleLeaveFilled -> { model with LeaveFieldsFilled = not model.LeaveFieldsFilled }, Cmd.none
        | ToggleHideExcluded -> { model with HideExcludedSources = not model.HideExcludedSources }, Cmd.none
        | ChangeImportText s -> { model with Import = s }, Cmd.none
        | SetFolderManually s -> { model with FolderLocation = s }, Cmd.none
        | FilterSourceList s -> { model with SourceFilter = s }, Cmd.none
        | ImportBibtex -> 
            match Sources.BibtexParser.parse model.Import with
            | Error e -> { model with Message = Some (ErrorMessage, e) }, Cmd.none
            | Ok nodes ->
                let nodes = nodes |> List.map (Sources.SourceNode.Unscreened >> GraphStructure.Node.SourceNode)
                match model.Graph with
                | Some g -> 
                    match Storage.addNodes g nodes with
                    | Ok g -> { model with Graph = Some (fst g) }, Cmd.none
                    | Error e -> { model with Message = Some (ErrorMessage, e) }, Cmd.none
                | None -> model, Cmd.none
        | ImportColandr ->
            match model.Graph with
            | Some g ->
                try
                    match Sources.ColandrParser.syncColandr (System.IO.Path.Combine(g.Directory,"colandr-titleabs-screen-results.csv")) with
                    | Error e -> { model with Message = Some (ErrorMessage, e) }, Cmd.none
                    | Ok nodes ->
                        let nodes = nodes |> Seq.map (Sources.SourceNode.Unscreened >> GraphStructure.Node.SourceNode) |> Seq.toList
                        match Storage.addOrSkipNodes g nodes with
                        | Ok g -> { model with Graph = Some (fst g) }, Cmd.none
                        | Error e -> { model with Message = Some (ErrorMessage, e) }, Cmd.none
                with e -> { model with Message = Some (ErrorMessage, e.Message) }, Cmd.none
            | None -> { model with Message = Some (ErrorMessage, "There was no graph") }, Cmd.none
        | SelectSource k ->
            match model.Graph with
            | None -> { model with Message = Some (ErrorMessage, "Can't select a source when no graph is loaded.") }, Cmd.none
            | Some g ->
                match g |> Storage.atomByKey k with
                | Some atom -> 
                    match model.SelectedSource with
                    | Some alreadySelected ->
                        // If re-loading the same source, don't bin all of the form entry in progress
                        if alreadySelected.SelectedSource |> fst |> fst = k
                        then { model with SelectedSource = Some { SelectedTimeline = None; AddBioticHyperedgeBatch = alreadySelected.AddBioticHyperedgeBatch; ProposedDatabaseLink = None; ProblematicSection = ""; ProblematicSectionReason = ""; AddingNewSource = false; MarkedPrimary = Unknown; ProposedLink = None; AddBioticHyperedge = Map.empty; SelectedSource = atom; LinksToPrimarySources = None; Screening = NotEnteredYet } }, Cmd.none
                        else { model with SelectedSource = Some { SelectedTimeline = None; AddBioticHyperedgeBatch = Map.empty; ProposedDatabaseLink = None; ProblematicSection = ""; ProblematicSectionReason = ""; AddingNewSource = false; MarkedPrimary = Unknown; ProposedLink = None; AddBioticHyperedge = Map.empty; SelectedSource = atom; LinksToPrimarySources = None; Screening = NotEnteredYet } }, Cmd.none
                    | None -> { model with SelectedSource = Some { SelectedTimeline = None; AddBioticHyperedgeBatch = Map.empty; ProposedDatabaseLink = None; ProblematicSection = ""; ProblematicSectionReason = ""; AddingNewSource = false; MarkedPrimary = Unknown; ProposedLink = None; AddBioticHyperedge = Map.empty; SelectedSource = atom; LinksToPrimarySources = None; Screening = NotEnteredYet } }, Cmd.none
                | None -> { model with Message = Some (ErrorMessage, sprintf "Could not find source with key %s [%A]" k.AsString k) }, Cmd.none
        | SelectTimeline k ->
            match model.Graph with
            | None -> { model with Message = Some (ErrorMessage, "Can't select a timeline when no graph is loaded.") }, Cmd.none
            | Some g ->
                match model.SelectedSource with
                | None -> { model with Message = Some (ErrorMessage, "No source is selected.") }, Cmd.none
                | Some source -> 
                    match g |> Storage.atomByKey k with
                    | Some _ -> { model with SelectedSource = Some { source with SelectedTimeline = Some k }}, Cmd.none
                    | None -> { model with Message = Some (ErrorMessage, "Could not find timeline node in database.") }, Cmd.none
        | SelectFolder ->
            let tokenSource = new System.Threading.CancellationTokenSource()
            model, Cmd.OfAsync.result(async {
                let! folder = openFolder tokenSource.Token |> Async.AwaitTask
                return SelectedFolder folder
            })
        | SelectedFolder folder ->
            match Storage.loadOrInitGraph folder with
            | Ok g ->  
                match (g.Nodes<Exposure.TemporalIndex.CalYearNode> ()) with
                | Some _ -> { model with Graph = Some g }, Cmd.ofMsg GenStatistics
                | None ->
                    match Storage.seedGraph g with
                    | Ok seeded -> { model with Graph = Some seeded }, Cmd.none
                    | Error e -> { model with Message = Some (ErrorMessage, e) }, Cmd.none
            | Error e -> { model with Message = Some (ErrorMessage, e) }, Cmd.none
        | ResetApplication -> { fst initModel with Message = Some (InfoMessage, "Reset the application and reloaded database") }, Cmd.ofMsg (SelectedFolder model.FolderLocation)
        | GenStatistics ->
            match model.Graph with
            | Some g ->
                let stats = 
                    g.Nodes<Sources.SourceNode> ()
                    |> Option.map(fun nodes ->
                        Seq.map(fun (kv: System.Collections.Generic.KeyValuePair<Graph.UniqueKey,string>) -> kv.Key) nodes
                        |> Seq.toList
                        |> Storage.loadAtoms g.Directory (typeof<Sources.SourceNode>).Name
                        |> Result.lift(fun sources ->
                            sources |> List.fold(fun stats (s: Graph.Atom<GraphStructure.Node,GraphStructure.Relation>) ->
                                match s |> fst |> snd with
                                | GraphStructure.Node.SourceNode s2 ->
                                    match s2 with
                                    | Sources.SourceNode.Excluded _ -> { stats with ExcludedSources = stats.ExcludedSources + 1 }
                                    | Sources.SourceNode.Included (i: Sources.Source,prog) ->
                                        match i with
                                        | Sources.Bibliographic bib ->
                                            let isPrimary = 
                                                s |> snd |> Seq.tryFind(fun (_,_,_,d: GraphStructure.Relation) -> 
                                                    match d with 
                                                    | GraphStructure.Relation.Source s -> 
                                                        match s with
                                                        | Sources.SourceRelation.HasTemporalExtent -> true
                                                        | _ -> false
                                                    | _ -> false ) |> Option.isSome
                                            match prog with
                                            | Sources.CodingProgress.CompletedNone -> { stats with IncludedSources = stats.IncludedSources + 1; ScreenedNotCoded = stats.ScreenedNotCoded + 1; PrimarySources = (if isPrimary then stats.PrimarySources + 1 else stats.PrimarySources) }
                                            | Sources.CodingProgress.InProgress _
                                            | Sources.CodingProgress.Stalled _ -> { stats with IncludedSources = stats.IncludedSources + 1; PrimarySources = (if isPrimary then stats.PrimarySources + 1 else stats.PrimarySources)  }
                                            | Sources.CodingProgress.CompletedAll -> 
                                                { stats with CompleteSources = stats.CompleteSources + 1; IncludedSources = stats.IncludedSources + 1; SecondarySources = (if not isPrimary then stats.SecondarySources + 1 else stats.SecondarySources); PrimarySources = (if isPrimary then stats.PrimarySources + 1 else stats.PrimarySources)  }
                                        | _ -> stats
                                    | _ -> stats
                                | _ -> stats
                            ) { TimeGenerated = System.DateTime.Now; IncludedSources = 0; CompleteSources = 0; ExcludedSources = 0; SecondarySources = 0; PrimarySources = 0; ScreenedNotCoded = 0 }
                        )
                        |> Result.toOption
                    ) |> Option.flatten
                { model with Statistics = stats }, Cmd.none
            | None -> { model with Message = Some (ErrorMessage, "No graph loaded") }, Cmd.none
        
        | FormMessage m ->
            match m with
            | RelateNodes(source, sink, rel) -> 
                match model.Graph with
                | Some g -> 
                    result {
                        let! sourceNode = g |> Storage.atomByKey source |> Result.ofOption "Could not find source node"
                        let! sinkNode = g |> Storage.atomByKey sink |> Result.ofOption "Could not find sink node"
                        let! updatedGraph = g |> Storage.addRelation sourceNode sinkNode rel
                        return updatedGraph
                    } |> Result.lower
                        (fun g -> { model with Graph = Some g }, Cmd.none )
                            (fun e -> { model with Message = Some (ErrorMessage, e) }, Cmd.none)

                | None -> { model with Message = Some (ErrorMessage, "No graph loaded") }, Cmd.none
            | EnterNodeCreationData(formId, vm) -> 
                match formId with
                | "EligbilityCriteria" ->
                    match model.SelectedSource with
                    | Some source -> { model with SelectedSource = Some { source with Screening = Merge.updateNodeViewModel source.Screening vm }}, Cmd.none
                    | None -> model, Cmd.none
                | _ ->
                    let updatedVm = 
                        match model.NodeCreationViewModels |> Map.tryFind formId with
                        | Some formData -> Merge.updateNodeViewModel formData vm
                        | None -> Merge.updateNodeViewModel NotEnteredYet vm
                    { model with NodeCreationViewModels = model.NodeCreationViewModels |> Map.add formId updatedVm }, Cmd.none

            | AddOrUpdateNode (nodeType, validateRelations, requiredRelations) -> 
                if typeof<EligbilityCriteria> = nodeType
                then
                    match model.Graph with
                    | Some g ->
                        // Handle special case: eligibility criteria.
                        match model.SelectedSource with
                        | Some source ->
                            // Update the source node and save it.
                            let updateNode (node:obj) = 
                                match source.SelectedSource |> fst |> snd with
                                | GraphStructure.Node.SourceNode s ->
                                    match s with
                                    | Sources.SourceNode.Excluded (s,_,_)
                                    | Sources.SourceNode.Included (s,_)
                                    | Sources.SourceNode.Unscreened s ->
                                        match (node :?> EligbilityCriteria) with
                                        | Include -> Sources.SourceNode.Included (s,Sources.CompletedNone) |> Ok
                                        | Exclude (because, notes) -> Sources.SourceNode.Excluded (s, because, notes) |> Ok
                                | _ -> Error "No source was selected"
                            Create.createFromViewModel nodeType source.Screening
                            |> Result.bind updateNode
                            |> Result.lift GraphStructure.Node.SourceNode
                            |> Result.bind (fun n -> Storage.updateNode g (source.SelectedSource |> fst |> fst, n))
                            |> Result.lift (fun g -> { model with Graph = Some g }, Cmd.ofMsg (SelectSource (source.SelectedSource |> fst |> fst)))
                            |> Result.lower (fun r -> r) (fun e -> { model with Message = Some (ErrorMessage, e) }, Cmd.none)
                        | None -> { model with Message = Some (ErrorMessage, "Cannot screen source as graph is not loaded.") }, Cmd.none
                    | None -> model, Cmd.none
                // TODO remove hardcoding of scenarios -> custom functions
                else if typeof<Scenarios.WoodRingScenario> = nodeType 
                then 
                    match model.Graph with
                    | Some g ->
                        match model.SelectedSource with
                        | Some source ->
                            match model.NodeCreationViewModels |> Map.tryFind nodeType.Name with
                            | Some (formData: NodeViewModel) -> 
                                Create.createFromViewModel nodeType formData
                                |> Result.lift (fun n -> n :?> Scenarios.WoodRingScenario)
                                |> Result.bind(fun vm -> 
                                    Scenarios.Automators.automateTreeRing vm source.SelectedSource g)
                                |> Result.lift (fun g -> { model with Message = Some (SuccessMessage, "Successfully saved tree-ring scenario information"); Graph = Some g; NodeCreationViewModels = (if model.LeaveFieldsFilled then model.NodeCreationViewModels else model.NodeCreationViewModels |> Map.remove nodeType.Name); NodeCreationRelations = model.NodeCreationRelations |> Map.remove nodeType.Name }, Cmd.ofMsg (SelectSource (source.SelectedSource |> fst |> fst)))
                                |> Result.lower (fun r -> r) (fun e -> { model with Message = Some (ErrorMessage, e) }, Cmd.none)
                            | None -> { model with Message = Some (ErrorMessage, "You haven't entered any data yet") }, Cmd.none
                        | None -> { model with Message = Some (ErrorMessage, "No source selected") }, Cmd.none
                    | None -> { model with Message = Some (ErrorMessage, "No graph loaded") }, Cmd.none
                else if typeof<Scenarios.SiteOnlyScenario> = nodeType 
                then 
                    match model.Graph with
                    | Some g ->
                        match model.SelectedSource with
                        | Some source ->
                            match model.NodeCreationViewModels |> Map.tryFind nodeType.Name with
                            | Some (formData: NodeViewModel) -> 
                                Create.createFromViewModel nodeType formData
                                |> Result.lift (fun n -> n :?> Scenarios.SiteOnlyScenario)
                                |> Result.bind(fun vm -> 
                                    Scenarios.Automators.automateSimpleSite vm source.SelectedSource g)
                                |> Result.lift (fun g -> { model with Message = Some (SuccessMessage, "Successfully saved timeline information"); Graph = Some g; NodeCreationViewModels = (if model.LeaveFieldsFilled then model.NodeCreationViewModels else model.NodeCreationViewModels |> Map.remove nodeType.Name); NodeCreationRelations = model.NodeCreationRelations |> Map.remove nodeType.Name }, Cmd.ofMsg (SelectSource (source.SelectedSource |> fst |> fst)))
                                |> Result.lower (fun r -> r) (fun e -> { model with Message = Some (ErrorMessage, e) }, Cmd.none)
                            | None -> { model with Message = Some (ErrorMessage, "You haven't entered any data yet") }, Cmd.none
                        | None -> { model with Message = Some (ErrorMessage, "No source selected") }, Cmd.none
                    | None -> { model with Message = Some (ErrorMessage, "No graph loaded") }, Cmd.none
                else
                    match model.NodeCreationViewModels |> Map.tryFind nodeType.Name with
                    | Some (formData: NodeViewModel) -> 
                        let node = 
                            try Create.createFromViewModel nodeType formData with
                            | exn -> Error <| sprintf "Internal error when making a node: %s %s" exn.Message exn.StackTrace
                        match node with
                        | Error e -> { model with Message = Some (ErrorMessage, e) }, Cmd.none
                        | Ok n -> 
                            match model.Graph with
                            | Some g ->

                                let checkRelations =
                                    let check =
                                        model.NodeCreationRelations 
                                        |> Map.tryFind nodeType.Name 
                                        |> Option.map(fun (_,rels) -> rels |> Map.map(fun k v -> v |> Seq.map(fun _ -> snd k)) |> Map.toSeq |> Seq.collect snd)
                                        |> Option.map validateRelations
                                    match check with
                                    | Some b -> if b then Ok () else Error "You have not entered all required details (relations)"
                                    | None -> Ok ()

                                // Is a normal new graph node:
                                checkRelations
                                |> Result.bind(fun _ -> GraphStructure.Nodes.tryMakeNode nodeType n)
                                |> Result.bind (fun n -> Storage.addNodes g [ n ])
                                |> Result.bind(fun (graph, addedNodes) ->
                                    // Add relations sink nodes were specified in EnterNodeRelationData
                                    let proposedRels =
                                        model.NodeCreationRelations 
                                        |> Map.tryFind nodeType.Name 
                                        |> Option.map(fun (_,rels) ->
                                            rels |> Map.toList |> List.collect(fun (r,sinks) ->
                                                sinks |> List.map(fun s -> ThisIsSource(s,snd r))))
                                        |> (fun o -> match o with | Some o -> o | None -> [])
                                        |> List.append requiredRelations
                                    
                                    // Add relations that are required as specified when creating each form.
                                    Seq.fold(fun fbg (relation:RelationViewModel) -> 
                                        fbg |> Result.bind(fun fbg ->
                                            match relation with
                                            | ThisIsSink (sourceKey, proposed) -> Storage.addRelationByKey fbg sourceKey ((addedNodes.Head |> fst |> fst)) proposed
                                            | ThisIsSource (sinkKey, proposed) -> Storage.addRelationByKey fbg ((addedNodes.Head |> fst |> fst)) sinkKey proposed
                                        )) (Ok graph) proposedRels)
                                |> Result.lift (fun g -> { model with Graph = Some g; Message = (if model.LeaveFieldsFilled then Some (SuccessMessage, "Successfully added node information") else None); NodeCreationViewModels = (if model.LeaveFieldsFilled then model.NodeCreationViewModels else model.NodeCreationViewModels |> Map.remove nodeType.Name); NodeCreationRelations = (if model.LeaveFieldsFilled then model.NodeCreationRelations else model.NodeCreationRelations |> Map.remove nodeType.Name) })
                                |> Result.lower (fun m -> 
                                    match model.SelectedSource with
                                    | Some s -> m, Cmd.ofMsg (SelectSource (s.SelectedSource |> fst |> fst))
                                    | None -> m, Cmd.none) (fun e -> { model with Message = Some (ErrorMessage, e) }, Cmd.none)
                            | None -> { model with Message = Some (ErrorMessage, "Cannot make node as graph is not loaded.") }, Cmd.none
                    | None -> { model with Message = Some (ErrorMessage, sprintf "Could not find type of %s" nodeType.Name) }, Cmd.none
            | EnterNodeRelationData(nodeType, toggleset, proposed, name, sinkKeys) -> 
                let x, y =
                    match model.NodeCreationRelations |> Map.tryFind nodeType with
                    | Some (toggleset, existing) -> 
                        if sinkKeys.IsEmpty 
                        then model.NodeCreationRelations |> Map.add nodeType (toggleset, existing |> Map.remove (name,proposed)), model.RelationCreationViewModels |> Map.remove (nodeType, toggleset)
                        else model.NodeCreationRelations |> Map.add nodeType (toggleset, existing |> Map.add (name,proposed) sinkKeys), model.RelationCreationViewModels
                    | None -> model.NodeCreationRelations |> Map.add nodeType (toggleset, Map.ofList [(name,proposed), sinkKeys]), model.RelationCreationViewModels
                { model with NodeCreationRelations = x; RelationCreationViewModels = y }, Cmd.none
            | EnterRelationCreationData(nodeType, toggle, name, vm, proposed) -> 
                let updatedVm = 
                    match model.RelationCreationViewModels |> Map.tryFind (nodeType,toggle) with
                    | Some existing -> 
                        match existing |> Map.tryFind name with
                        | Some (formData, _) -> existing |> Map.add name (Merge.updateNodeViewModel formData vm, proposed)
                        | None -> existing |> Map.add name (Merge.updateNodeViewModel NotEnteredYet vm, proposed)
                    | None -> Map.empty |> Map.add name (Merge.updateNodeViewModel NotEnteredYet vm, proposed)
                { model with RelationCreationViewModels = model.RelationCreationViewModels |> Map.add (nodeType,toggle) updatedVm }, Cmd.none
            | ChangeNodeRelationToggle(nodeType, toggle) ->
                { model with NodeCreationRelations = model.NodeCreationRelations |> Map.add nodeType (toggle, Map.empty)
                             RelationCreationViewModels = model.RelationCreationViewModels |> Map.filter(fun k _ -> fst k <> nodeType) }, Cmd.none
        | LookupTaxon l ->
            match l with
            | ChangeFormFields f -> 
                match f.Rank with
                | "Family" -> { model with TaxonLookup = { Result = None; Rank = "Family"; Family = f.Family; Genus = ""; Species = ""; Authorship = "" }}, Cmd.none
                | "Genus" -> { model with TaxonLookup = { Result = None; Rank = "Genus"; Family = f.Family; Genus = f.Genus; Species = ""; Authorship = "" }}, Cmd.none
                | "Species" -> { model with TaxonLookup = { Result = None; Rank = "Species"; Family = f.Family; Genus = f.Genus; Species = f.Species; Authorship = f.Authorship }}, Cmd.none
                | _ -> model, Cmd.none
            | RunLookup ->
                let run = TaxonomicBackbone.GlobalPollenProject.lookupAsNodesAndRelations model.TaxonLookup.Rank model.TaxonLookup.Family model.TaxonLookup.Genus model.TaxonLookup.Species model.TaxonLookup.Authorship
                match run with
                | Ok t -> { model with TaxonLookup = { model.TaxonLookup with Result = Some t } }, Cmd.none
                | Error e -> { model with Message = Some (ErrorMessage, e) }, Cmd.none
            | SaveTaxonResult ->
                match model.TaxonLookup.Result with
                | None -> { model with Message = Some (ErrorMessage, "Cannot save taxon, as none was found") }, Cmd.none
                | Some (taxon, relations) ->
                    match model.Graph with
                    | None -> { model with Message = Some (ErrorMessage, "Cannot save taxon, as graph is not loaded") }, Cmd.none
                    | Some g ->
                        let add () =
                            result {
                                let nodes =
                                    relations
                                    |> List.collect(fun r ->
                                        match r with
                                        | Population.PopulationNodeRelation.IsA(source, sink) ->
                                            [ source; sink ]
                                        | _ -> [])
                                    |> List.append [ taxon ]
                                    |> List.distinct
                                    |> List.map(GraphStructure.TaxonomyNode >> GraphStructure.Node.PopulationNode)

                                let! updatedGraph, updatedNodes = Storage.addOrSkipNodes g nodes

                                let relationsByKey =
                                    relations
                                    |> List.choose(fun r ->
                                        match r with
                                        | Population.PopulationNodeRelation.IsA(source, sink) ->
                                            Some (
                                                GraphStructure.ProposedRelation.Population <| Population.PopulationRelation.IsA,
                                                ((updatedNodes |> Seq.find(fun a -> (a |> fst |> fst) = (source |> GraphStructure.TaxonomyNode |> GraphStructure.Node.PopulationNode |> GraphStructure.makeUniqueKey))) |> fun ((a,_),c) -> ((a,source),c)),
                                                ((updatedNodes |> Seq.find(fun a -> (a |> fst |> fst) = (sink |> GraphStructure.TaxonomyNode |> GraphStructure.Node.PopulationNode |> GraphStructure.makeUniqueKey))) |> fun ((a,_),c) -> ((a,sink),c))
                                            )
                                        | _ -> None)

                                let! updatedGraphWithRels =
                                    Seq.fold(fun g (rel, source, sink) -> 
                                        g |> Result.bind(Storage.addRelation source sink rel)) (Ok updatedGraph) relationsByKey
                                return updatedGraphWithRels
                            }
                        match add () with
                        | Ok g -> { model with Graph = Some g; TaxonLookup = { Result = None; Rank = "Family"; Family = ""; Genus = ""; Species = ""; Authorship = "" }}, Cmd.none
                        | Error e -> { model with Message = Some (ErrorMessage, e) }, Cmd.none
        
        // --- Start batch mode messages ---
        | UseBatchModeForProxiedTaxon (timeline, mode) ->
            match model.SelectedSource with
            | None -> { model with Message = Some (ErrorMessage, "Source was not selected.") }, Cmd.none
            | Some source ->
                match mode with
                | true -> { model with SelectedSource = Some { source with AddBioticHyperedgeBatch = source.AddBioticHyperedgeBatch |> Map.add timeline { Group = Population.BioticProxies.Pollen; Outcome = None; InferenceMethod = None; UsePlaceholderInfer = false; UsePlaceholderTaxon = false; LinkedTaxa = Map.empty; UnlinkedTaxa = Map.empty } } }, Cmd.none
                | false -> { model with SelectedSource = Some { source with AddBioticHyperedgeBatch = source.AddBioticHyperedgeBatch |> Map.remove timeline } }, Cmd.none
        
        | SetBatchModeAutofill (timeline, group,infer, outcome,usePlaceholderTaxon, usePlaceholderInfer) ->
            model |> isBatchMode timeline (fun (source, batch) ->
                let infer = if usePlaceholderInfer then Some <| Graph.UniqueKey.FriendlyKey("inferencemethodnode", "atlas_unknown not yet coded") else infer
                { model with SelectedSource = Some { source with AddBioticHyperedgeBatch = source.AddBioticHyperedgeBatch |> Map.add timeline { batch with Group = group; Outcome = outcome; InferenceMethod = infer; UsePlaceholderTaxon = usePlaceholderTaxon; UsePlaceholderInfer = usePlaceholderInfer } } }, Cmd.none)
            
        | ChangeBatchUnverifiedProxiedTaxon (timeline, i,morph,taxa) ->
            model |> isBatchMode timeline (fun (source, batch) ->
                let updatedUnverified =
                    if batch.UnlinkedTaxa.IsEmpty then batch.UnlinkedTaxa |> Map.add 1 (morph, taxa)
                    else 
                        let key = if i = 999 then (fst (Map.maxKeyValue batch.UnlinkedTaxa) + 1) else i
                        batch.UnlinkedTaxa |> Map.add key (morph, taxa)
                { model with SelectedSource = Some { source with AddBioticHyperedgeBatch = source.AddBioticHyperedgeBatch |> Map.add timeline { batch with UnlinkedTaxa = updatedUnverified } } }, Cmd.none)

        | ChangeBatchVerifiedTaxon (timeline, i,x) ->
            model |> isBatchMode timeline (fun (source, batch) ->
                let updatedLinked = batch.LinkedTaxa |> Map.add i x
                { model with SelectedSource = Some { source with AddBioticHyperedgeBatch = source.AddBioticHyperedgeBatch |> Map.add timeline { batch with LinkedTaxa = updatedLinked } } }, Cmd.none)
        
        | RemoveBatchVerifiedTaxon (timeline, i) ->
            model |> isBatchMode timeline (fun (source, batch) ->
                let updatedLinked = batch.LinkedTaxa |> Map.remove i
                { model with SelectedSource = Some { source with AddBioticHyperedgeBatch = source.AddBioticHyperedgeBatch |> Map.add timeline { batch with LinkedTaxa = updatedLinked } } }, Cmd.none)

        | RemoveBatchUnverifiedTaxon (timeline, i) ->
            model |> isBatchMode timeline (fun (source, batch) ->
                let updatedUnlinked = batch.UnlinkedTaxa |> Map.remove i
                { model with SelectedSource = Some { source with AddBioticHyperedgeBatch = source.AddBioticHyperedgeBatch |> Map.add timeline { batch with UnlinkedTaxa = updatedUnlinked } } }, Cmd.none)

        | ValidateOrConfirmBatch timeline ->
            model |> isBatchMode timeline (fun (source, batch) ->
                if not batch.LinkedTaxa.IsEmpty && batch.UnlinkedTaxa.IsEmpty then // Confirm batch
                    match model.Graph with
                    | Some g ->
                        let updatedGraph =
                            Map.fold(fun state k (proxyId,inferId,taxaIds,measureId) ->
                                state |> Result.bind(fun g -> commitProxiedTaxon proxyId inferId taxaIds measureId timeline g)) (Ok g) batch.LinkedTaxa
                        match updatedGraph with
                        | Ok saved -> { model with Graph = Some saved; SelectedSource = Some { source with AddBioticHyperedgeBatch = source.AddBioticHyperedgeBatch |> Map.remove timeline } }, Cmd.ofMsg (SelectSource (source.SelectedSource |> fst |> fst))
                        | Error e -> { model with Message = Some (ErrorMessage, e) }, Cmd.none
                    | None -> { model with Message = Some (ErrorMessage, "Graph is not loaded.") }, Cmd.none
                else if batch.UnlinkedTaxa.Count > 0 then
                    match model.Graph with
                    | Some g ->

                        // Validate unlinked taxa.
                        // For each one, move it IF it is valid. Otherwise, leave where it is.
                        let newLinked, newUnlinked, errors = 
                            Map.fold(fun (linked, unlinked, errors) k (morphotype, taxaList:string) ->

                                let taxa = 
                                    if batch.UsePlaceholderTaxon
                                    then [| "Unknown (not yet coded)" |]
                                    else taxaList.Split(";") |> Array.map(fun s -> s.Trim())
                                if Seq.isEmpty taxa || batch.InferenceMethod.IsNone || batch.Outcome.IsNone
                                then (linked, unlinked, sprintf "Some details were blank for entry %i" k :: errors)
                                else
                                    let r = result {
                                        let! taxaNodes = 
                                            taxa 
                                            |> Seq.map(fun t -> t |> FieldDataTypes.Text.createShort)
                                            |> Seq.toList |> Result.ofList
                                        let! morphotypeNode = 
                                            morphotype |> FieldDataTypes.Text.createShort 
                                            |> Result.lift(fun t -> 
                                                Population.BioticProxies.Microfossil(batch.Group, t) |> Population.BioticProxies.Morphotype)
                                        
                                        let taxonExists = 
                                            taxaNodes
                                            |> List.choose(fun t -> Storage.tryFindTaxonByName t g)
                                        let morphotypeExists = Storage.tryFindProxy morphotypeNode g

                                        if taxonExists.Length = taxa.Length && morphotypeExists.IsSome
                                        then return (linked |> Map.add k (morphotypeExists.Value |> fst |> fst, batch.InferenceMethod.Value, (taxonExists |> List.map(fun t -> t |> fst |> fst)), batch.Outcome.Value), unlinked |> Map.remove k, errors)
                                        else return (linked, unlinked, sprintf "At least one node doesn't exist already for %i (%s / %s)" k morphotype taxaList :: errors)
                                    }
                                    match r with
                                    | Ok r -> r
                                    | Error e -> (linked, unlinked, e :: errors)
                                ) (batch.LinkedTaxa, batch.UnlinkedTaxa, []) batch.UnlinkedTaxa
                        let isError = if errors.Length = 0 then None else Some (ErrorMessage, String.concat ", and " errors)
                        { model with Message = isError; SelectedSource = Some { source with AddBioticHyperedgeBatch = source.AddBioticHyperedgeBatch |> Map.add timeline { batch with LinkedTaxa = newLinked; UnlinkedTaxa = newUnlinked } } }, Cmd.none
                    | None -> model, Cmd.none
                else { model with Message = Some (ErrorMessage, "Cannot validate or confirm taxa when none are specified") }, Cmd.none
            )
        // --- End batch mode messages ---

        | ChangeProxiedTaxonVm(timeline, proxy, inference, taxon, measure) -> 
            match model.SelectedSource with
            | None -> { model with Message = Some (ErrorMessage, "Source was not selected.") }, Cmd.none
            | Some source ->
                { model with SelectedSource = Some { source with AddBioticHyperedge = source.AddBioticHyperedge |> Map.add timeline (proxy, inference, taxon, measure) } }, Cmd.none
        | SubmitProxiedTaxon timelineId -> 
            match model.Graph with
            | Some g ->
                match model.SelectedSource with
                | Some source -> 
                    match source.AddBioticHyperedge |> Map.tryFind timelineId with
                    | Some (proxyId, inferId, taxaIds, measureId) ->
                        match commitProxiedTaxon proxyId.Value inferId.Value taxaIds.Value measureId.Value timelineId g with
                        | Ok saved -> { model with Graph = Some saved }, Cmd.ofMsg (SelectSource (source.SelectedSource |> fst |> fst))
                        | Error e -> { model with Message = Some (ErrorMessage, e) }, Cmd.none
                    | None -> { model with Message = Some (ErrorMessage, "No hyper edge details found") }, Cmd.none
                | None -> { model with Message = Some (ErrorMessage, "No source loaded") }, Cmd.none
            | None -> { model with Message = Some (ErrorMessage, "No graph loaded") }, Cmd.none
        | ToggleConnectNewOrExistingSource ->
            match model.SelectedSource with
            | Some s -> { model with SelectedSource = Some { s with AddingNewSource = not s.AddingNewSource } }, Cmd.none
            | None -> model, Cmd.none
        | ChangeProposedSourceLink k ->
            match model.SelectedSource with
            | Some s -> { model with SelectedSource = Some { s with ProposedLink = k } }, Cmd.none
            | None -> model, Cmd.none
        | MarkPrimary is ->
            match model.SelectedSource with
                | Some s -> { model with SelectedSource = Some { s with MarkedPrimary = is } }, Cmd.none
                | None -> model, Cmd.none
        | CompleteSection section ->
            match model.Graph with
            | Some g ->
                match model.SelectedSource with
                | Some source ->
                    let allSectionsComplete (completed:FieldDataTypes.Text.ShortText list) =
                        let diff = Set.difference (Set.ofSeq (sections |> Map.keys)) (Set.ofList (completed |> List.map(fun s -> s.Value)))
                        diff.IsEmpty
                    let updateSource section =
                        match source.SelectedSource |> fst |> snd with
                        | GraphStructure.Node.SourceNode s ->
                            match s with
                            | Sources.SourceNode.Included (s,progress) ->
                                match progress with
                                | Sources.CodingProgress.CompletedAll -> (s, progress) |> Sources.Included |> Ok
                                | Sources.CodingProgress.CompletedNone -> 
                                    if allSectionsComplete [section]
                                    then (s, Sources.CodingProgress.CompletedAll) |> Sources.Included |> Ok
                                    else (s, Sources.CodingProgress.InProgress [section]) |> Sources.Included |> Ok
                                | Sources.CodingProgress.InProgress completed -> 
                                    if allSectionsComplete ((section :: completed) |> List.distinct)
                                    then (s, Sources.CodingProgress.CompletedAll) |> Sources.Included |> Ok
                                    else (s, Sources.CodingProgress.InProgress ((section :: completed) |> List.distinct)) |> Sources.Included |> Ok
                                | Sources.CodingProgress.Stalled (completed,stalledSection,reason) -> 
                                    if stalledSection = section then 
                                        if allSectionsComplete ((section :: completed) |> List.distinct)
                                        then (s, Sources.CompletedAll) |> Sources.Included |> Ok
                                        else (s, Sources.InProgress ((section :: completed) |> List.distinct)) |> Sources.Included |> Ok
                                    else (s, (Sources.Stalled (((section :: completed) |> List.distinct),stalledSection,reason))) |> Sources.Included |> Ok
                            | _ -> Error "Cannot complete section on non-included sources"
                        | _ -> Error "No source was selected"
                    FieldDataTypes.Text.createShort section
                    |> Result.bind updateSource
                    |> Result.lift GraphStructure.Node.SourceNode
                    |> Result.bind (fun n -> Storage.updateNode g (source.SelectedSource |> fst |> fst, n))
                    |> Result.lift (fun g -> { model with Graph = Some g }, Cmd.ofMsg (SelectSource (source.SelectedSource |> fst |> fst)))
                    |> Result.lower (fun r -> r) (fun e -> { model with Message = Some (ErrorMessage, e) }, Cmd.none)
                | None -> { model with Message = Some (ErrorMessage, "Cannot modify source as graph is not loaded.") }, Cmd.none
            | None -> model, Cmd.none
        | ChangeCodingProblem(section, reason) ->
            match model.SelectedSource with
            | Some s -> { model with SelectedSource = Some { s with ProblematicSection = section; ProblematicSectionReason = reason } }, Cmd.none
            | None -> model, Cmd.none
        | SubmitCodingProblem ->
            match model.Graph with
            | Some g ->
                match model.SelectedSource with
                | Some source ->
                    // Update the source node and save it.
                    let updateSource problemSection problemReason =
                        match source.SelectedSource |> fst |> snd with
                        | GraphStructure.Node.SourceNode s ->
                            match s with
                            | Sources.SourceNode.Included (s,progress) ->
                                match progress with
                                | Sources.CodingProgress.CompletedAll -> Error "Cannot flag when all completed"
                                | Sources.CodingProgress.CompletedNone -> (s, Sources.CodingProgress.Stalled ([], problemSection, problemReason)) |> Sources.Included |> Ok
                                | Sources.CodingProgress.InProgress completed
                                | Sources.CodingProgress.Stalled (completed,_,_) -> 
                                    if completed |> Seq.contains problemSection then Error "Cannot flag a completed section"
                                    else (s, Sources.CodingProgress.Stalled (completed, problemSection, problemReason)) |> Sources.Included |> Ok
                            | _ -> Error "Cannot complete section on non-included sources"
                        | _ -> Error "No source was selected"
                    (updateSource <!> (FieldDataTypes.Text.createShort source.ProblematicSection) <*> (FieldDataTypes.Text.create source.ProblematicSectionReason))
                    |> Result.bind id
                    |> Result.lift GraphStructure.Node.SourceNode
                    |> Result.bind (fun n -> Storage.updateNode g (source.SelectedSource |> fst |> fst, n))
                    |> Result.lift (fun g -> { model with Graph = Some g }, Cmd.ofMsg (SelectSource (source.SelectedSource |> fst |> fst)))
                    |> Result.lower (fun r -> r) (fun e -> { model with Message = Some (ErrorMessage, e) }, Cmd.none)
                | None -> { model with Message = Some (ErrorMessage, "Cannot modify source as graph is not loaded.") }, Cmd.none
            | None -> model, Cmd.none
        | ChangeProposedSourceDatabaseLink p -> 
            match model.SelectedSource with
            | Some s -> { model with SelectedSource = Some { s with ProposedDatabaseLink = p } }, Cmd.none
            | None -> model, Cmd.none

        | EnterCrossRefSearch s -> { model with CrossRefLookupModel = { model.CrossRefLookupModel with SearchTerm = s }}, Cmd.none
        | SearchCrossRef q ->
            match Sources.CrossRef.tryMatch q with
            | Ok m ->
                match m with
                | Some n -> { model with CrossRefLookupModel = { model.CrossRefLookupModel with Match = Some n }}, Cmd.none
                | None -> { model with Message = Some (InfoMessage, sprintf "No viable match found in CrossRef for %s" model.CrossRefLookupModel.SearchTerm); CrossRefLookupModel = { model.CrossRefLookupModel with Match = None }}, Cmd.none
            | Error e -> { model with Message = Some (ErrorMessage, e) }, Cmd.none
        | ClearCrossRef -> { model with CrossRefLookupModel = { SearchTerm = ""; Match = None } }, Cmd.none
        | AddCrossRefMatch -> 
            match model.Graph with
            | Some g ->
                match model.CrossRefLookupModel.Match with
                | Some m ->
                    match Storage.addNodes g [ GraphStructure.SourceNode (Sources.SourceNode.Unscreened m) ] with
                    | Ok (g,n) ->
                        let newNodeId = (n.Head |> fst |> fst).AsString
                        { model with Graph = Some g; Message = Some (SuccessMessage, sprintf "Source %s added successfully" newNodeId ); CrossRefLookupModel = { SearchTerm = ""; Match = None } }, Cmd.none
                    | Error e -> { model with Message = Some (ErrorMessage, e) }, Cmd.none
                | None -> { model with Message = Some (ErrorMessage,"Cannot add a match, as one has not been found") }, Cmd.none
            | None -> model, Cmd.none


    let _class = attr.``class``

    let sidebarView (links:Page list) dispatch =
        div [ _class "col-auto col-md-3 col-xl-2 px-sm-2 px-0 bg-dark" ] [
            div [ _class "d-flex flex-column align-items-center align-items-sm-start px-3 pt-2 text-white min-vh-100" ] [
                ul [ _class "nav nav-pills flex-column mb-sm-auto mb-0 align-items-center align-items-sm-start" ] [
                    forEach links <| fun page ->
                        li [ _class "nav-item" ] [
                            a [ _class "nav-link align-middle px-0"; attr.href "#"; on.click (fun _ -> page |> SetPage |> dispatch) ] [ (Reflection.FSharpValue.GetUnionFields(page, typeof<Page>) |> fst).Name |> text ]
                        ]
                ]
            ]
        ]

    let alert' name msg dispatch =
        div [ _class <| sprintf "alert alert-%s" name ] [ 
            text msg
            button [ _class "btn"; on.click (fun _ -> DismissMessage |> dispatch) ] [ text "Dismiss" ] ]

    let alert model dispatch =
        cond model.Message <| function
        | Some msg ->
            cond (fst msg) <| function
            | ErrorMessage -> alert' "danger" (snd msg) dispatch
            | SuccessMessage -> alert' "success" (snd msg) dispatch
            | InfoMessage -> alert' "info" (snd msg) dispatch
        | None -> empty

    let timelineAtomDetailsView timelineAtom g =
        div [ _class "card "] [
            div [ _class "card-body" ] [
                cond (timelineAtom |> GraphStructure.Relations.nodeIdsByRelation<Exposure.ExposureRelation> Exposure.ExposureRelation.ExtentEarliest |> Seq.tryHead |> Option.bind (fun k -> Storage.atomFriendlyNameByKey k g)) <| function
                | Some early ->
                    cond (timelineAtom |> GraphStructure.Relations.nodeIdsByRelation<Exposure.ExposureRelation> Exposure.ExposureRelation.ExtentLatest |> Seq.tryHead |> Option.bind (fun k -> Storage.atomFriendlyNameByKey k g)) <| function
                    | Some late ->
                        h5 [ _class "card-title" ] [ textf "%s - %s timeline" early late ]
                    | None -> empty
                | None -> empty
                cond ((timelineAtom |> fst |> snd) |> GraphStructure.Nodes.asExposureNode) <| function
                | Some exposureNode ->
                    cond exposureNode <| function
                    | Exposure.ExposureNode.TimelineNode timeline ->
                        cond timeline <| function
                        | Exposure.StudyTimeline.Continuous res ->
                            cond res <| function
                            | Exposure.StudyTimeline.Regular (r,g) -> h6 [ _class "card-subtitle mb-2 text-muted" ] [ textf "A continuous timeline with %A year timesteps (formed using %A)" r g ]
                            | Exposure.StudyTimeline.Irregular -> h6 [ _class "card-subtitle mb-2 text-muted" ] [ textf "A continuous timeline with irregular timesteps" ]
                        | Exposure.StudyTimeline.Discontinuous (res,_) ->
                            cond res <| function
                            | Exposure.StudyTimeline.Regular (r,g) -> h6 [ _class "card-subtitle mb-2 text-muted" ] [ textf "A continuous timeline with %A year timesteps (formed using %A)" r g ]
                            | Exposure.StudyTimeline.Irregular -> h6 [ _class "card-subtitle mb-2 text-muted" ] [ textf "A discontinuous timeline with irregular timesteps" ]
                    | _ -> empty
                | _ -> empty
                cond (timelineAtom |> GraphStructure.Relations.nodeIdsByRelation<Exposure.ExposureRelation> Exposure.ExposureRelation.IsLocatedAt |> Seq.tryHead |> Option.bind (fun k -> Storage.atomByKey k g)) <| function
                | Some context ->
                    p [] [
                        cond (context |> fst |> snd) <| function
                        | GraphStructure.Node.PopulationNode p ->
                            cond p <| function
                            | GraphStructure.ContextNode c ->
                                concat [
                                    textf "%s. " c.Name.Value
                                    cond c.SamplingLocation <| function
                                    | FieldDataTypes.Geography.Site (lat,lon) -> textf "Occurs at the point %A DD, %A, DD" lat lon
                                    | FieldDataTypes.Geography.Area poly -> textf "Occurs in an area: %s" <| poly.ToString()
                                    | FieldDataTypes.Geography.Locality (l,d,r,c) -> textf "Occurs at the locality %s (%s, %s, %s)" l.Value d.Value r.Value c.Value
                                    | FieldDataTypes.Geography.Region (r,c) -> textf "Occurs in the region %s (%s)" r.Value c.Value
                                    | FieldDataTypes.Geography.Country (c) -> textf "Occurs in the country %s" c.Value
                                    | _ -> textf "Occurs at %s." c.Name.Value
                                ]
                            | _ -> empty
                        | _ -> empty
                    ]
                | None -> empty
                small [] [ text "The time-series is constructed from the following individual dates:" ]
                ul [] [
                    forEach (timelineAtom |> GraphStructure.Relations.nodeIdsByRelation<Exposure.ExposureRelation> Exposure.ExposureRelation.ConstructedWithDate |> Seq.map (fun k -> Storage.atomByKey k g)) <| function
                    | Some dateNode ->
                            cond (dateNode |> fst |> snd) <| function
                            | GraphStructure.Node.ExposureNode p ->
                                cond p <| function
                                | Exposure.ExposureNode.DateNode d ->
                                    li [] [ textf "Esimated date from %A (depth %A). Method was %A." d.MaterialDated.Value d.SampleDepth d.Date ]
                                | _ -> empty
                            | _ -> empty
                    | None -> empty
                ]
            ]
        ]

    let displayDataTableSummary (digi:Datasets.DigitisedDataset) =
        div [ _class "table-responsive" ] [
            p [] [ text (sprintf "Digitised data from figure %s. %A (%A). [only showing top six rows]" digi.WhatWasDigitised.Name digi.Metric digi.Units) ]
            table [ _class "table" ] [
                cond (Some <| digi.DataTable.Depths()) <| function
                | Some (indexUnit, rows) -> concat [
                    thead [] [
                        tr [] [
                            th [] [ textf "%A" indexUnit ]
                            forEach (digi.DataTable.Morphotypes()) <| fun taxa -> th [] [ text taxa ]
                        ]
                    ]
                    tbody [] [
                        forEach (rows |> Seq.truncate 6) <| fun row ->
                            tr [] [
                                td [] [ textf "%f" row.Key ]
                                forEach row.Value <| fun v -> td [] [ textf "%f" v ]
                            ]
                    ]]
                | None -> empty
            ]
        ]

    let private removeUnit (x:float<_>) = float x

    /// Connects a date to the pre-Holocene 'out of scope' node, for when a time series contains
    /// a maximum extent that is older than the Holocene boundary.
    let trySelectPreHoloceneScope (graph:Storage.FileBasedGraph<GraphStructure.Node,GraphStructure.Relation>) (value:FieldDataTypes.OldDate.OldDateSimple) =
        let v =
            match value with
            | FieldDataTypes.OldDate.OldDateSimple.CalYrBP (f, _) -> removeUnit f
            | FieldDataTypes.OldDate.OldDateSimple.HistoryYearAD f -> 1950. - removeUnit f
            | FieldDataTypes.OldDate.OldDateSimple.HistoryYearBC f -> removeUnit f + 1950.
            | FieldDataTypes.OldDate.OldDateSimple.BP f -> removeUnit f
        if v > 11650 then
            let key = Graph.UniqueKey.FriendlyKey("qualitativelabeloutofscopenode","pre-holocene_by_global stratotype section and point")
            graph.Nodes<Exposure.TemporalIndex.QualitativeLabelOutOfScopeNode>()
            |> Option.bind(fun n -> if Map.containsKey key n then Some (key, value) else None)
        else None

    /// Tests if a section is marked as complete on a source
    let isCompletedSection name codingStatus =
        match codingStatus with
        | Sources.CodingProgress.CompletedAll -> true
        | Sources.CodingProgress.CompletedNone -> false
        | Sources.CodingProgress.InProgress l -> l |> Seq.map (fun l -> l.Value) |> Seq.contains name
        | Sources.CodingProgress.Stalled (l,_,_) -> l |> Seq.map (fun l -> l.Value) |> Seq.contains name

    let isFlaggable codingStatus =
        match codingStatus with
        | Sources.CodingProgress.CompletedAll -> false
        | _ -> true

    let codingStatusView codingStatus =
        cond codingStatus <| function
        | Sources.CodingProgress.CompletedAll -> div [ _class "alert alert-success" ] [ text "Data coding is complete for this source" ]
        | Sources.CodingProgress.CompletedNone
        | Sources.CodingProgress.InProgress _ -> p [] [ text "Data coding is not yet complete for this source" ]
        | Sources.CodingProgress.Stalled (_,s,r) ->
            div [ _class "alert alert-warning" ] [ 
                textf "A problem has been flagged when coding this source, which needs to be resolved. %s (section: %s)." r.Value s.Value ]

    let asDatabaseLink (link:ProposedDatabaseLink) = 
        result {
            let! date =
                if link.AccessDate = "" then Ok None
                else
                    let couldParse, parsedDate = System.DateOnly.TryParse(link.AccessDate)
                    if not couldParse then Error "Date not in correct format"
                    else Ok <| Some parsedDate
            match link.AccessMethod with
            | "all" -> return Sources.SourceRelation.UsedDatabase(date, Sources.AllRecordsInStudyScope)
            | "specific" -> 
                let! textIds = 
                    link.AccessDetails.Split("\n")
                    |> Array.map(fun s -> FieldDataTypes.Text.createShort s)
                    |> Array.toList
                    |> Result.ofList
                let! ids = 
                    if textIds.Length = 1 then Ok (textIds.[0], [])
                    else if textIds.Length = 0 then Error "No IDs specified"
                    else Ok (textIds.[0], List.tail textIds)
                return Sources.SourceRelation.UsedDatabase(date, Sources.SpecificRecords ids)
            | "complex" -> 
                let! details = link.AccessDetails |> FieldDataTypes.Text.create
                return Sources.SourceRelation.UsedDatabase(date, Sources.ComplexSubset details)
            | _ -> return! Error "Not a valid access method"
        } |> Result.toOption

    let sourceTitle = function
        | Sources.Source.Bibliographic b -> b.Title
        | Sources.Source.DarkData d -> Some d.Details
        | Sources.Source.Database d -> Some d.FullName
        | Sources.Source.DatabaseEntry _ -> None
        | Sources.Source.GreyLiterature g -> Some g.Title
        | Sources.Source.GreyLiteratureSource g -> Some g.Title
        | Sources.Source.DarkDataSource d -> Some d.Details
        | Sources.Source.PublishedSource s ->
            match s with
            | Sources.Book b -> Some b.BookTitle
            | Sources.BookChapter c -> Some c.ChapterTitle
            | Sources.Dissertation c -> Some c.Title
            | Sources.IndividualDataset d -> Some d.Title
            | Sources.JournalArticle d -> Some d.Title

    let sourceDetailsView source =
        div [ _class "card"; attr.style "width: 30rem" ] [
            div [ _class "card-body" ] [
                textf "%A" source
            ]
        ]

    let scenarioView<'a> title description formTitle formDescription extraElements (model:Model) dispatch =
        concat [
            h2 [] [ textf "Scenario: %s" title ]
            p [] [ text description ]
            alert model dispatch
            cond model.SelectedSource <| function
            | None -> p [] [ text "Select a source in the 'extract' view to use the scenario." ]
            | Some s -> 
                cond (s.SelectedSource |> fst |> snd) <| function
                | GraphStructure.Node.SourceNode sn ->
                    cond sn <| function
                    | Sources.SourceNode.Included (s,_) -> concat [
                        cond (sourceTitle s) <| function
                        | Some title -> p [] [ textf "Selected source: %s" title.Value ]
                        | None -> p [] [ text "Unknown source selected" ]
                        div [ _class "card mb-4" ] [
                            div [ _class "card-header text-bg-secondary" ] [ text "Source Status: Included" ]
                            div [ _class "card-body" ] [
                                div [ _class "alert alert-success" ] [ text "This source has been included at full-text level. Please code information as stated below." ]
                                div [ _class "card mb-4" ] [
                                    div [ _class "card-header text-bg-secondary" ] [ text formTitle ]
                                    div [ _class "card-body" ] [
                                        p [] [ text formDescription ]
                                        extraElements
                                        Scenarios.scenarioGen<'a> (model.NodeCreationViewModels |> Map.tryFind (typeof<'a>).Name) (FormMessage >> dispatch)
                                        textf "%A" ((model.NodeCreationViewModels |> Map.tryFind (typeof<'a>).Name))
                                    ]
                                ]
                            ]
                        ]]
                    | _ -> text "You may only use scenarios on 'Included' sources."
                | _ -> empty
        ]

    let asMicrofossilGroup = function
        | s when s = "Pollen" -> Population.BioticProxies.MicrofossilGroup.Pollen
        | s when s = "Ostracod" -> Population.BioticProxies.MicrofossilGroup.Ostracod
        | s when s = "PlantMacrofossil" -> Population.BioticProxies.MicrofossilGroup.PlantMacrofossil
        | s when s = "Diatom" -> Population.BioticProxies.MicrofossilGroup.Diatom
        | _ -> Population.BioticProxies.MicrofossilGroup.Pollen

    let view model dispatch =
        div [ _class "container-fluid" ] [
            div [ _class "row flex-nowrap" ] [ 
                // 1. Sidebar for selecting section
                // Should link to editable info for core node types: population (context, proxied taxa), exposure (time), outcome (biodiversity indicators).
                sidebarView [ Page.Extract; Page.Population; Page.Exposure; Page.Outcome; Page.AppendData; Page.Sources; Page.Scenario WoodRing; Page.Scenario SimpleEntry; Page.Statistics; Page.Settings ] dispatch

                // 2. Page view
                div [ _class "col" ] [
                    cond model.Page <| function
                        | Page.AppendData ->
                            concat [
                                h2 [] [ text "Append / digitise datasets" ]
                                p [] [
                                    text "Below you may append digitised datasets to individual timelines coded into a source."
                                    text "\nDatasets should be digitised using appropriate digitisation software and saved as a tab-delimited file."
                                    text "\nIn the tab-delimited text file, the first column should be either (1) depth in centimetres, or (2) 'bp', 'cal yr bp', 'ad', or 'bc' if the data is age-indexed only and no depths are available."
                                ]

                                cond model.SelectedSource <| function
                                | None -> p [] [ text "Select a source in the 'extract' view first." ]
                                | Some s -> 
                                    cond (s.SelectedSource |> fst |> snd) <| function
                                    | GraphStructure.Node.SourceNode sn ->
                                        cond sn <| function
                                        | Sources.SourceNode.Included (includedSource,_) ->
                                            cond model.Graph <| function
                                            | Some g -> concat [

                                                concat [
                                                    // Show existing datasets here.
                                                    h3 [] [ text "Summary of timelines in this source" ]
                                                    hr []
                                                    forEach (s.SelectedSource |> GraphStructure.Relations.nodeIdsByRelation<Sources.SourceRelation> Sources.SourceRelation.HasTemporalExtent |> Storage.atomsByKey g ) <| fun timelineAtom ->
                                                        div [ _class "card mb-4" ] [
                                                            div [ _class "card-header text-bg-secondary" ] [ text "Timeline" ]
                                                            div [ _class "card-body" ] [
                                                                timelineAtomDetailsView timelineAtom g
                                                                forEach (timelineAtom |> GraphStructure.Relations.nodeIdsByRelation<Exposure.ExposureRelation> Exposure.ExposureRelation.HasRawData |> Storage.atomsByKey g ) <| fun rawDataAtom ->
                                                                    cond ((rawDataAtom |> fst |> snd) |> GraphStructure.Nodes.asDatasetNode) <| function
                                                                    | Some datasetNode ->
                                                                        cond datasetNode <| function
                                                                        | Datasets.DatasetNode.Digitised digi ->
                                                                            displayDataTableSummary digi
                                                                    | None -> empty
                                                            ]
                                                        ]

                                                    h3 [] [ text "Add a digitised dataset" ]
                                                    hr []
                                                    div [ _class "card mb-4" ] [
                                                        div [ _class "card-header text-bg-secondary" ] [ text "Append a dataset to a timeline" ]
                                                        div [ _class "card-body" ] [
                                                            p [] [ text "Select a timeline from the selected study." ]
                                                            select [ _class "form-select"; bind.change.string (if s.SelectedTimeline.IsSome then s.SelectedTimeline.Value.AsString else "") 
                                                                (fun s -> s |> Graph.stringToKey |> SelectTimeline |> dispatch) ] [
                                                                ViewGen.optionGenFiltered<Exposure.StudyTimeline.IndividualTimelineNode> (fun k -> s.SelectedSource |> snd |> List.map(fun (_,sink,_,_) -> sink) |> List.contains k) model.Graph
                                                            ]
                                                            cond s.SelectedTimeline <| function
                                                            | None -> empty
                                                            | Some timeline ->
                                                                concat [
                                                                    cond (Storage.atomByKey timeline g) <| function
                                                                    | Some atom -> timelineAtomDetailsView atom g
                                                                    | None -> text "Problem fetching timeline node"
                                                                    ViewGen.RelationsForms.relationsToggle<Datasets.DatasetNode> "ProxyGroup" [
                                                                        ("ProxyGroup", [
                                                                            ViewGen.RelationsForms.selectExistingNode<Population.BioticProxies.BioticProxyCategoryNode> "Which proxy group are the morphotypes from?" "" (Datasets.DatasetRelation.IsProxyGroup |> GraphStructure.ProposedRelation.Dataset)
                                                                        ])] model.NodeCreationRelations g (FormMessage >> dispatch)
                                                                    ViewGen.makeNodeFormWithRelations<Datasets.DatasetNode> (fun savedRelations ->
                                                                        ViewGen.RelationsForms.Validation.hasOne (GraphStructure.ProposedRelation.Dataset Datasets.DatasetRelation.IsProxyGroup) savedRelations)
                                                                        (model.NodeCreationViewModels |> Map.tryFind "DatasetNode") [
                                                                            ThisIsSink (timeline, GraphStructure.ProposedRelation.Exposure(Exposure.ExposureRelation.HasRawData))
                                                                        ] (FormMessage >> dispatch)
                                                                ]
                                                        ]
                                                    ]
                                                ]
                                                ]
                                            | _ -> empty
                                        | _ -> empty
                                    | _ -> empty
                            ]
                        | Page.Settings -> concat [
                                h2 [] [ text "Settings" ]
                                p [] [ text "Here you can change the interface's behaviour and reload the database." ]
                                div [ _class "row mb-3" ] [
                                    label [ attr.``for`` "toggle-keep-values"; _class "col-sm-2 col-form-label" ] [ text "Keep field values after successful data entry" ]
                                    div [ _class "col-sm-10" ] [
                                        input [ attr.``type`` "checkbox"; bind.``checked`` model.LeaveFieldsFilled (fun _ -> ToggleLeaveFilled |> dispatch) ]
                                        small [ _class "form-text" ] [ text "If you are entering repetitive information, you may toggle this option on to temporarily keep values filled when a node is successfully created." ]
                                    ]
                                ]
                                div [ _class "card mb-4" ] [
                                    div [ _class "card-header text-bg-secondary" ] [ text "Reload database" ]
                                    div [ _class "card-body" ] [
                                        p [] [ text "By selecting reload below, the application will be reset. All form fields will be emptied and the database reloaded." ]
                                        button [ _class "btn btn-danger"; on.click(fun _ -> ResetApplication |> dispatch) ] [ text "Reload Database" ]
                                    ]
                                ]
                            ]
                        | Page.Scenario scenario ->
                            cond scenario <| function
                            | SimpleEntry ->
                                scenarioView<Scenarios.SiteOnlyScenario> 
                                    Scenarios.SiteOnlyScenario.Title 
                                    Scenarios.SiteOnlyScenario.Description
                                    "Streamlined Space/Time/Proxy entry"
                                    "For quick entry of the spatial-temporal properties of sources, use this form."
                                    empty model dispatch
                            | WoodRing -> 
                                scenarioView<Scenarios.WoodRingScenario> 
                                    Scenarios.WoodRingScenario.Title 
                                    Scenarios.WoodRingScenario.Description
                                    "Add a tree ring timeline"
                                    "Add a new timeline and associated site to the currently selected source (in the extract tab). All information is required. After creating, go back to the 'extract' tab and mark sections as complete as normal."
                                    empty model dispatch
                        | Page.Population -> concat [
                                h2 [] [ text "Population" ]
                                p [] [ text "List existing population nodes and create new ones." ]
                                img [ attr.src "images/population-diagram.png" ]
                                p [] [ 
                                    text "Use the forms on this page to add new options for taxonomic nodes."
                                    text "Plant taxa are created by validating names against a taxonomic backbone." ]
                                alert model dispatch

                                // Allow linking to taxonomic backbone here.
                                div [ _class "card mb-4" ] [
                                    div [ _class "card-header text-bg-secondary" ] [ text "Plant Taxonomy: Add a Taxon" ]
                                    div [ _class "card-body" ] [
                                        div [ _class "row g-3" ] [
                                            div [ _class "col" ] [
                                                label [] [ text "Rank" ]
                                                select [ _class "form-select"; bind.change.string model.TaxonLookup.Rank (fun i -> { model.TaxonLookup with Rank = i} |> ChangeFormFields |> LookupTaxon |> dispatch) ] [
                                                    option [ attr.value "Family" ] [ text "Family" ]
                                                    option [ attr.value "Genus" ] [ text "Genus" ]
                                                    option [ attr.value "Species" ] [ text "Species" ]
                                                ]
                                            ]
                                            div [ _class "col-md-3" ] [
                                                label [] [ text "Family" ]
                                                input [ _class "form-control"; bind.input.string model.TaxonLookup.Family (fun f -> { model.TaxonLookup with Family = f} |> ChangeFormFields |> LookupTaxon |> dispatch) ]
                                            ]
                                            div [ _class "col-md-3" ] [
                                                label [] [ text "Genus" ]
                                                input [ _class "form-control"; bind.input.string model.TaxonLookup.Genus (fun g -> { model.TaxonLookup with Genus = g} |> ChangeFormFields |> LookupTaxon |> dispatch) ]
                                            ]
                                            div [ _class "col-md-3" ] [
                                                label [] [ text "Species" ]
                                                input [ _class "form-control"; bind.input.string model.TaxonLookup.Species (fun s -> { model.TaxonLookup with Species = s} |> ChangeFormFields |> LookupTaxon |> dispatch) ]
                                            ]
                                            div [ _class "col-md-3" ] [
                                                label [] [ text "Authorship" ]
                                                input [ _class "form-control"; bind.input.string model.TaxonLookup.Authorship (fun a -> { model.TaxonLookup with Authorship = a} |> ChangeFormFields |> LookupTaxon |> dispatch) ]
                                            ]
                                        ]

                                        cond <| model.TaxonLookup.Result <| function
                                        | Some (taxon, relations) -> concat [
                                                // TODO better display of taxa found
                                                p [] [ textf "The taxon found is: %A" taxon ]
                                                p [] [ textf "This is the heirarchy: %A" relations ]
                                                button [ _class "btn btn-primary"; on.click(fun _ -> SaveTaxonResult |> LookupTaxon |> dispatch) ] [ text "Confirm: add these taxa" ]
                                                button [ _class "btn btn-danger"; on.click (fun _ -> ChangeFormFields { Rank = "Family"; Family = ""; Genus = ""; Species = ""; Authorship = ""; Result = None } |> LookupTaxon |> dispatch)] [ text "Reset" ]
                                            ]
                                        | None ->
                                            div [ _class "col-12" ] [
                                                button [ _class "btn btn-primary"; on.click (fun _ -> RunLookup |> LookupTaxon |> dispatch)] [ text "Lookup name in taxonomic backbone" ]
                                                button [ _class "btn btn-danger"; on.click (fun _ -> ChangeFormFields { Rank = "Family"; Family = ""; Genus = ""; Species = ""; Authorship = ""; Result = None } |> LookupTaxon |> dispatch)] [ text "Reset" ]
                                            ]
                                    ]
                                ]

                                div [ _class "card mb-4" ] [
                                    div [ _class "card-header text-bg-secondary" ] [ text "Non-Plant Taxonomic Groups: Add a Taxon" ]
                                    div [ _class "card-body" ] [
                                        p [] [ text "In our systematic map, biotic proxies are related to real taxa by an inference method. Here, the 'Taxonomy Node' represents a *real* botanical or other taxon. We have pre-populated plant names using a taxonomic backbone." ]
                                        p [] [ text "Use this form to manually add taxa from non-plant groups. Make sure to add all levels in the hierarchy as accepted by the non-plant naming authority as specified in the protocol." ]
                                        cond model.Graph <| function
                                        | Some g -> concat [
                                                ViewGen.RelationsForms.relationsToggle<Population.Taxonomy.TaxonNode> "Parent taxon" [
                                                    ("Parent", [
                                                        ViewGen.RelationsForms.selectExistingNode<Population.Taxonomy.TaxonNode> "The parent taxon" "The taxon of the rank immediately above the desired new taxon. Example: the order 'Procellariiformes' for the family 'Procellariidae'." (Population.PopulationRelation.IsA |> GraphStructure.ProposedRelation.Population)
                                                    ])] model.NodeCreationRelations g (FormMessage >> dispatch)
                                                ViewGen.makeNodeFormWithRelations<Population.Taxonomy.TaxonNode> (fun savedRelations ->
                                                    ViewGen.RelationsForms.Validation.hasOne (GraphStructure.ProposedRelation.Population Population.PopulationRelation.IsA) savedRelations)
                                                    (model.NodeCreationViewModels |> Map.tryFind "TaxonNode") [] (FormMessage >> dispatch)
                                            ]
                                        | None -> empty
                                    ]
                                ]
                                div [ _class "card mb-4" ] [
                                    div [ _class "card-header text-bg-secondary" ] [ text "Add a Biotic Proxy" ]
                                    div [ _class "card-body" ] [
                                        p [] [ text "Biotic proxies are used to represent morphotypes, fossil remains etc. that are used to proxy species presence, but require further interpretation to connect to a *real* taxon. For example, pollen morphotypes require inference to connect to botanical taxa." ]
                                        ViewGen.makeNodeForm<Population.BioticProxies.BioticProxyNode> (model.NodeCreationViewModels |> Map.tryFind "BioticProxyNode") [] (FormMessage >> dispatch)
                                    ]
                                ]
                                div [ _class "card mb-4" ] [
                                    div [ _class "card-header text-bg-secondary" ] [ text "Add an Inference Method" ]
                                    div [ _class "card-body" ] [
                                        p [] [ text "Inference methods are used to translate from morphotypes or biotic proxies to real taxa." ]
                                        ViewGen.makeNodeForm<Population.BioticProxies.InferenceMethodNode> (model.NodeCreationViewModels |> Map.tryFind "InferenceMethodNode") [] (FormMessage >> dispatch)
                                    ]
                                ]
                                // p [] [ text "In addition to the taxon node, there are nodes representing the common or vernacular names of species, genera, or families. Adding these is purely for the purposes of public interpretation of the graph database." ]
                                // ViewGen.makeNodeForm<Population.Taxonomy.VernacularTaxonLabelNode> (model.NodeCreationViewModels |> Map.tryFind "VernacularTaxonLabelNode") [] (FormMessage >> dispatch)
                            ]
                        | Page.Exposure -> div [] [
                                h2 [] [ text "Exposure" ]
                                p [] [ text "The timeline information is pre-configured within the graph node structure. However, you may add qualitative time labels that represent certain periods in time." ]
                                alert model dispatch
                                div [ _class "card mb-4" ] [
                                    div [ _class "card-header text-bg-secondary" ] [ text "Add a qualitative time label" ]
                                    div [ _class "card-body" ] [
                                        p [] [ text "A qualitative label represents a temporal extent on a certain authority. For example, a pollen zone may be defined by a certain author in a certain region, which is used to 'date' cores by other authors. Alternatively, they may be internationally defined periods, such as Marine Isotope Stages." ]
                                        cond model.Graph <| function
                                        | Some g -> concat [
                                                ViewGen.RelationsForms.relationsToggle<Exposure.TemporalIndex.QualitativeLabelNode> "Temporal Index" [
                                                    ("Temporal Extent", [
                                                        ViewGen.RelationsForms.selectExistingNode<Exposure.TemporalIndex.CalYearNode> "Earliest year" "The calendar year BP (cal yr BP) that is the earliest included year." (Exposure.ExposureRelation.EarliestTime |> GraphStructure.ProposedRelation.Exposure)
                                                        ViewGen.RelationsForms.selectExistingNode<Exposure.TemporalIndex.CalYearNode> "Latest year" "The calendar year BP (cal yr BP) that is the latest included year." (Exposure.ExposureRelation.LatestTime |> GraphStructure.ProposedRelation.Exposure)
                                                    ])] model.NodeCreationRelations g (FormMessage >> dispatch)
                                                ViewGen.makeNodeFormWithRelations<Exposure.TemporalIndex.QualitativeLabelNode> (fun savedRelations ->
                                                    (ViewGen.RelationsForms.Validation.hasOne (GraphStructure.ProposedRelation.Exposure Exposure.ExposureRelation.LatestTime) savedRelations) &&
                                                        (ViewGen.RelationsForms.Validation.hasOne (GraphStructure.ProposedRelation.Exposure Exposure.ExposureRelation.EarliestTime) savedRelations))
                                                    (model.NodeCreationViewModels |> Map.tryFind "QualitativeLabelNode") [] (FormMessage >> dispatch)
                                            ]
                                        | None -> empty
                                    ]
                                ]
                            ]
                        | Page.Outcome -> div [] [ text "There are no options to display" ]
                        | Page.Statistics -> concat [
                                h2 [] [ text "Statistics" ]
                                hr []
                                p [] [ text "Contains various statistics about the compiled graph database." ]
                                alert model dispatch
                                cond model.Statistics <| function
                                | Some stats ->
                                    div [ _class "card mb-4" ] [
                                        div [ _class "card-header text-bg-primary" ] [ text "Sources - Statistics" ]
                                        div [ _class "card-body" ] [
                                            dl [ _class "row" ] [
                                                dt [ _class "col-sm-3" ] [ text "Generated at" ]
                                                dd [ _class "col-sm-9" ] [ textf "%A" stats.TimeGenerated ]
                                                dt [ _class "col-sm-3" ] [ text "Included bibliographic sources" ]
                                                dd [ _class "col-sm-9" ] [ textf "%A" stats.IncludedSources ]
                                                dt [ _class "col-sm-3" ] [ text "Excluded bibliographic sources" ]
                                                dd [ _class "col-sm-9" ] [ textf "%A" stats.ExcludedSources ]
                                                dt [ _class "col-sm-3" ] [ text "Included secondary bibliographic sources" ]
                                                dd [ _class "col-sm-9" ] [ textf "%A" stats.SecondarySources ]
                                                dt [ _class "col-sm-3" ] [ text "Included primary bibliographic sources" ]
                                                dd [ _class "col-sm-9" ] [ textf "%A" stats.PrimarySources ]
                                                dt [ _class "col-sm-3" ] [ text "Screened but not data coded" ]
                                                dd [ _class "col-sm-9" ] [ textf "%A" stats.ScreenedNotCoded ]
                                                dt [ _class "col-sm-3" ] [ text "Data coding completed for bibliographic sources" ]
                                                dd [ _class "col-sm-9" ] [ textf "%A" stats.CompleteSources ]
                                            ]
                                        ]
                                    ]
                                | None -> empty
                            ]
                        | Page.Sources -> concat [
                                h2 [] [ text "Sources Manager" ]
                                hr []
                                p [] [ text "Sources may be imported in bulk from Colandr screening, or added individually." ]
                                alert model dispatch
                                h3 [] [ text "Add an individual source" ]
                                hr []

                                div [ _class "card mb-4" ] [
                                    div [ _class "card-header text-bg-secondary" ] [ text "CrossRef lookup" ]
                                    div [ _class "card-body" ] [
                                        p [] [ text "You may use CrossRef to attempt to find the full reference for a source. If the source is found by CrossRef, you can quickly import it into BiodiversityCoder without manual data entry." ]
                                        input [ _class "form-control form-control-sm"; bind.input.string model.CrossRefLookupModel.SearchTerm (EnterCrossRefSearch >> dispatch) ]
                                        cond model.CrossRefLookupModel.Match <| function
                                        | Some m -> concat [
                                            sourceDetailsView m
                                            button [ _class "btn btn-secondary"; on.click (fun _ -> AddCrossRefMatch |> dispatch)] [ text "Confirm (add this source)" ]
                                            button [ _class "btn btn-secondary"; on.click (fun _ -> ClearCrossRef |> dispatch)] [ text "Clear (cancel)" ] ]
                                        | None ->
                                            button [ _class "btn btn-primary"; on.click(fun _ -> model.CrossRefLookupModel.SearchTerm |> SearchCrossRef |> dispatch) ] [ text "Try to match" ]
                                    ]
                                ]
                                
                                div [ _class "card mb-4" ] [
                                    div [ _class "card-header text-bg-secondary" ] [ text "Add a source manually" ]
                                    div [ _class "card-body" ] [
                                        p [] [ text "Manually add a record for a published, grey literature, or 'dark data' source. Published sources include standard bibliographic texts such as journal articles, books and book chapters, and theses." ]
                                        p [] [ text "Grey literature. A common working definition of 'grey literature' is: 'information produced on all levels of government, academia, business and industry in electronic and print formats not controlled by commercial publishing ie. where publishing is not the primary activity of the producing body.'" ]
                                        p [] [ text "Dark Data. 'Dark data are datasets where - unlike grey literature - there has been no 'write-up' of the results. For example, data sat on a floppy disk in someone's drawer." ]
                                        p [] [ strong [] [ text "NOTE: Do NOT use the last three options - Bibliographic, GreyLiterature, DarkData - as these have been depreciated." ] ]
                                        ViewGen.makeNodeForm<Sources.Source> (model.NodeCreationViewModels |> Map.tryFind "Source") [] (FormMessage >> dispatch)
                                    ]
                                ]

                                div [ _class "card mb-4" ] [
                                    div [ _class "card-header text-bg-secondary" ] [ text "Advanced bulk import options (use with caution)" ]
                                    div [ _class "card-body" ] [
                                        h5 [] [ text "Import using BibTex format"]
                                        hr []
                                        text "Enter the contents of a bibtex-format file in the text box below to import many sources at once, for example from a reference manager exported file."
                                        br []
                                        textarea [ bind.input.string model.Import (fun s -> ChangeImportText s |> dispatch) ] []
                                        button [ on.click (fun _ -> ImportBibtex |> dispatch ) ] [ text "Import" ]
                                        h5 [] [ text "Import from a Colandr export file" ]
                                        hr []
                                        p [] [ text "You can import sources from Colandr using the button below. The colandr raw output should be saved as 'colandr-titleabs-screen-results.csv' in the graph database ('data') folder." ]
                                        button [ _class "btn btn-primary"; on.click (fun _ -> ImportColandr |> dispatch) ] [ text "Import from Colandr (title-abstract screening)" ]
                                    ]
                                ]
                            ]

                        | Page.Extract -> div [] [

                            h2 [] [ text "Data Coding" ]
                            p [] [ text "This tool allows coding information from bibliographic sources directly into a graph database." ]

                            cond model.Graph <| function
                            | None -> concat [
                                alert model dispatch
                                // No graph is loaded. Connect to a graph folder.
                                p [] [ text "To get started, please connect the data coding tool to the folder where you are storing the graph database files." ]
                                label [] [ text "Where is the graph database stored? Enter the folder location here:" ]
                                input [ _class "form-control"; bind.input.string model.FolderLocation (fun s -> SetFolderManually s |> dispatch) ]
                                button [ _class "btn btn-primary"; on.click(fun _ -> SelectedFolder model.FolderLocation |> dispatch) ] [ text "Set folder" ]
                                button [ _class "btn btn-primary"; attr.disabled "disabled"; on.click (fun _ -> SelectFolder |> dispatch)] [ text "Select folder to connect to." ] ]
                            | Some g -> concat [
                                
                                div [ _class "card mb-4 text-bg-secondary" ] [
                                    div [ _class "card-header text-bg-secondary" ] [ text "Source Details" ]
                                    div [ _class "card-body" ] [
                                        text "Selected source:"
                                        cond model.SelectedSource <| function
                                        | Some s ->
                                            select [ _class "form-select"; bind.change.string ((s.SelectedSource |> fst |> fst).AsString) (fun k -> SelectSource (Graph.stringToKey k) |> dispatch) ] [(
                                                cond (g.Nodes<Sources.SourceNode>()) <| function
                                                | Some sources ->
                                                    sources
                                                    |> Seq.filter(fun k -> if model.SourceFilter <> "" then System.Text.RegularExpressions.Regex.IsMatch(k.Value, model.SourceFilter) else true)
                                                    |> Seq.map(fun k ->
                                                        option [ attr.value k.Key.AsString ] [ text k.Value ])
                                                    |> Seq.append [ option [ attr.disabled true; attr.value "NONE SELECTED" ] [ text "-- select a source --" ] ]
                                                    |> Seq.toList
                                                    |> concat
                                                | None -> empty
                                            )]
                                        | None ->
                                            select [ _class "form-select"; bind.change.string "" (fun k -> SelectSource (Graph.stringToKey k) |> dispatch) ] [(
                                                cond (g.Nodes<Sources.SourceNode>()) <| function
                                                | Some sources ->
                                                    sources
                                                    |> Seq.filter(fun k -> if model.SourceFilter <> "" then System.Text.RegularExpressions.Regex.IsMatch(k.Value, model.SourceFilter) else true)
                                                    |> Seq.map(fun k ->
                                                        option [ attr.value k.Key.AsString ] [ text k.Value ])
                                                    |> Seq.append [ option [ attr.disabled true; attr.value "NONE SELECTED" ] [ text "-- select a source --" ] ]
                                                    |> Seq.toList
                                                    |> concat
                                                | None -> empty
                                            )]
                                        input [ _class "form-control"; attr.placeholder "Filter... (accepts regex)";
                                            bind.input.string model.SourceFilter (FilterSourceList >> dispatch) ]
                                    ]
                                ]

                                alert model dispatch

                                cond model.SelectedSource <| function
                                | None -> div [ _class "alert alert-info" ] [ text "Select a source to continue" ]
                                | Some source -> concat [

                                    // Allow coding if included, or show exclusion reasons / options.
                                    cond (source.SelectedSource |> fst |> snd) <| function
                                    | GraphStructure.Node.SourceNode sn ->
                                        cond sn <| function
                                        | Sources.SourceNode.Unscreened s -> 
                                            concat [
                                                // Full-text screening options.
                                                div [ _class "card mb-4" ] [
                                                    div [ _class "card-header text-bg-secondary" ] [ text "Q: Is the source relevant?" ]
                                                    div [ _class "card-body" ] [
                                                        p [] [ text "Please apply the eligbility criteria against the full-text PDF of this source and determine if the source should be included or excluded." ]
                                                        ViewGen.makeNodeForm'<EligbilityCriteria> (Some source.Screening) "Screen" (FormMessage >> dispatch) (fun _ -> true) []
                                                    ]
                                                ]
                                                div [ _class "alert alert-info" ] [ text "Screen this source to continue coding." ]
                                            ]
                                        | Sources.SourceNode.Excluded (s,reason,notes) ->
                                            div [ _class "alert alert-success" ] [ 
                                                text "This source has been excluded at full-text level."
                                                textf "The reason stated was: %s" (reason.ToString())
                                                text notes.Value ]
                                        | Sources.SourceNode.Included (s,codingStatus) -> concat [
                                            div [ _class "card mb-4" ] [
                                                div [ _class "card-header text-bg-secondary" ] [ text "Source Status: Included" ]
                                                div [ _class "card-body" ] [
                                                    div [ _class "alert alert-success" ] [ text "This source has been included at full-text level. Please code information as stated below." ]
                                                    codingStatusView codingStatus
                                                ]
                                            ]
                                            div [ _class "card mb-4" ] [
                                                div [ _class "card-header text-bg-secondary" ] [ text "Q: Is it a primary or secondary source?" ]
                                                div [ _class "card-body" ] [
                                                    p [] [ 
                                                        text "A source may be 'secondary' if it does not contain any new information, but references information in other publications."
                                                        text "You can link this source to the primary sources by selecting an existing source, or adding a new one. Please check that the source does not already exist before creating a new one." ]
                                                    cond (isCompletedSection "source-primary-or-secondary" codingStatus) <| function
                                                    | true -> concat [
                                                        div [ _class "alert alert-success" ] [ strong [] [ text "Thank you! " ]; text "You've finished the primary / secondary source question." ]
                                                        div [ _class "row g-3" ] [
                                                            div [ _class "col-md-3" ] [
                                                                label [] [ text "Linked sources" ]
                                                            ]
                                                            div [ _class "col-md-6" ] [
                                                                forEach (source.SelectedSource |> GraphStructure.Relations.nodeIdsByRelation<Sources.SourceRelation> Sources.SourceRelation.UsesPrimarySource |> Storage.atomsByKey g ) <| fun linkedSource ->
                                                                    p [] [ textf "%A" (linkedSource |> fst |> snd) ]
                                                            ]
                                                        ] ]
                                                    | false ->
                                                        cond source.MarkedPrimary <| function
                                                        | Unknown ->
                                                            concat [
                                                                div [ _class "row g-3" ] [
                                                                    div [ _class "col-md-3" ] [
                                                                        label [] [ text "Is this a primary source?" ]
                                                                    ]
                                                                    div [ _class "col-md-9" ] [
                                                                        button [ _class "btn btn-secondary mr-2"; on.click(fun _ -> MarkPrimary Primary |> dispatch) ] [ text "Primary source" ]
                                                                        button [ _class "btn btn-secondary"; on.click(fun _ -> MarkPrimary Secondary |> dispatch) ] [ text "Secondary source" ]
                                                                    ]
                                                                ]
                                                            ]
                                                        | Primary
                                                        | Secondary ->
                                                            cond source.AddingNewSource <| function
                                                            | false -> 
                                                                concat [
                                                                    div [ _class "row g-3" ] [
                                                                        div [ _class "col-md-3" ] [
                                                                            label [] [ text "Is this a primary source?" ]
                                                                        ]
                                                                        div [ _class "col-md-9" ] [ 
                                                                                cond source.MarkedPrimary <| function
                                                                                | Primary -> text "Yes. This source is a primary source, but may also be linked to other sources."
                                                                                | Secondary -> text "No. This is only a secondary source."
                                                                                | _ -> empty
                                                                            ]
                                                                    ]
                                                                    div [ _class "row g-3" ] [
                                                                        div [ _class "col-md-3" ] [
                                                                            label [] [ text "Linked sources" ]
                                                                        ]
                                                                        div [ _class "col-md-6" ] [
                                                                            forEach (source.SelectedSource |> GraphStructure.Relations.nodeIdsByRelation<Sources.SourceRelation> Sources.SourceRelation.UsesPrimarySource |> Storage.atomsByKey g ) <| fun linkedSource ->
                                                                                p [] [ textf "%A" (linkedSource |> fst |> snd) ]
                                                                        ]
                                                                    ]
                                                                    cond (isCompletedSection "source-primary-or-secondary" codingStatus) <| function
                                                                    | true -> div [ _class "alert alert-success" ] [ strong [] [ text "Thank you! " ]; text "You've finished the primary / secondary source question." ]
                                                                    | false -> concat [
                                                                        div [ _class "row g-3" ] [
                                                                            div [ _class "col-md-3" ] [
                                                                                label [] [ text "Link to another primary source" ]
                                                                            ]
                                                                            div [ _class "col-md-6" ] [
                                                                                select [ _class "form-select"; bind.change.string (if source.ProposedLink.IsSome then source.ProposedLink.Value.AsString else "") 
                                                                                    (fun s -> s |> Graph.stringToKey |> Some |> ChangeProposedSourceLink |> dispatch) ] [ ViewGen.optionGenFiltered<Sources.SourceNode> (fun k -> k.AsString.Split("_").[1] <> "database") model.Graph ]
                                                                            ]
                                                                            div [ _class "col-md-3" ] [
                                                                                button [ attr.disabled source.ProposedLink.IsNone; _class "btn btn-primary"; on.click(
                                                                                    fun _ -> RelateNodes(
                                                                                        source.SelectedSource |> fst |> fst, 
                                                                                        source.ProposedLink.Value, 
                                                                                        Sources.SourceRelation.UsesPrimarySource |> GraphStructure.ProposedRelation.Source) 
                                                                                            |> FormMessage |> dispatch; ChangeProposedSourceLink None |> dispatch) ] [ text "Link" ]
                                                                            ]
                                                                        ]
                                                                        div [ _class "row g-3" ] [
                                                                            div [ _class "col-md-3" ] [
                                                                                label [] [ text "...or add a source not yet listed" ]
                                                                            ]
                                                                            div [ _class "col-md-6" ] []
                                                                            div [ _class "col-md-3" ] [
                                                                                em [] [ text "To add new sources, use the 'Sources' tab to the left." ]
                                                                                // button [ _class "btn btn-primary"; on.click(fun _ -> ToggleConnectNewOrExistingSource |> dispatch) ] [ text "Add a new source" ]
                                                                            ]
                                                                        ]
                                                                        div [ _class "row g-3" ] [
                                                                            div [ _class "col-md-3" ] [
                                                                                label [] [ text "This source uses an existing database" ]
                                                                            ]
                                                                            cond source.ProposedDatabaseLink <| function
                                                                            | Some link -> concat [
                                                                                div [ _class "col-md-6" ] [
                                                                                    select [ _class "form-select"; bind.change.string (if link.Id.IsSome then link.Id.Value.AsString else "") 
                                                                                        (fun s -> { link with Id = s |> Graph.stringToKey |> Some } |> Some |> ChangeProposedSourceDatabaseLink |> dispatch) ] [ ViewGen.optionGenFiltered<Sources.SourceNode> (fun k -> k.AsString.Split("_").Length = 3 && k.AsString.Split("_").[1] = "database") model.Graph ]
                                                                                    div [ _class "mb-3" ] [
                                                                                        label [ _class "form-label" ] [ text "Date accessed" ]
                                                                                        input [ _class "form-control"; bind.input.string link.AccessDate (fun d -> { link with AccessDate = d } |> Some |> ChangeProposedSourceDatabaseLink |> dispatch) ]
                                                                                        small [ _class "form-text" ] [ text "Enter the date in the format YYYY-MM-DD" ]
                                                                                    ]
                                                                                    select [ _class "form-select"; bind.change.string link.AccessMethod
                                                                                        (fun s -> { link with AccessMethod = s } |> Some |> ChangeProposedSourceDatabaseLink |> dispatch) ] [
                                                                                            option [ attr.value "all" ] [ text "All records from database (within scope of study)" ]
                                                                                            option [ attr.value "specific" ] [ text "Some specific records (by unique IDs)" ]
                                                                                            option [ attr.value "complex" ] [ text "A specific method was used to derive a subset" ]
                                                                                        ]                                                                                    
                                                                                    cond (link.AccessMethod = "complex") <| function
                                                                                    | true -> div [ _class "mb-3" ] [
                                                                                        label [ _class "form-label" ] [ text "Method used" ]
                                                                                        textarea [ _class "form-input"; bind.input.string link.AccessDetails (fun d -> { link with AccessDetails = d } |> Some |>ChangeProposedSourceDatabaseLink |> dispatch) ] []
                                                                                        small [ _class "form-text" ] [ text "Briefly describe the method used to select records (e.g. the geographic or quality criteria)." ] ]
                                                                                    | false ->
                                                                                        cond (link.AccessMethod = "specific") <| function
                                                                                        | true -> div [ _class "mb-3" ] [
                                                                                            label [ _class "form-label" ] [ text "List of specific IDs" ]
                                                                                            textarea [ _class "form-input"; bind.input.string link.AccessDetails (fun d -> { link with AccessDetails = d } |> Some |> ChangeProposedSourceDatabaseLink |> dispatch) ] []
                                                                                            small [ _class "form-text" ] [ text "Enter a list of specific IDs of records used from this database. Seperate each item with a line break." ] ]
                                                                                        | false -> empty
                                                                                ]
                                                                                div [ _class "col-md-3" ] [
                                                                                    cond (asDatabaseLink link) <| function
                                                                                    | Some l ->
                                                                                        button [ attr.disabled link.Id.IsNone; _class "btn btn-primary"; on.click(
                                                                                            fun _ -> RelateNodes(
                                                                                                source.SelectedSource |> fst |> fst, 
                                                                                                link.Id.Value,
                                                                                                l |> GraphStructure.ProposedRelation.Source) 
                                                                                                    |> FormMessage |> dispatch; ChangeProposedSourceDatabaseLink None |> dispatch) ] [ text "Link" ]
                                                                                    | None -> button [ attr.disabled true; _class "btn btn-primary" ] [ text "Link" ]
                                                                                ]]
                                                                            | None ->
                                                                                div [ _class "col-md-6" ] [
                                                                                    select [ _class "form-select"; bind.change.string ""
                                                                                        (fun s -> {Id = s |> Graph.stringToKey |> Some; AccessDate = ""; AccessMethod = "all"; AccessDetails = "" } |> Some |> ChangeProposedSourceDatabaseLink |> dispatch) ] [ ViewGen.optionGenFiltered<Sources.SourceNode> (fun k -> k.AsString.Split("_").Length = 3 && k.AsString.Split("_").[1] = "database") model.Graph ]
                                                                                ]
                                                                        ]
                                                                        hr []
                                                                        button [ _class "btn btn-primary"; on.click (fun _ -> CompleteSection "source-primary-or-secondary" |> dispatch) ] [ text "I've finished this question" ]
                                                                    ]
                                                                ]
                                                            | true -> 
                                                                concat [
                                                                    ViewGen.makeNodeForm<Sources.SourceNode> (model.NodeCreationViewModels |> Map.tryFind "SourceNode") [
                                                                        ThisIsSink ((source.SelectedSource |> fst |> fst), GraphStructure.ProposedRelation.Source(Sources.SourceRelation.UsesPrimarySource))
                                                                    ] (FormMessage >> dispatch)
                                                                    button [ _class "btn btn-danger"; on.click(fun _ -> ToggleConnectNewOrExistingSource |> dispatch) ] [ text "Back (discard)" ]
                                                                ]
                                                    ]
                                            ] // end primary / secondary card.

                                            div [ _class "card mb-4" ] [
                                                div [ _class "card-header text-bg-secondary" ] [ text "Q: How are the study timeline(s) formed?" ]
                                                div [ _class "card-body" ] [
                                                        div [ _class "row" ] [
                                                            div [ _class "col-md-12" ] [
                                                                figure [ _class "figure w-50 float-end m-3" ] [ img [ _class "figure-img img-fluid rounded img-thumbnail"; attr.src "images/exposure-diagram-dates.png" ] ]
                                                                p [] [ 
                                                                    text "A study timeline is a continuous or discontinuous time-sequence over which biodiversity measures for biotic proxies are specified."
                                                                    text "Individual study timelines have their own spatial contexts. Therefore, time-series from different spatial locations should be classed as seperate time-series (e.g. multiple sediment cores)."
                                                                    text "If there are many points in a spatial area - for example individual tree-ring series - but the species are the same, you should specify a single timeline but attach a broader (e.g. regional) spatial context." ]
                                                            ]
                                                        ]
                                                        // View an individual temporal extent.
                                                        forEach (source.SelectedSource |> GraphStructure.Relations.nodeIdsByRelation<Sources.SourceRelation> Sources.SourceRelation.HasTemporalExtent |> Storage.atomsByKey g ) <| fun timelineAtom ->
                                                            div [ _class "card mb-4" ] [
                                                                div [ _class "card-header" ] [ text ((timelineAtom |> fst |> snd).DisplayName()) ]
                                                                div [ _class "card-body" ] [
                                                                    timelineAtomDetailsView timelineAtom g
                                                                    cond (isCompletedSection "exposure" codingStatus) <| function
                                                                    | true -> empty
                                                                    | false ->
                                                                        concat [
                                                                            // Add a single spatial context or display it here.
                                                                            cond (timelineAtom |> GraphStructure.Relations.nodeIdsByRelation<Exposure.ExposureRelation> Exposure.ExposureRelation.IsLocatedAt |> Seq.tryHead |> Option.map (fun k -> Storage.atomByKey k g) ) <| function
                                                                            | Some _ -> p [ _class "text-success" ] [ text "You've added the spatial context." ]
                                                                            | None -> 
                                                                                concat [
                                                                                    div [ _class "alert alert-warning" ] [ text "You need to add a spatial context to this timeline." ]
                                                                                    p [] [ 
                                                                                        text "Specify a single spatial context for the timeline."
                                                                                        text "Individual study timelines have their own spatial contexts. Therefore, time-series from different spatial locations should be classed as seperate time-series (e.g. multiple sediment cores)."
                                                                                    ]
                                                                                    div [ _class "card mb-4"] [
                                                                                        div [ _class "card-header text-bg-warning" ] [ text "Required: Define the timeline's spatial context" ]
                                                                                        div [ _class "card-body" ] [
                                                                                            ViewGen.makeNodeForm<Population.Context.ContextNode> (model.NodeCreationViewModels |> Map.tryFind "ContextNode") [
                                                                                                ThisIsSink ((timelineAtom |> fst |> fst), Exposure.ExposureRelation.IsLocatedAt |> GraphStructure.ProposedRelation.Exposure)
                                                                                            ] (FormMessage >> dispatch)
                                                                                        ]
                                                                                    ]
                                                                                ]

                                                                            // Add dates and display them here.
                                                                            cond (timelineAtom |> GraphStructure.Relations.nodeIdsByRelation<Exposure.ExposureRelation> Exposure.ExposureRelation.ConstructedWithDate |> Seq.tryHead) <| function
                                                                            | Some _ -> empty
                                                                            | None -> div [ _class "alert alert-warning" ] [ strong [] [ text "Dating method. " ]; text "You should enter at least one dating measurement here." ]
                                                                            concat [
                                                                                p [] [ 
                                                                                    text "Describe the individul dates used to form this individual time-series. For example, these may be radiocarbon dates for a sedimentary sequence. "
                                                                                    text "For living specimen data (e.g. tree-ring data), use the 'Collection Date' option to specify the time of sampling (that forms the origin point of the time-series - e.g. growth year zero)." ]
                                                                                div [ _class "card mb-4"] [
                                                                                    div [ _class "card-header" ] [ text "Add a date" ]
                                                                                    div [ _class "card-body" ] [
                                                                                        ViewGen.RelationsForms.relationsToggle<Exposure.StudyTimeline.IndividualDateNode> "Expression of time" [
                                                                                            ("Zone or Qualitative", [
                                                                                                ViewGen.RelationsForms.selectExistingNode<Exposure.TemporalIndex.QualitativeLabelNode> "Intersects this time period" "Use if the dating method indicates a connection to a qualitative period. Example: peat deposit occurs within a locally-defined pollen zone." (Exposure.ExposureRelation.OccursWithin |> GraphStructure.ProposedRelation.Exposure)
                                                                                            ])
                                                                                            ("Absolute", [
                                                                                                ViewGen.RelationsForms.selectExistingBy<Exposure.TemporalIndex.CalYearNode, FieldDataTypes.OldDate.OldDateSimple> "Estimated date (as stated by the authors)" "You may select a date in one of three units: BP, cal yr BP, or AD/BC (calendar year)." (Exposure.ExposureRelation.TimeEstimate >> GraphStructure.ProposedRelation.Exposure) (NodeSelection.trySelectTimeNode g) model.RelationCreationViewModels
                                                                                                // ViewGen.RelationsForms.selectExistingBy<Exposure.TemporalIndex.CalYearNode, FieldDataTypes.OldDate.OldDateSimple> "Uncertainty: earliest date (as stated by the authors)" "Optional. If an uncertainty range has been given for the date, enter the earliest stated date here." (Exposure.ExposureRelation.UncertaintyOldest >> GraphStructure.ProposedRelation.Exposure) (NodeSelection.trySelectTimeNode g) model.RelationCreationViewModels
                                                                                                // ViewGen.RelationsForms.selectExistingBy<Exposure.TemporalIndex.CalYearNode, FieldDataTypes.OldDate.OldDateSimple> "Uncertainty: latest date (as stated by the authors)" "Optional. Enter the latest stated date here." (Exposure.ExposureRelation.UncertaintyYoungest >> GraphStructure.ProposedRelation.Exposure) (NodeSelection.trySelectTimeNode g) model.RelationCreationViewModels
                                                                                            ])
                                                                                            ("Pre-Holocene", [
                                                                                                ViewGen.RelationsForms.selectExistingNode<Exposure.TemporalIndex.QualitativeLabelOutOfScopeNode> "Intersects this time period" "Use if the dating method indicates a connection to a qualitative period. Example: peat deposit occurs within a locally-defined pollen zone." (Exposure.ExposureRelation.OccursOutOfScope |> GraphStructure.ProposedRelation.Exposure)
                                                                                            ])
                                                                                        ] model.NodeCreationRelations g (FormMessage >> dispatch)
                                                                                        ViewGen.makeNodeFormWithRelations<Exposure.StudyTimeline.IndividualDateNode> (fun savedRelations ->
                                                                                            (ViewGen.RelationsForms.Validation.hasOne (GraphStructure.ProposedRelation.Exposure Exposure.ExposureRelation.OccursWithin) savedRelations)
                                                                                            || (ViewGen.RelationsForms.Validation.hasOneByCase "TimeEstimate" savedRelations)
                                                                                        ) (model.NodeCreationViewModels |> Map.tryFind "IndividualDateNode") [
                                                                                            ThisIsSink ((timelineAtom |> fst |> fst), Exposure.ExposureRelation.ConstructedWithDate |> GraphStructure.ProposedRelation.Exposure)
                                                                                        ] (FormMessage >> dispatch)
                                                                                    ]
                                                                                ]
                                                                            ]
                                                                    ]

                                                                ]
                                                            ] // end viewing of a temporal extent.
                                                        hr []
                                                        cond (isCompletedSection "exposure" codingStatus) <| function
                                                        | true -> div [ _class "alert alert-success" ] [ strong [] [ text "Thank you! " ]; text "You've finished the timelines section." ]
                                                        | false -> concat [
                                                            // Add another temporal extent.
                                                            div [ _class "card mb-4" ] [
                                                                div [ _class "card-header" ] [ text "Add an additional timeline" ]
                                                                div [ _class "card-body" ] [
                                                                    p [] [ text "When defining a timeline by start and end dates, you may optionally enter uncertainty values. These are not required if not available." ]
                                                                    ViewGen.RelationsForms.relationsToggle<Exposure.StudyTimeline.IndividualTimelineNode> "Expression of time" [
                                                                        ("Qualitative", [
                                                                            ViewGen.RelationsForms.selectExistingNodeMulti<Exposure.TemporalIndex.QualitativeLabelNode> "Intersects this time period" "" (Exposure.ExposureRelation.IntersectsTime |> GraphStructure.ProposedRelation.Exposure)
                                                                        ])
                                                                        ("Year (within Holocene)", [
                                                                            ViewGen.RelationsForms.selectExistingBy<Exposure.TemporalIndex.CalYearNode, FieldDataTypes.OldDate.OldDateSimple> "Temporal extent: youngest date uncertainty (younger bound)" "" (fun _ -> Exposure.ExposureRelation.ExtentLatestUncertainty |> GraphStructure.ProposedRelation.Exposure) (NodeSelection.trySelectTimeNode g) model.RelationCreationViewModels
                                                                            ViewGen.RelationsForms.selectExistingBy<Exposure.TemporalIndex.CalYearNode, FieldDataTypes.OldDate.OldDateSimple> "Temporal extent: youngest date" "Required." (fun d -> Exposure.ExposureRelation.ExtentLatestSpecified (Some d) |> GraphStructure.ProposedRelation.Exposure) (NodeSelection.trySelectTimeNode g) model.RelationCreationViewModels
                                                                            ViewGen.RelationsForms.selectExistingBy<Exposure.TemporalIndex.CalYearNode, FieldDataTypes.OldDate.OldDateSimple> "Temporal extent: youngest date uncertainty (older bound)" "" (fun _ -> Exposure.ExposureRelation.ExtentLatestUncertainty |> GraphStructure.ProposedRelation.Exposure) (NodeSelection.trySelectTimeNode g) model.RelationCreationViewModels
                                                                            ViewGen.RelationsForms.selectExistingBy<Exposure.TemporalIndex.CalYearNode, FieldDataTypes.OldDate.OldDateSimple> "Temporal extent: oldest date uncertainty (younger bound)" "" (fun _ -> Exposure.ExposureRelation.ExtentEarliestUncertainty |> GraphStructure.ProposedRelation.Exposure) (NodeSelection.trySelectTimeNode g) model.RelationCreationViewModels
                                                                            ViewGen.RelationsForms.selectExistingBy<Exposure.TemporalIndex.CalYearNode, FieldDataTypes.OldDate.OldDateSimple> "Temporal extent: oldest date" "Required." (fun d -> Exposure.ExposureRelation.ExtentEarliestSpecified (Some d) |> GraphStructure.ProposedRelation.Exposure) (NodeSelection.trySelectTimeNode g) model.RelationCreationViewModels
                                                                            ViewGen.RelationsForms.selectExistingBy<Exposure.TemporalIndex.CalYearNode, FieldDataTypes.OldDate.OldDateSimple> "Temporal extent: oldest date uncertainty (older bound)" "" (fun _ -> Exposure.ExposureRelation.ExtentEarliestUncertainty |> GraphStructure.ProposedRelation.Exposure) (NodeSelection.trySelectTimeNode g) model.RelationCreationViewModels
                                                                        ])
                                                                        ("Year (goes older than Holocene)", [
                                                                            ViewGen.RelationsForms.selectExistingBy<Exposure.TemporalIndex.CalYearNode, FieldDataTypes.OldDate.OldDateSimple> "Temporal extent: youngest date uncertainty (younger bound)" "" (fun _ -> Exposure.ExposureRelation.ExtentLatestUncertainty |> GraphStructure.ProposedRelation.Exposure) (NodeSelection.trySelectTimeNode g) model.RelationCreationViewModels
                                                                            ViewGen.RelationsForms.selectExistingBy<Exposure.TemporalIndex.CalYearNode, FieldDataTypes.OldDate.OldDateSimple> "Temporal extent: youngest date" "Required." (fun d -> Exposure.ExposureRelation.ExtentLatestSpecified (Some d) |> GraphStructure.ProposedRelation.Exposure) (NodeSelection.trySelectTimeNode g) model.RelationCreationViewModels
                                                                            ViewGen.RelationsForms.selectExistingBy<Exposure.TemporalIndex.CalYearNode, FieldDataTypes.OldDate.OldDateSimple> "Temporal extent: youngest date uncertainty (older bound)" "" (fun _ -> Exposure.ExposureRelation.ExtentLatestUncertainty |> GraphStructure.ProposedRelation.Exposure) (NodeSelection.trySelectTimeNode g) model.RelationCreationViewModels
                                                                            ViewGen.RelationsForms.selectExistingBy<Exposure.TemporalIndex.QualitativeLabelOutOfScopeNode, FieldDataTypes.OldDate.OldDateSimple> "Temporal extent: oldest date (pre-Holocene)" "" (Exposure.ExposureRelation.ExtentEarliestOutOfScope >> GraphStructure.ProposedRelation.Exposure) (trySelectPreHoloceneScope g) model.RelationCreationViewModels])                                                                    
                                                                    ] model.NodeCreationRelations g (FormMessage >> dispatch)
                                                                    ViewGen.makeNodeFormWithRelations<Exposure.StudyTimeline.IndividualTimelineNode> (fun savedRelations ->
                                                                        (ViewGen.RelationsForms.Validation.hasOneByCase "ExtentEarliest" savedRelations 
                                                                        && ViewGen.RelationsForms.Validation.hasOneByCase "ExtentLatest" savedRelations)
                                                                        || (ViewGen.RelationsForms.Validation.hasOneByCase "ExtentLatest" savedRelations 
                                                                        && ViewGen.RelationsForms.Validation.hasOneByCase "ExtentEarliestOutOfScope" savedRelations)
                                                                        || ViewGen.RelationsForms.Validation.hasOne (GraphStructure.ProposedRelation.Exposure Exposure.ExposureRelation.IntersectsTime) savedRelations
                                                                    ) (model.NodeCreationViewModels |> Map.tryFind "IndividualTimelineNode") [
                                                                        ThisIsSink ((source.SelectedSource |> fst |> fst), GraphStructure.ProposedRelation.Source(Sources.SourceRelation.HasTemporalExtent))
                                                                    ] (FormMessage >> dispatch)
                                                                ]
                                                            ]
                                                            hr []
                                                            button [ _class "btn btn-primary"; on.click (fun _ -> CompleteSection "exposure" |> dispatch) ] [ text "I've finished coding timelines" ]
                                                        ]
                                                    ]
                                            ] // end timelines card.

                                            div [ _class "card mb-4" ] [
                                                div [ _class "card-header text-bg-secondary" ] [ text "Q: Which biodiversity outcomes are associated with the timelines in this source?" ]
                                                div [ _class "card-body" ] [
                                                    div [ _class "row" ] [
                                                        div [ _class "col-md-12" ] [
                                                            figure [ _class "figure w-50 float-end m-3" ] [ img [ _class "figure-img img-fluid rounded img-thumbnail"; attr.src "images/population-diagram-hyperedge.png" ] ]
                                                            p [] [
                                                                text "Each temporal-spatial context (as specified above) may have one or more biodiversity outcomes associated with it."
                                                                text "A biodiversity outcome here means a combination of a biotic proxy (proxy -> inference method -> taxon) and an outcome measurement."
                                                                text "For example, an outcome may be 'abundance', and the biotic proxy may be [ acacia-type (pollen morphotype) -> African pollen atlas -> Acacia (genus) ]"
                                                            ]
                                                            p [] [
                                                                text "If a taxon is not present that you need, you can use the taxonomic lookup (requires internet connection) to create new taxon nodes."
                                                                text "Similarly, if you need a new biotic proxy node or inference node, make these manually in the 'Population' tab to the left."
                                                            ]
                                                        ]
                                                    ]

                                                    cond (isCompletedSection "outcome" codingStatus) <| function
                                                    | true -> div [ _class "alert alert-success" ] [ strong [] [ text "Thank you! " ]; text "You've finished the outcomes section." ]
                                                    | false -> concat [
                                                        // 3. For each study timeline, add proxied taxa.
                                                        forEach (source.SelectedSource |> GraphStructure.Relations.nodeIdsByRelation<Sources.SourceRelation> Sources.SourceRelation.HasTemporalExtent |> Storage.atomsByKey g ) <| fun timelineAtom ->
                                                            // Display existing and add new proxied taxa
                                                            // Proxied taxa are related to an outcome too.
                                                            concat [
                                                                h4 [] [ text ((timelineAtom |> fst |> snd).DisplayName())]
                                                                hr []
                                                                timelineAtomDetailsView timelineAtom g
                                                                cond (source.AddBioticHyperedgeBatch |> Map.tryFind (timelineAtom |> fst |> fst)) <| function
                                                                | Some batch -> div [ _class "card border-info mb-4 mt-4"] [
                                                                    div [ _class "card-header text-dark bg-info" ] [ text "You are using batch mode" ]
                                                                    div [ _class "card-body" ] [
                                                                        // Add 'auto-fill' boxes for batch mode
                                                                        p [] [ text "In 'batch mode', you can more quickly add many microfossil proxy records at the same time (e.g. pollen, plant macrofossils, diatoms). Use the autofill boxes below to apply a common inference method and outcome measure across multiple biotic proxies." ]
                                                                        div [ _class "row" ] [
                                                                            div [ _class "col-md-3" ] [
                                                                                label [ _class "form-label" ] [ text "Microfossil group" ]
                                                                                select [ _class "form-select form-select-sm"; bind.change.string (batch.Group.ToString()) (fun s -> SetBatchModeAutofill((timelineAtom |> fst |> fst), asMicrofossilGroup s,batch.InferenceMethod,batch.Outcome,batch.UsePlaceholderTaxon, batch.UsePlaceholderInfer) |> dispatch) ] [
                                                                                    option [ attr.value Population.BioticProxies.MicrofossilGroup.Pollen ] [ text "Pollen" ]
                                                                                    option [ attr.value Population.BioticProxies.MicrofossilGroup.PlantMacrofossil ] [ text "Plant Macrofossil" ]
                                                                                    option [ attr.value Population.BioticProxies.MicrofossilGroup.Diatom ] [ text "Diatom" ]
                                                                                    option [ attr.value Population.BioticProxies.MicrofossilGroup.Ostracod ] [ text "Ostracod" ]
                                                                                ]
                                                                            ]
                                                                            div [ _class "col-md-3" ] [
                                                                                cond batch.UsePlaceholderInfer <| function
                                                                                | false -> concat [
                                                                                    label [ _class "form-label" ] [ text "Inference method (autofill)" ]
                                                                                    select [ _class "form-select form-select-sm"; bind.change.string (if batch.InferenceMethod.IsSome then batch.InferenceMethod.Value.AsString else "") (fun s -> SetBatchModeAutofill((timelineAtom |> fst |> fst), batch.Group,Graph.stringToKey s |> Some,batch.Outcome,batch.UsePlaceholderTaxon, batch.UsePlaceholderInfer) |> dispatch) ] [ ViewGen.optionGen<Population.BioticProxies.InferenceMethodNode> model.Graph ]]
                                                                                | true -> empty
                                                                            ]
                                                                            div [ _class "col-md-2" ] [
                                                                                label [ _class "form-label" ] [ text "Outcome (autofill)" ]
                                                                                select [ _class "form-select form-select-sm"; bind.change.string (if batch.Outcome.IsSome then batch.Outcome.Value.AsString else "") (fun s -> SetBatchModeAutofill((timelineAtom |> fst |> fst), batch.Group,batch.InferenceMethod,Graph.stringToKey s |> Some,batch.UsePlaceholderTaxon, batch.UsePlaceholderInfer) |> dispatch) ] [ ViewGen.optionGen<Outcomes.Biodiversity.BiodiversityDimensionNode> model.Graph ]
                                                                            ]
                                                                            div [ _class "col-md-2" ] [
                                                                                div [ _class "form-check" ] [
                                                                                    input [ _class "form-check-input"; attr.``type`` "checkbox"; bind.``checked`` batch.UsePlaceholderInfer (fun c -> SetBatchModeAutofill((timelineAtom |> fst |> fst), batch.Group,batch.InferenceMethod,batch.Outcome,batch.UsePlaceholderTaxon,c) |> dispatch) ]
                                                                                    label [ _class "form-check-label" ] [ text "Use placeholder for inference" ]
                                                                                ]
                                                                                small [ attr.``class`` "form-text" ] [ text "Uses 'unknown' to fill at a later date." ]
                                                                            ]
                                                                            div [ _class "col-md-2" ] [
                                                                                div [ _class "form-check" ] [
                                                                                    input [ _class "form-check-input"; attr.``type`` "checkbox"; bind.``checked`` batch.UsePlaceholderTaxon (fun c -> SetBatchModeAutofill((timelineAtom |> fst |> fst), batch.Group,batch.InferenceMethod,batch.Outcome,c,batch.UsePlaceholderInfer) |> dispatch) ]
                                                                                    label [ _class "form-check-label" ] [ text "Use placeholder for taxon?" ]
                                                                                ]
                                                                                small [ attr.``class`` "form-text" ] [ text "Uses 'unknown' to fill at a later date" ]
                                                                            ]
                                                                        ]
                                                                    ]
                                                                    div [ _class "card-footer" ] [
                                                                        cond (batch.LinkedTaxa.Count > 0 && batch.UnlinkedTaxa.Count = 0) <| function
                                                                        | true -> button [ _class "btn btn btn-primary"; on.click (fun _ -> ValidateOrConfirmBatch (timelineAtom |> fst |> fst) |> dispatch) ] [ text "Confirm (add) batch" ]
                                                                        | false -> button [ _class "btn btn-primary"; on.click (fun _ -> ValidateOrConfirmBatch (timelineAtom |> fst |> fst) |> dispatch) ] [ text "Validate batch" ]
                                                                        button [ _class "btn btn-outline-secondary"; on.click (fun _ -> UseBatchModeForProxiedTaxon ((timelineAtom |> fst |> fst),false) |> dispatch) ] [ text "Switch back to 'single add' mode" ]
                                                                    ]]
                                                                | None -> button [ _class "btn btn-outline-secondary mt-2 mb-2"; on.click (fun _ -> UseBatchModeForProxiedTaxon ((timelineAtom |> fst |> fst),true) |> dispatch) ] [ text "Switch to 'batch add' mode" ]
                                                                div [] [
                                                                    table [ _class "table" ] [
                                                                        thead [] [
                                                                            tr [] [
                                                                                th [ attr.scope "column" ] [ text "Biotic Proxy" ]
                                                                                th [ attr.scope "column" ] [ text "Inference Method" ]
                                                                                th [ attr.scope "column" ] [ text "Taxon / Taxa" ]
                                                                                th [ attr.scope "column" ] [ text "Measured by" ]
                                                                                th [ attr.scope "column" ] [ text "Actions" ]
                                                                            ]
                                                                        ]
                                                                        tbody [] [
                                                                            // Add a new proxied taxon and outcome measure.
                                                                            cond (source.AddBioticHyperedgeBatch |> Map.tryFind (timelineAtom |> fst |> fst)) <| function
                                                                            | None ->
                                                                                tr [] [        
                                                                                    cond (source.AddBioticHyperedge |> Map.tryFind (timelineAtom |> fst |> fst)) <| function
                                                                                    | Some (proxy,infer,taxon,outcome) -> concat [
                                                                                        td [] [ select [ _class "form-select"; bind.change.string (if proxy.IsSome then proxy.Value.AsString else "") (fun s -> ChangeProxiedTaxonVm((timelineAtom |> fst |> fst),Some (Graph.stringToKey s),infer,taxon,outcome) |> dispatch) ] [ ViewGen.optionGen<Population.BioticProxies.BioticProxyNode> model.Graph ] ]
                                                                                        td [] [ select [ _class "form-select"; bind.change.string (if infer.IsSome then infer.Value.AsString else "") (fun s -> ChangeProxiedTaxonVm((timelineAtom |> fst |> fst),proxy,Some (Graph.stringToKey s),taxon,outcome) |> dispatch) ] [ ViewGen.optionGen<Population.BioticProxies.InferenceMethodNode> model.Graph ] ]
                                                                                        td [] [ 
                                                                                            cond taxon <| function
                                                                                            | Some t -> 
                                                                                                concat [
                                                                                                    forEach t <| fun t -> div [ _class "form-floating" ] [
                                                                                                        select [ _class "form-select"; bind.change.string (t.AsString) (fun s -> ChangeProxiedTaxonVm((timelineAtom |> fst |> fst),proxy,infer,Some (taxon.Value |> List.map(fun f -> if f = t then Graph.stringToKey s else f)),outcome) |> dispatch) ] [ ViewGen.optionGen<Population.Taxonomy.TaxonNode> model.Graph ]
                                                                                                        label [ attr.style "pointer-events: auto !important;"; on.click (fun _ -> ChangeProxiedTaxonVm((timelineAtom |> fst |> fst),proxy,infer,Some (taxon.Value |> List.except [t]),outcome) |> dispatch)] [ text "Remove" ] ]
                                                                                                    select [ _class "form-select"; bind.change.string "" (fun s -> ChangeProxiedTaxonVm((timelineAtom |> fst |> fst),proxy,infer,Some (Graph.stringToKey s :: taxon.Value),outcome) |> dispatch) ] [ ViewGen.optionGen<Population.Taxonomy.TaxonNode> model.Graph ]
                                                                                                ]
                                                                                            | None -> select [ _class "form-select"; bind.change.string "" (fun s -> ChangeProxiedTaxonVm((timelineAtom |> fst |> fst),proxy,infer,Some ([Graph.stringToKey s]),outcome) |> dispatch) ] [ ViewGen.optionGen<Population.Taxonomy.TaxonNode> model.Graph ]
                                                                                        ]
                                                                                        td [] [ select [ _class "form-select"; bind.change.string (if outcome.IsSome then outcome.Value.AsString else "") (fun s -> ChangeProxiedTaxonVm((timelineAtom |> fst |> fst),proxy,infer,taxon,Some (Graph.stringToKey s)) |> dispatch) ] [ ViewGen.optionGen<Outcomes.Biodiversity.BiodiversityDimensionNode> model.Graph ] ] ]
                                                                                    | None -> concat [
                                                                                        td [] [ select [ _class "form-select"; bind.change.string "" (fun s -> ChangeProxiedTaxonVm((timelineAtom |> fst |> fst),Some (Graph.stringToKey s),None,None,None) |> dispatch) ] [ ViewGen.optionGen<Population.BioticProxies.BioticProxyNode> model.Graph ] ]
                                                                                        td [] [ select [ _class "form-select"; bind.change.string "" (fun s -> ChangeProxiedTaxonVm((timelineAtom |> fst |> fst),None,Some (Graph.stringToKey s),None,None) |> dispatch) ] [ ViewGen.optionGen<Population.BioticProxies.InferenceMethodNode> model.Graph ] ]
                                                                                        td [] [ select [ _class "form-select"; bind.change.string "" (fun s -> ChangeProxiedTaxonVm((timelineAtom |> fst |> fst),None,None,Some ([Graph.stringToKey s]),None) |> dispatch) ] [ ViewGen.optionGen<Population.Taxonomy.TaxonNode> model.Graph ] ]
                                                                                        td [] [ select [ _class "form-select"; bind.change.string "" (fun s -> ChangeProxiedTaxonVm((timelineAtom |> fst |> fst),None,None,None,Some (Graph.stringToKey s)) |> dispatch) ] [ ViewGen.optionGen<Outcomes.Biodiversity.BiodiversityDimensionNode> model.Graph ] ] ]
                                                                                    td [] [
                                                                                        button [ _class "btn btn-primary"; on.click (fun _ -> SubmitProxiedTaxon (timelineAtom |> fst |> fst) |> dispatch) ] [ text "Save" ]
                                                                                    ]
                                                                                ]
                                                                            | Some batch -> concat [
                                                                                // In batch mode, show each verified taxon as a normal entry field set (so it can be changed further)
                                                                                forEach batch.LinkedTaxa <| fun proxiedTaxonEdge -> tr [] [
                                                                                    td [] [ select [ _class "form-select"; bind.change.string (proxiedTaxonEdge.Value |> fun (a,_,_,_) -> a.AsString) (fun s -> (proxiedTaxonEdge.Value |> fun (m,i,t,o) -> ChangeBatchVerifiedTaxon((timelineAtom |> fst |> fst),proxiedTaxonEdge.Key,(Graph.stringToKey s,i,t,o))) |> dispatch) ] [ ViewGen.optionGen<Population.BioticProxies.BioticProxyNode> model.Graph ] ]
                                                                                    td [] [ select [ _class "form-select"; bind.change.string (proxiedTaxonEdge.Value |> fun (_,i,_,_) -> i.AsString) (fun s -> (proxiedTaxonEdge.Value |> fun (m,i,t,o) -> ChangeBatchVerifiedTaxon((timelineAtom |> fst |> fst),proxiedTaxonEdge.Key,(m,Graph.stringToKey s,t,o))) |> dispatch) ] [ ViewGen.optionGen<Population.BioticProxies.InferenceMethodNode> model.Graph ] ]
                                                                                    td [] [ 
                                                                                        forEach (proxiedTaxonEdge.Value |> fun(_,_,t,_) -> t) <| fun taxon ->
                                                                                            div [ _class "form-floating" ] [
                                                                                                select [ _class "form-select"; bind.change.string (taxon.AsString) (fun s -> (proxiedTaxonEdge.Value |> fun (m,i,t,o) -> ChangeBatchVerifiedTaxon((timelineAtom |> fst |> fst),proxiedTaxonEdge.Key,(m,i,(t |> List.map(fun f -> if f = taxon then Graph.stringToKey s else f)),o))) |> dispatch) ] [ ViewGen.optionGen<Population.Taxonomy.TaxonNode> model.Graph ]
                                                                                                label [ attr.style "pointer-events: auto !important;"; on.click (fun s -> (proxiedTaxonEdge.Value |> fun (m,i,t,o) -> ChangeBatchVerifiedTaxon((timelineAtom |> fst |> fst),proxiedTaxonEdge.Key,(m,i,(t |> List.except [taxon]),o))) |> dispatch) ] [ text "Remove" ]
                                                                                            ]
                                                                                        select [ _class "form-select"; bind.change.string "" (fun s -> (proxiedTaxonEdge.Value |> fun (m,i,t,o) -> ChangeBatchVerifiedTaxon((timelineAtom |> fst |> fst),proxiedTaxonEdge.Key,(m,i,Graph.stringToKey s :: t,o))) |> dispatch) ] [ ViewGen.optionGen<Population.Taxonomy.TaxonNode> model.Graph ]
                                                                                        ]
                                                                                    td [] [ select [ _class "form-select"; bind.change.string (proxiedTaxonEdge.Value |> fun (_,_,_,o) -> o.AsString) (fun s -> (proxiedTaxonEdge.Value |> fun (m,i,t,o) -> ChangeBatchVerifiedTaxon((timelineAtom |> fst |> fst),proxiedTaxonEdge.Key,(m,i,t,Graph.stringToKey s))) |> dispatch) ] [ ViewGen.optionGen<Outcomes.Biodiversity.BiodiversityDimensionNode> model.Graph ] ]
                                                                                    td [] [ button [ _class "btn btn-outline-secondary"; on.click(fun _ -> RemoveBatchVerifiedTaxon(timelineAtom |> fst |> fst, proxiedTaxonEdge.Key) |> dispatch) ] [ text "Remove" ]]
                                                                                ]
                                                                                cond (batch.InferenceMethod.IsSome && batch.Outcome.IsSome) <| function
                                                                                | true ->
                                                                                    cond (Storage.atomFriendlyNameByKey batch.InferenceMethod.Value g) <| function
                                                                                    | Some inference ->
                                                                                        cond (Storage.atomFriendlyNameByKey batch.Outcome.Value g) <| function
                                                                                        | Some outcome -> concat [
                                                                                            // Display unverified batch entries
                                                                                            forEach batch.UnlinkedTaxa <| fun unlinkedT -> tr [] [
                                                                                                td [] [ input [ _class "form-control form-control-sm"; bind.input.string (fst unlinkedT.Value) (fun s -> ((timelineAtom |> fst |> fst),unlinkedT.Key, s, (snd unlinkedT.Value)) |> ChangeBatchUnverifiedProxiedTaxon |> dispatch) ] ]
                                                                                                td [] [ text inference ]
                                                                                                td [] [
                                                                                                    cond batch.UsePlaceholderTaxon <| function
                                                                                                    | false -> concat [
                                                                                                        input [ _class "form-control form-control-sm"; bind.input.string (snd unlinkedT.Value) (fun s -> ((timelineAtom |> fst |> fst),unlinkedT.Key, (fst unlinkedT.Value), s) |> ChangeBatchUnverifiedProxiedTaxon |> dispatch) ]
                                                                                                        small [ _class "form-text" ] [ text "for multiple, use ; seperator" ] ]
                                                                                                    | true -> text "Unknown (to be coded later)"
                                                                                                ]
                                                                                                td [] [ text outcome ]
                                                                                                td [] [ button [ _class "btn btn-outline-secondary"; on.click(fun _ -> RemoveBatchUnverifiedTaxon(timelineAtom |> fst |> fst, unlinkedT.Key) |> dispatch) ] [ text "Remove" ]]
                                                                                            ]
                                                                                            tr [] [
                                                                                                td [] []
                                                                                                td [] []
                                                                                                td [] []
                                                                                                td [] []
                                                                                                td [] [ button [ _class "btn btn-outline-secondary"; on.click (fun _ -> ((timelineAtom |> fst |> fst),999, "", "") |> ChangeBatchUnverifiedProxiedTaxon |> dispatch) ] [ text "Add another row" ]]
                                                                                            ]]
                                                                                        | None -> empty
                                                                                    | None -> empty
                                                                                | false -> tr [] [ td [] [ text "For batch entry, please fill in the inference method and outcome above." ] ]
                                                                            ]

                                                                            forEach (timelineAtom |> GraphStructure.Relations.nodeIdsByRelation<Exposure.ExposureRelation> Exposure.ExposureRelation.HasProxyInfo |> Storage.atomsByKey g ) <| fun proxiedTaxonEdge -> concat [
                                                                                cond (proxiedTaxonEdge |> GraphStructure.Relations.nodeIdsByRelation<Population.PopulationRelation> Population.PopulationRelation.InferredFrom |> Seq.head |> (fun k -> Storage.atomFriendlyNameByKey k g)) <| function
                                                                                | Some proxyName ->
                                                                                    cond (proxiedTaxonEdge |> GraphStructure.Relations.nodeIdsByRelation<Population.PopulationRelation> Population.PopulationRelation.InferredUsing |> Seq.head |> (fun k -> Storage.atomFriendlyNameByKey k g)) <| function
                                                                                    | Some methodName ->
                                                                                        cond (proxiedTaxonEdge |> GraphStructure.Relations.nodeIdsByRelation<Population.PopulationRelation> Population.PopulationRelation.MeasuredBy |> Seq.head |> (fun k -> Storage.atomFriendlyNameByKey k g)) <| function
                                                                                        | Some outcome ->
                                                                                        tr [] [
                                                                                            td [] [ text proxyName ]
                                                                                            td [] [ text methodName ]
                                                                                            td [] [ 
                                                                                                forEach (proxiedTaxonEdge |> GraphStructure.Relations.nodeIdsByRelation<Population.PopulationRelation> Population.PopulationRelation.InferredAs |> Seq.map (fun k -> Storage.atomFriendlyNameByKey k g)) <| fun taxonName ->
                                                                                                    cond taxonName <| function
                                                                                                    | Some taxonName -> text taxonName
                                                                                                    | None -> empty ]
                                                                                            td [] [ text outcome ]
                                                                                            td [] []
                                                                                        ]
                                                                                        | None -> text "None A"
                                                                                    | None -> text "None B"
                                                                                | None -> textf "Proxy relation: %A" proxiedTaxonEdge
                                                                            ]
                                                                        ]
                                                                    ] // end proxied taxa table
                                                                ]
                                                                hr []
                                                            ]
                                                        button [ _class "btn btn-primary"; on.click (fun _ -> CompleteSection "outcome" |> dispatch) ] [ text "I've finished coding outcomes" ]
                                                    ]
                                                ]
                                            ] // end outcomes card

                                            cond (isFlaggable codingStatus) <| function
                                            | true ->
                                                div [ _class "card mb-4" ] [
                                                    div [ _class "card-header text-bg-secondary" ] [ text "I can't complete this source now..." ]
                                                    div [ _class "card-body" ] [
                                                        p [] [ text "If you can't complete this source now, please state the section that you're having problems with and state a reason." ]
                                                        div [ _class "row mb-3" ] [
                                                            label [ _class "col-sm-2 col-form-label" ] [ text "I can't complete this section" ]
                                                            div [ _class "col-sm-10" ] [
                                                                select [ _class "form-select"; bind.change.string source.ProblematicSection (fun v -> ChangeCodingProblem (v,"") |> dispatch ) ] [ 
                                                                    forEach sections <| fun s ->
                                                                        option [ attr.value s.Key ] [ text s.Value ]
                                                                ]]
                                                            ]
                                                        div [ _class "row mb-3" ] [
                                                            label [ _class "col-sm-2 col-form-label" ] [ text "The reason is..." ]
                                                            div [ _class "col-sm-10" ] [
                                                                textarea [ _class "form-control"; bind.change.string source.ProblematicSectionReason (fun v -> ChangeCodingProblem (source.ProblematicSection,v) |> dispatch ) ] []
                                                            ]
                                                        ]
                                                        button [ on.click (fun _ -> SubmitCodingProblem |> dispatch ) ] [ text "Flag" ]
                                                    ]
                                                ] // end flagging card
                                            | false -> empty

                                        ]
                                    | _ -> empty // is not a source node.
                                ] // end source loaded
                            ] // end graph loaded
                        ]
                ]
            ]
        ]

open Microsoft.Extensions.DependencyInjection

type MainApp() =
    inherit ProgramComponent<App.Model, App.Message>()

    override this.Program =
        let folderPicker = this.Services.GetService<IFolderPicker>()
        Program.mkProgram (fun _ -> App.initModel) (App.update folderPicker.PickFolder) App.view