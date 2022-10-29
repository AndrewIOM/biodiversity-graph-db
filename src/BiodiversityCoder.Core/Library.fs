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
            node (atom |> Graph.getAtomId |> string) [ CyParam.label label ] )

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
        |> HTML.toEmbeddedHTML


module App =

    open System.Threading.Tasks

    type Page =
        | Extract
        | Sources
        | Population
        | Exposure
        | Outcome

    type Model =
        {
            Page: Page
            Graph: Storage.FileBasedGraph<GraphStructure.Node,GraphStructure.Relation> option
            Import: string
            Error: string option
            NodeCreationViewModels: Map<string, NodeViewModel>
            NodeCreationValidationErrors: Map<string, (string * string) list>
            NodeCreationRelations: Map<string, string * Map<GraphStructure.ProposedRelation, string list>>
            SelectedSource: SelectedSource option
            TaxonLookup: TaxonomicLookupModel
        }

    and TaxonomicLookupModel = {
        Rank: string
        Family: string
        Genus: string
        Species: string
        Authorship: string
        Result: (Population.Taxonomy.TaxonNode * list<Population.PopulationNodeRelation>) option
    }
    
    and SelectedSource = {
        SelectedSource: Graph.Atom<GraphStructure.Node,GraphStructure.Relation>
        LinkToSecondarySource: string option
        Screening: NodeViewModel
    }
    
    and EligbilityCriteria =
        | Include
        | Exclude of because:Sources.ExclusionReason * notes:FieldDataTypes.Text.Text

    let initModel =
        match Storage.loadOrInitGraph "/Users/andrewmartin/Desktop/test-graph/" with
        | Ok g ->  
            // TODO If graph has no data, run seed here.
            { TaxonLookup = { Rank = "Genus"; Family = ""; Genus = ""; Species = ""; Authorship = ""; Result = None }; NodeCreationRelations = Map.empty; SelectedSource = None; Graph = Some g; Import = ""; Error = None; Page = Extract; NodeCreationViewModels = Map.empty; NodeCreationValidationErrors = Map.empty }, Cmd.none
        | Error e -> { TaxonLookup = { Rank = "Genus"; Family = ""; Genus = ""; Species = ""; Authorship = ""; Result = None }; NodeCreationRelations = Map.empty; SelectedSource = None; Graph = None; Import = ""; Error = Some e; Page = Extract; NodeCreationViewModels = Map.empty; NodeCreationValidationErrors = Map.empty }, Cmd.none

    type Message =
        | SetPage of Page
        | DismissError
        | SelectFolder
        | SelectedFolder of string
        | ChangeImportText of string
        | ImportBibtex
        | ImportColandr
        | FormMessage of FormMessage
        | SelectSource of key:string
        | ScreenSource of NodeViewModel
        | LookupTaxon of LookupTaxonMessage

    and LookupTaxonMessage =
        | ChangeFormFields of TaxonomicLookupModel
        | RunLookup

    let update (openFolder:unit -> Task<string>) message model =
        match message with
        | SetPage page -> { model with Page = page }, Cmd.none
        | DismissError -> { model with Error = None }, Cmd.none
        | ChangeImportText s -> { model with Import = s }, Cmd.none
        | ImportBibtex -> 
            match BibtexParser.parse model.Import with
            | Error e -> { model with Error = Some e }, Cmd.none
            | Ok nodes ->
                let nodes = nodes |> List.map (Sources.SourceNode.Unscreened >> GraphStructure.Node.SourceNode)
                match model.Graph with
                | Some g -> 
                    match Storage.addNodes g (typeof<Sources.SourceNode>).Name nodes with
                    | Ok g -> { model with Graph = Some g }, Cmd.none
                    | Error e -> { model with Error = Some e }, Cmd.none
                | None -> model, Cmd.none
        | ImportColandr ->
            match model.Graph with
            | Some g ->
                try
                    match ColandrParser.syncColandr (System.IO.Path.Combine(g.Directory,"colandr-titleabs-screen-results.csv")) with
                    | Error e -> { model with Error = Some e }, Cmd.none
                    | Ok nodes ->
                        let nodes = nodes |> Seq.map (Sources.SourceNode.Unscreened >> GraphStructure.Node.SourceNode) |> Seq.toList
                        // TODO Don't overwrite existing nodes.
                        match Storage.addNodes g (typeof<Sources.SourceNode>).Name nodes with
                        | Ok g -> { model with Graph = Some g }, Cmd.none
                        | Error e -> { model with Error = Some e }, Cmd.none
                with e -> { model with Error = Some e.Message }, Cmd.none
            | None -> { model with Error = Some "There was no graph" }, Cmd.none
        | SelectSource k ->
            match model.Graph with
            | None -> { model with Error = Some <| "Can't select a source when no graph is loaded." }, Cmd.none
            | Some g ->
                match g |> Storage.atomByKey k with
                | Some atom -> 
                    { model with SelectedSource = Some { SelectedSource = atom; LinkToSecondarySource = None; Screening = NotEnteredYet } }, Cmd.none
                | None -> { model with Error = Some <| sprintf "Could not find source with key %s" k }, Cmd.none
        | SelectFolder ->
            model, Cmd.OfAsync.result(async {
                let! folder = openFolder () |> Async.AwaitTask
                return SelectedFolder folder
            })
        | SelectedFolder folder ->
            match Storage.loadOrInitGraph folder with
            | Ok g -> { model with Graph = Some g }, Cmd.none
            | Error e -> { model with Error = Some e }, Cmd.none
        | FormMessage m ->
            match m with
            | RelateNodes(_, _, _) -> failwith "Not Implemented"
            | AddProxiedTaxon(_) -> failwith "Not Implemented"

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

            | AddOrUpdateNode (nodeType, validateRelations) -> 
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
                                    | Sources.SourceNode.Included s
                                    | Sources.SourceNode.Unscreened s ->
                                        match (node :?> EligbilityCriteria) with
                                        | Include -> Sources.SourceNode.Included s |> Ok
                                        | Exclude (because, notes) -> Sources.SourceNode.Excluded (s, because, notes) |> Ok
                                | _ -> Error "No source was selected"
                            Create.createFromViewModel nodeType source.Screening
                            |> Result.bind updateNode
                            |> Result.lift GraphStructure.Node.SourceNode
                            |> Result.bind (fun n -> Storage.updateNode g "SourceNode" (source.SelectedSource |> fst |> fst, n))
                            |> Result.lift (fun g -> { model with Graph = Some g })
                            |> Result.lower (fun r -> r, Cmd.none) (fun e -> { model with Error = Some e }, Cmd.none)
                        | None -> { model with Error = Some "Cannot screen source as graph is not loaded." }, Cmd.none
                    | None -> model, Cmd.none
                else
                    match model.NodeCreationViewModels |> Map.tryFind nodeType.Name with
                    | Some (formData: NodeViewModel) -> 
                        let node = Create.createFromViewModel nodeType formData
                        match node with
                        | Error e -> { model with Error = Some e }, Cmd.none
                        | Ok n -> 
                            match model.Graph with
                            | Some g ->
                                // Is a normal new graph node:
                                GraphStructure.Nodes.tryMakeNode nodeType n
                                |> Result.bind (fun n -> Storage.addNodes g nodeType.Name [ n ])
                                |> Result.lift (fun g -> { model with Graph = Some g })
                                |> Result.lower (fun r -> r, Cmd.none) (fun e -> { model with Error = Some e }, Cmd.none)
                            | None -> { model with Error = Some "Cannot make node as graph is not loaded." }, Cmd.none
                    | None -> { model with Error = Some (sprintf "Could not find type of %s" nodeType.Name) }, Cmd.none
            | EnterNodeRelationData(_, _, sinkKeys) -> failwith "Not Implemented"
            | ChangeNodeRelationToggle(_, _) -> failwith "Not Implemented"
        | ScreenSource(_) -> failwith "Not Implemented"
        | LookupTaxon l ->
            match l with
            | ChangeFormFields f -> { model with TaxonLookup = f }, Cmd.none
            | RunLookup ->
                let run = TaxonomicBackbone.GlobalPollenProject.lookupAsNodesAndRelations model.TaxonLookup.Rank model.TaxonLookup.Family model.TaxonLookup.Genus model.TaxonLookup.Species model.TaxonLookup.Authorship
                match run with
                | Ok t -> { model with TaxonLookup = { model.TaxonLookup with Result = Some t } }, Cmd.none
                | Error e -> { model with Error = Some e }, Cmd.none

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

    let view model dispatch =
        div [ _class "container-fluid" ] [
            div [ _class "row flex-nowrap" ] [ 
                // 1. Sidebar for selecting section
                // Should link to editable info for core node types: population (context, proxied taxa), exposure (time), outcome (biodiversity indicators).
                sidebarView [ Page.Extract; Page.Population; Page.Exposure; Page.Outcome; Page.Sources ] dispatch

                // 2. Page view
                div [ _class "col py-3" ] [
                    cond model.Page <| function
                        | Page.Population -> concat [
                                h2 [] [ text "Population" ]
                                p [] [ text "List existing population nodes and create new ones." ]
                                img [ attr.src "images/population-diagram.png" ]
                                h3 [] [ text "Context" ]
                                hr []
                                p [] [ text "You can relate `timelines` to a spatial context, represented by a Context node." ]
                                ViewGen.makeNodeForm<Population.Context.ContextNode> (model.NodeCreationViewModels |> Map.tryFind "ContextNode") (FormMessage >> dispatch)
                                h3 [] [ text "Taxonomy Node" ]
                                hr []
                                p [] [ text "In our systematic map, biotic proxies are related to real taxa by an inference method. Here, the 'Taxonomy Node' represents a *real* botanical or other taxon. We have pre-populated plant names using a taxonomic backbone." ]
                                ViewGen.makeNodeForm<Population.Taxonomy.TaxonNode> (model.NodeCreationViewModels |> Map.tryFind "TaxonNode") (FormMessage >> dispatch)
                                p [] [ text "In addition to the taxon node, there are nodes representing the common or vernacular names of species, genera, or families. Adding these is purely for the purposes of public interpretation of the graph database." ]
                                ViewGen.makeNodeForm<Population.Taxonomy.VernacularTaxonLabelNode> (model.NodeCreationViewModels |> Map.tryFind "VernacularTaxonLabelNode") (FormMessage >> dispatch)
                                h3 [] [ text "Biotic proxies" ]
                                hr []
                                p [] [ text "Biotic proxies are used to represent morphotypes, fossil remains etc. that are used to proxy species presence, but require further interpretation to connect to a *real* taxon. For example, pollen morphotypes require inference to connect to botanical taxa." ]
                                ViewGen.makeNodeForm<Population.BioticProxies.BioticProxyNode> (model.NodeCreationViewModels |> Map.tryFind "BioticProxyNode") (FormMessage >> dispatch)

                                
                                textf "%A" model.NodeCreationViewModels
                                textf "\n Model is: %A" model.Error
                            ]
                        | Page.Exposure -> div [] [ text "Exposure page" ]
                        | Page.Outcome -> div [] [ text "Outcome page" ]
                        | Page.Sources -> concat [
                                h2 [] [ text "Sources Manager" ]
                                hr []

                                text "You can import sources from Colandr using the button below. The colandr raw output should be saved as 'colandr-titleabs-screen-results.csv' in the graph database folder."
                                button [ _class "btn btn-primary"; on.click (fun _ -> ImportColandr |> dispatch) ] [ text "Import from Colandr (title-abstract screening)" ]

                                div [ _class "card" ] [
                                    div [ _class "card-header" ] [ text "Import new sources" ]
                                    text "Enter a bibtex-format file below."
                                    textarea [ bind.input.string model.Import (fun s -> ChangeImportText s |> dispatch) ] []
                                    button [ on.click (fun _ -> ImportBibtex |> dispatch ) ] [ text "Import" ]
                                ]

                                text <| sprintf "%A" model
                            ]

                        | Page.Extract -> div [] [

                            h2 [] [ text "Data Coding" ]
                            p [] [ text "This tool allows coding information from bibliographic sources directly into a graph database." ]

                            cond model.Error <| function
                            | Some e -> div [ _class "alert alert-danger" ] [ 
                                    text e
                                    button [ _class "btn"; on.click (fun _ -> DismissError |> dispatch) ] [ text "Dismiss" ] ]
                            | None -> empty

                            cond model.Graph <| function
                            | None -> concat [
                                // No graph is loaded. Connect to a graph folder.
                                p [] [ text "To get started, please connect the data coding tool to the folder where you are storing the graph database files." ]
                                label [] [ text "Where is the graph database stored?" ]
                                button [ _class "btn btn-primary"; on.click (fun _ -> SelectFolder |> dispatch)] [ text "Select folder to connect to." ] ]
                            | Some g -> concat [
                                
                                div [ _class "card text-bg-secondary" ] [
                                    div [ _class "card-header" ] [ text "Source Details" ]
                                    div [ _class "card-body" ] [
                                        text "Selected source:"
                                        cond model.SelectedSource <| function
                                        | Some s ->
                                            select [ bind.change.string ((s.SelectedSource |> fst |> snd).Key()) (fun k -> SelectSource k |> dispatch) ] [(
                                                cond (g.Nodes<Sources.SourceNode>()) <| function
                                                | Some sources ->
                                                    sources
                                                    |> Seq.map(fun k ->
                                                        option [ attr.value k.Key ] [ text k.Value ])
                                                    |> Seq.toList
                                                    |> concat
                                                | None -> empty
                                            )]
                                        | None ->
                                            select [ bind.change.string "" (fun k -> SelectSource k |> dispatch) ] [(
                                                cond (g.Nodes<Sources.SourceNode>()) <| function
                                                | Some sources ->
                                                    sources
                                                    |> Seq.map(fun k ->
                                                        option [ attr.value k.Key ] [ text k.Value ])
                                                    |> Seq.toList
                                                    |> concat
                                                | None -> empty
                                            )]
                                    ]
                                ]

                                cond model.SelectedSource <| function
                                | None -> text "Select a source to continue"
                                | Some source -> concat [

                                    div [ _class "card" ] [
                                        div [ _class "card-header" ] [ text "Q: Is the source relevant?" ]
                                        div [ _class "card-body" ] [
                                            p [] [ text "Please apply the eligbility criteria against the full-text PDF of this source and determine if the source should be included or excluded." ]
                                            ViewGen.makeNodeForm'<EligbilityCriteria> (Some source.Screening) "Screen" (FormMessage >> dispatch) (fun _ -> true)
                                        ]
                                    ]

                                    div [ _class "card" ] [
                                        div [ _class "card-header" ] [ text "Q: Is it a primary or secondary source?" ]
                                        div [ _class "card-body" ] [
                                            p [] [ 
                                                text "A source may be 'secondary' if it does not contain any new information, but references information in other publications."
                                                text "You can link this source to the primary sources by selecting an existing source, or adding a new one. Please check that the source does not already exist before creating a new one." ]
                                            select [ bind.change.string (if model.SelectedSource.IsSome then (model.SelectedSource.Value.SelectedSource |> fst |> snd).Key() else "") (fun k -> SelectSource k |> dispatch) ] [(
                                                cond (g.Nodes<Sources.SourceNode>()) <| function
                                                | Some sources ->
                                                    sources
                                                    |> Seq.map(fun k ->
                                                        option [ attr.value k.Key ] [ text k.Value ])
                                                    |> Seq.toList
                                                    |> concat
                                                | None -> empty
                                            )]
                                            button [ _class "btn btn-primary" ] [ text "Link to this primary source." ]
                                            p [] [ text "You may alternatively specify a source we do not already have listed using the below fields. This will be linked to the selected source." ]
                                            div [ _class "card" ] [
                                                div [ _class "card-header" ] [ text "Add a new source" ]
                                                text "You may need to reference another source from this source that isn't already in our included sources."
                                                // TODO relate to master source node.
                                                ViewGen.makeNodeForm<Sources.SourceNode> (model.NodeCreationViewModels |> Map.tryFind "SourceNode") (FormMessage >> dispatch)
                                            ]
                                        ]
                                    ]

                                    div [ _class "card" ] [
                                        div [ _class "card-header" ] [ text "Q: How are the study timeline(s) formed?" ]
                                        div [ _class "card-body" ] [
                                            div [ _class "card text-bg-secondary p-3"] [
                                                p [] [ 
                                                    text "A study timeline is a continuous or discontinuous time-sequence over which biodiversity measures for biotic proxies are specified."
                                                    text "Individual study timelines have their own spatial contexts. Therefore, time-series from different spatial locations should be classed as seperate time-series (e.g. multiple sediment cores)."
                                                    text "If there are many points in a spatial area - for example individual tree-ring series - but the species are the same, you should specify a single timeline but attach a broader (e.g. regional) spatial context." ]
                                                ViewGen.makeNodeFormWithRelations<Exposure.StudyTimeline.IndividualTimelineNode> (fun savedRelations ->
                                                    (ViewGen.RelationsForms.Validation.hasOne (GraphStructure.ProposedRelation.Exposure Exposure.ExposureRelation.ExtentEarliest) savedRelations 
                                                    && ViewGen.RelationsForms.Validation.hasOne (GraphStructure.ProposedRelation.Exposure Exposure.ExposureRelation.ExtentLatest) savedRelations)
                                                    || ViewGen.RelationsForms.Validation.hasOne (GraphStructure.ProposedRelation.Exposure Exposure.ExposureRelation.IntersectsTime) savedRelations
                                                ) (model.NodeCreationViewModels |> Map.tryFind "IndividualTimelineNode") (FormMessage >> dispatch)
                                                ViewGen.RelationsForms.relationsToggle<Exposure.StudyTimeline.IndividualTimelineNode> [
                                                    ("Year", [
                                                        ViewGen.RelationsForms.selectExistingNode<Exposure.TemporalIndex.CalYearNode> "" (Exposure.ExposureRelation.ExtentEarliest |> GraphStructure.ProposedRelation.Exposure) // TODO Uncertainty
                                                        ViewGen.RelationsForms.selectExistingNode<Exposure.TemporalIndex.CalYearNode> "" (Exposure.ExposureRelation.ExtentLatest |> GraphStructure.ProposedRelation.Exposure) ])
                                                    ("Qualitative", [
                                                        ViewGen.RelationsForms.selectExistingNodeMulti<Exposure.TemporalIndex.QualitativeLabelNode> "" (Exposure.ExposureRelation.IntersectsTime |> GraphStructure.ProposedRelation.Exposure)
                                                    ])
                                                ] model.NodeCreationRelations g (FormMessage >> dispatch)
                                                p [] [ 
                                                    text "Each study timeline is defined by a dating method. Explain these here."
                                                ]
                                                forEach (source.SelectedSource |> GraphStructure.Relations.nodeIdsByRelation<Sources.SourceRelation> Sources.SourceRelation.HasTemporalExtent |> Storage.atomsByGuid g ) <| fun timeline ->
                                                    // TODO once above node is created (study timeline), require a
                                                    // single relation to a study context.
                                                    ViewGen.makeNodeForm<Population.Context.ContextNode> (model.NodeCreationViewModels |> Map.tryFind "ContextNode") (FormMessage >> dispatch)
                                                p [] [ 
                                                    text "Each study timeline is defined by a dating method. Explain these here."
                                                ]
                                                forEach (source.SelectedSource |> GraphStructure.Relations.nodeIdsByRelation<Sources.SourceRelation> Sources.SourceRelation.HasTemporalExtent |> Storage.atomsByGuid g ) <| fun timeline ->
                                                    // TODO once above node is created (study timeline), require a relation
                                                    // between that node and these individual date nodes.
                                                    // Allow insertion of one to many date relations.
                                                    ViewGen.makeNodeForm<Exposure.StudyTimeline.IndividualDateNode> (model.NodeCreationViewModels |> Map.tryFind "IndividualDateNode") (FormMessage >> dispatch)
                                            ]
                                            
                                            
                                             // end make new timeline card.
                                        ] // end timelines card.
                                    ]

                                    div [ _class "card" ] [
                                        div [ _class "card-header" ] [ text "Q: Which biodiversity outcomes are associated with each timeline?" ]
                                        div [ _class "card-body" ] [
                                            p [] [
                                                text ""
                                            ]
                                            forEach (source.SelectedSource |> GraphStructure.Relations.nodeIdsByRelation<Sources.SourceRelation> Sources.SourceRelation.HasTemporalExtent |> Storage.atomsByGuid g ) <| fun timeline ->
                                                
                                                
                                                
                                                empty
                                        ]
                                    ]

                                ] // end source loaded
                            ] // end graph loaded

                            div [ _class "card" ] [
                                div [ _class "card-header" ] [ text "Data coding" ]

                                // 3. For each study timeline, add proxied taxa.
                                forEach [ 1 .. 2 ] <| fun timeline ->
                                    // Display existing and add new proxied taxa
                                    // Proxied taxa are related to an outcome too.
                                    div [] [
                                        p [] [ textf "Timeline %i" timeline ]
                                        table [ _class "table" ] [
                                            thead [] [
                                                tr [] [
                                                    th [ attr.scope "column" ] [ text "Biotic Proxy" ]
                                                    th [ attr.scope "column" ] [ text "Inference Method" ]
                                                    th [ attr.scope "column" ] [ text "Botanical Taxon / Taxa" ]
                                                    th [ attr.scope "column" ] [ text "Measured by" ]
                                                ]
                                            ]
                                            tbody [] [
                                                // Add a new one. form in here.
                                                // Existing proxied taxa:
                                                tr [] [                                                                    
                                                    td [] [ select [] [ ViewGen.optionGen<Population.BioticProxies.BioticProxyNode> model.Graph ] ]
                                                    td [] [ select [] [ ViewGen.optionGen<Population.BioticProxies.InferenceMethodNode> model.Graph ] ]
                                                    td [] [ select [] [ ViewGen.optionGen<Population.Taxonomy.TaxonNode> model.Graph ] ]
                                                    td [] [ select [] [ ViewGen.optionGen<Outcomes.Biodiversity.BiodiversityDimensionNode> model.Graph ] ]
                                                ]
                                                forEach [ 1 .. 7 ] <| fun proxiedTaxa ->
                                                    tr [] [
                                                        td [] [ text "Betula (pollen morphotype)" ]
                                                        td [] [ text "Implicit" ]
                                                        td [] [ text "Betula" ]
                                                        td [] [ text "Abundance" ]
                                                    ]
                                            ]
                                        ]
                                    ]


                            ]

                            cond model.Error (function
                            | Some e ->
                                // div [ _class "toast" ] [
                                //     div [ _class "toast-header" ] [
                                //         strong [] [ text "Error" ]
                                //         button [ attr.``type`` "button"; _class "btn-close" ] [] 
                                //     ]
                                //     div [ _class "toast-body" ] [
                                        text e
                                //     ]
                                // ]
                            | None -> empty)
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