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
        | Source
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
        }

    let initModel =
        match Storage.loadOrInitGraph "/Users/andrewmartin/Desktop/test-graph/" with
        | Ok g ->  { Graph = Some g; Import = ""; Error = None; Page = Source; NodeCreationViewModels = Map.empty; NodeCreationValidationErrors = Map.empty }, Cmd.none
        | Error e -> { Graph = None; Import = ""; Error = Some e; Page = Source; NodeCreationViewModels = Map.empty; NodeCreationValidationErrors = Map.empty }, Cmd.none

    type Message =
        | SetPage of Page
        | SelectFolder
        | SelectedFolder of string
        | SyncFileSystem
        | ChangeImportText of string
        | ImportBibtex
        | FormMessage of FormMessage

    let update (openFolder:unit -> Task<string>) message model =
        match message with
        | SetPage page -> { model with Page = page }, Cmd.none
        | SyncFileSystem -> model, Cmd.none
        | ChangeImportText s -> { model with Import = s }, Cmd.none
        | ImportBibtex -> 
            match BibtexParser.parse model.Import with
            | Error e -> { model with Error = Some e }, Cmd.none
            | Ok nodes ->
                let nodes = nodes |> List.map GraphStructure.Node.SourceNode
                match model.Graph with
                | Some g -> 
                    match Storage.addNodes g nodes with
                    | Ok g -> { model with Graph = Some g }, Cmd.none
                    | Error e -> { model with Error = Some e }, Cmd.none
                | None -> model, Cmd.none
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
                let updatedVm = 
                    match model.NodeCreationViewModels |> Map.tryFind formId with
                    | Some formData -> Merge.updateNodeViewModel formData vm
                    | None -> Merge.updateNodeViewModel NotEnteredYet vm
                { model with NodeCreationViewModels = model.NodeCreationViewModels |> Map.add formId updatedVm }, Cmd.none

            | AddOrUpdateNode nodeType -> 
                match model.NodeCreationViewModels |> Map.tryFind nodeType.Name with
                | Some (formData: NodeViewModel) -> 
                    let node = Create.createFromViewModel nodeType formData
                    match node with
                    | Error e -> { model with Error = Some e }, Cmd.none
                    | Ok n -> 
                        match model.Graph with
                        | Some g ->
                            GraphStructure.Nodes.tryMakeNode nodeType n
                            |> Result.bind (fun n -> Storage.addNodes g [ n ])
                            |> Result.lift (fun g -> { model with Graph = Some g })
                            |> Result.lower (fun r -> r, Cmd.none) (fun e -> { model with Error = Some e }, Cmd.none)
                        | None -> { model with Error = Some "Cannot make node as graph is not loaded." }, Cmd.none
                | None -> { model with Error = Some (sprintf "Could not find type of %s" nodeType.Name) }, Cmd.none

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
                sidebarView [ Page.Source; Page.Population; Page.Exposure; Page.Outcome ] dispatch

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
                        | Page.Source -> div [] [

                            div [] [ 
                                text "Make a source node."
                                ViewGen.makeNodeForm<Sources.SourceNode> (model.NodeCreationViewModels |> Map.tryFind "SourceNode") (FormMessage >> dispatch)
                                textf "%A" model.NodeCreationViewModels
                                textf "\n Model is: %A" model.Error
                            ]

                            label [] [ text "Where is the graph database stored?" ]
                            button [ on.click (fun _ -> SelectFolder |> dispatch)] [ text "Select folder" ]

                            // RawHtml (GraphVisualisation.view model.Graph)
                            p [] [ text "This tool allows coding information from bibliographic sources directly into a graph database." ]

                            div [ _class "card" ] [
                                div [ _class "card-header" ] [ text "Selected source" ]
                                div [ _class "card-body" ] [
                                    select [] [(
                                        cond model.Graph <| function
                                        | Some g ->
                                            cond (g.Nodes<Sources.SourceNode>()) <| function
                                            | Some sources ->
                                                sources
                                                |> Seq.map(fun k ->
                                                    option [ attr.name k.Key ] [ text k.Value ])
                                                |> Seq.toList
                                                |> concat
                                            | None -> empty
                                        | None -> empty
                                    )]
                                ]
                            ]

                            div [ _class "card" ] [
                                div [ _class "card-header" ] [ text "Data coding" ]

                                // Generate a form for coding data.
                                // 1. Source information - DONE.

                                // 2. Study timelines.

                                p [] [ text "Add a study timeline." ]
                                div [ _class "card" ] [
                                    label [] [ text "Start year" ]
                                    div [ _class "input-group mb-3" ] [
                                        input [ attr.``type`` "number";  ]
                                        span [ _class "input-group-text" ] [ text "years before present" ]
                                    ]
                                    label [] [ text "End year" ]
                                    div [ _class "input-group mb-3" ] [
                                        input [ attr.``type`` "number";  ]
                                        span [ _class "input-group-text" ] [ text "years before present" ]
                                    ]
                                    select [] [
                                        option [] [ text "Continuous" ]
                                        option [] [ text "Discontinuous" ]
                                    ]
                                    small [] [ text "A timeline is continuous if there are no breaks within the record. There may be breaks (hiatuses), for example a period during which deposition halted within a sedimentary sequence." ]
                                    cond (true (* Is this discontinuous? *)) <| fun _ ->
                                        div [] [
                                            text "Specify one or more hiatuses."
                                        ]
                                ]

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

                            div [ _class "card" ] [
                                div [ _class "card-header" ] [ text "Import new sources" ]
                                text "Enter a bibtex-format file below."
                                textarea [ bind.input.string model.Import (fun s -> ChangeImportText s |> dispatch) ] []
                                button [ on.click (fun _ -> ImportBibtex |> dispatch ) ] [ text "Import" ]
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