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

    type NodeViewModel =
        | DU of string * NodeViewModel
        | Fields of Map<string,NodeViewModel>
        | FieldValue of string
        | NotEnteredYet

    type Model =
        {
            Page: Page
            Graph: Graph.Graph<GraphStructure.Node,GraphStructure.Relation>
            Import: string
            Error: string option
            NodesByType: Map<string, Map<string, string>>
            NodeCreationViewModels: Map<string, NodeViewModel>
        }

    let initModel =
        // TODO Replace seed with loading mechanism. Only seed if initing a new database.
        match Seed.initGraph() with
        | Ok g ->  { Graph = g; Import = ""; Error = None; Page = Source; NodesByType = Map.empty; NodeCreationViewModels = Map.empty }, Cmd.none
        | Error e -> { Graph = []; Import = ""; Error = Some e; Page = Source; NodesByType = Map.empty; NodeCreationViewModels = Map.empty }, Cmd.none

    type Message =
        | SetPage of Page
        | SelectFolder
        | SelectedFolder of string
        | SyncFileSystem
        | ChangeImportText of string
        | ImportBibtex
        | RelateNodes of string * string * GraphStructure.ProposedRelation
        | AddOrUpdateNode
        | AddProxiedTaxon of Population.ProxiedTaxon.ProxiedTaxonHyperEdge
        | EnterNodeCreationData of string * NodeViewModel

    /// Crawl the new view models by level from top to bottom, replacing
    /// data in the old master viewmodel or merging where appropriate.
    let rec updateNodeViewModel master (newMaster: NodeViewModel) = 
        match newMaster with
        | DU (case1,newValue) -> 
            match master with
            | DU(case2,_) -> 
                if case1 = case2 
                then DU(case2, updateNodeViewModel newValue newValue)
                else DU(case1, updateNodeViewModel NotEnteredYet newValue)
            | NotEnteredYet -> DU(case1, updateNodeViewModel newValue newValue)
            | FieldValue _
            | Fields _ -> DU(case1, updateNodeViewModel newValue newValue)
        | NotEnteredYet -> NotEnteredYet
        | FieldValue v -> FieldValue v
        | Fields newFields ->
            // Merge fields together if both field types.
            match master with
            | DU _
            | NotEnteredYet
            | FieldValue _ -> Fields(newFields |> Map.map(fun _ vm -> updateNodeViewModel NotEnteredYet vm))
            | Fields oldFields ->
                if newFields.Count <> 1 then Fields newFields
                else
                    let k = newFields |> Seq.head
                    let x = oldFields |> Map.add k.Key k.Value
                    Fields x

    let update (openFolder:unit -> Task<string>) message model =
        match message with
        | SetPage page -> { model with Page = page }, Cmd.none
        | SyncFileSystem -> model, Cmd.none
        | ChangeImportText s -> { model with Import = s }, Cmd.none
        | ImportBibtex -> 
            match BibtexParser.parse model.Import with
            | Error e -> { model with Error = Some e }, Cmd.none
            | Ok nodes ->
                { model with Graph = model.Graph |> Graph.addNodeData (nodes |> List.map GraphStructure.Node.SourceNode) |> fst }, Cmd.none
        | SelectFolder ->
            model, Cmd.OfAsync.result(async {
                let! folder = openFolder () |> Async.AwaitTask
                return SelectedFolder folder
            })
        | SelectedFolder folder ->
            match Storage.loadOrInitGraph folder with
            | Ok g -> { model with Graph = g }, Cmd.none
            | Error e -> { model with Error = Some e }, Cmd.none
        | RelateNodes(_, _, _) -> failwith "Not Implemented"
        | AddProxiedTaxon(_) -> failwith "Not Implemented"

        | EnterNodeCreationData(formId, vm) -> 
            let updatedVm = 
                match model.NodeCreationViewModels |> Map.tryFind formId with
                | Some formData -> updateNodeViewModel formData vm
                | None -> updateNodeViewModel NotEnteredYet vm
            { model with NodeCreationViewModels = model.NodeCreationViewModels |> Map.add formId updatedVm
                         Error = Some <| sprintf "%A" vm }, Cmd.none

        | AddOrUpdateNode -> 
            // 1. Gather fields required for this node (where are these stored in view model?)
            // 2. Validate by converting into internal graph types (e.g. Text.ShortText)
            // 3. Submit change to filesystem-based graph.
            // 4. Handle any errors or return OK.
            failwith "Not Implemented"

    // Can we generate a form field for node creation from a type definition for a node?
    module ViewGen =

        /// Generate a select box for all possible cases when a DU.
        let selectCase t selected dispatch =
            select [ bind.change.string selected (fun s -> s |> dispatch)] [
                forEach (Reflection.FSharpType.GetUnionCases(t)) <| fun c ->
                option [ attr.name c.Name ] [ text c.Name ]
            ]

        let genField (field:System.Reflection.PropertyInfo) binding =
            div [] [
                label [] [ text field.Name ]
                // TODO customise input depending on type
                input [ binding ]
            ]
            // let cre = field.PropertyType.GetMember("Create")
            // if cre.Length <> 1 then empty
            // else

        open Population

        let renderPropertyInfo f (field: System.Reflection.PropertyInfo) (nestedVm:NodeViewModel -> NodeViewModel) dispatch makeField' formId =
            // Figure out if the field already has a value.
            cond (f |> Map.tryFind field.Name) <| function
            | Some v ->
                // Field has an existing value
                cond v <| function
                | DU _ -> 
                    // Field is a DU with a select value already set. Render select box and possible value.
                    makeField' v nestedVm field.PropertyType dispatch
                | FieldValue existingValue -> 
                    // Field is a simple type with a value already set. Render with value.
                    genField field (bind.input.string (string existingValue) (fun s -> EnterNodeCreationData(formId,nestedVm(FieldValue(s))) |> dispatch))
                | Fields _
                | NotEnteredYet ->
                    // Field does not have an existing value. Render cleanly.
                    cond (Reflection.FSharpType.IsUnion(field.PropertyType)) <| function
                    | true -> 
                        // A field is another DU. Make a nested DU view model.
                        makeField' NotEnteredYet nestedVm field.PropertyType dispatch
                    | false -> 
                        // A field is not a DU. 
                        genField field (bind.input.string "" (fun s -> EnterNodeCreationData(formId,nestedVm(FieldValue(s))) |> dispatch))
            | None ->
                // Field does not have an existing value. Render cleanly.
                cond (Reflection.FSharpType.IsUnion(field.PropertyType)) <| function
                | true -> 
                    // A field is another DU. Make a nested DU view model.
                    makeField' NotEnteredYet nestedVm field.PropertyType dispatch
                | false -> 
                    // A field is not a DU. 
                    genField field (bind.input.string "" (fun s -> EnterNodeCreationData(formId,nestedVm(FieldValue(s))) |> dispatch))

        /// Generate form fields corresponding to a nested node view model.
        /// Each level may be a DU, which leads to generation of a select box with case options
        /// and - if a case is selected - generated fields.
        let rec makeField formId viewModel (nestedVm:NodeViewModel -> NodeViewModel) nestedType dispatch =
            cond (Reflection.FSharpType.IsUnion(nestedType)) <| function
            | true -> 
                // Is a DU. Display a select box to select possible cases.
                    concat [
                        cond viewModel <| function
                        | NotEnteredYet ->
                            // Nothing entered yet. Display an empty select box for the case.
                            // A DU case is not yet selected. Don't display any fields.
                            selectCase nestedType "" (fun s -> EnterNodeCreationData(formId,nestedVm(DU(s,NotEnteredYet))) |> dispatch)
                        | DU (selectedCase, vm) ->
                            // A DU case is selected. Display its fields for editing.
                            concat [
                                selectCase nestedType selectedCase (fun (s: string) -> EnterNodeCreationData(formId,nestedVm(DU(s,vm))) |> dispatch)
                                cond (Reflection.FSharpType.GetUnionCases(nestedType) |> Seq.tryFind(fun c -> c.Name = selectedCase)) <| function
                                | Some s ->
                                    // Get all of the field's values
                                    cond vm <| function
                                    | Fields f ->
                                        // TODO May be a simple value OR a record type (with lots of fields. Render this as nested fields)...
                                        forEach (s.GetFields()) <| fun field -> renderPropertyInfo f field (fun vm -> nestedVm <| Fields([field.Name, vm] |> Map.ofList)) dispatch (makeField formId) formId
                                    | _ ->
                                        // None of the fields have any entered values yet. Render all of them cleanly.
                                        forEach (s.GetFields()) <| fun field -> renderPropertyInfo Map.empty field (fun vm -> nestedVm <| Fields([field.Name, vm] |> Map.ofList)) dispatch (makeField formId) formId
                                | None -> empty ]
                        | _ -> 
                            // Shouldn't be able to have a field when it is a DU.
                            empty ]
            | false -> // Is not a DU.
                cond (Reflection.FSharpType.IsRecord(nestedType)) <| function
                | true -> // Is an F# record. Generate display for all fields.
                    cond viewModel <| function
                    | Fields f ->
                        // Fields already have some data entered. Render with existing values.
                        forEach (Reflection.FSharpType.GetRecordFields(nestedType)) <| fun field ->
                            renderPropertyInfo f field (fun vm -> nestedVm <| Fields([field.Name, vm] |> Map.ofList)) dispatch (makeField formId) formId
                    | _ ->
                        // Fields have no data entered yet. Render using blank field view model.
                        forEach (Reflection.FSharpType.GetRecordFields(nestedType)) <| fun field ->
                            renderPropertyInfo Map.empty field (fun vm -> nestedVm <| Fields([field.Name, vm] |> Map.ofList)) dispatch (makeField formId) formId
                | false -> empty // Unsupported type (not a record or DU).

        /// Given a node type (maybe a DU or record or normal type),
        /// generate a set of fields to use for inputting the data.
        /// Also, generate a placeholder area for the data to bind to in the
        /// view model, which can then be submitted.
        let makeNodeForm<'a> (nodeViewModel: NodeViewModel option) dispatch =
            div [] [
                cond nodeViewModel <| function
                | Some vm -> makeField (typeof<'a>).Name vm id typeof<'a> dispatch
                | None -> makeField (typeof<'a>).Name NotEnteredYet id typeof<'a> dispatch
                button [ on.click (fun _ -> AddOrUpdateNode |> dispatch) ] [ text "Create" ]
            ]

        /// Generate a list of select options based on available nodes in the graph.
        let optionGen nodeType (nodes:Map<string, Map<string, string>>) =
            cond (nodes |> Map.tryFind nodeType) <| function
                | None -> empty
                | Some nodes ->
                    forEach nodes <| fun node ->
                        option [ attr.value node.Key ] [ text node.Value ]


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
                        | Page.Population -> 
                            div [] [ 
                                text "Make a population context node."
                                ViewGen.makeNodeForm<Population.Context.ContextNode> (model.NodeCreationViewModels |> Map.tryFind "ContextNode") dispatch
                                textf "%A" model.NodeCreationViewModels
                                textf "\n Model is: %A" model.Error
                            ]
                        | Page.Exposure -> div [] [ text "Exposure page" ]
                        | Page.Outcome -> div [] [ text "Outcome page" ]
                        | Page.Source -> div [] [

                            div [] [ 
                                text "Make a source node."
                                ViewGen.makeNodeForm<Sources.SourceNode> (model.NodeCreationViewModels |> Map.tryFind "SourceNode") dispatch
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
                                    select [] (
                                        model.Graph 
                                        |> List.where(fun (n,_) -> 
                                                match snd n with 
                                                | GraphStructure.Node.SourceNode _ -> true 
                                                | _ -> false )
                                        |> List.map(fun n ->
                                            option [] [
                                            match snd (fst n) with
                                            | GraphStructure.Node.SourceNode s ->
                                                match s with
                                                | Sources.SourceNode.Bibliographic d -> d.Title.Value.Value
                                                | Sources.SourceNode.GreyLiterature d -> d.Title.Value
                                                | Sources.SourceNode.DarkData d -> d.Details.Value
                                            | _ -> "Error"
                                        |> text ])
                                    )
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
                                                    td [] [ select [] [ ViewGen.optionGen "BioticProxyNode" model.NodesByType ] ]
                                                    td [] [ select [] [ ViewGen.optionGen "InferenceMethodNode" model.NodesByType ] ]
                                                    td [] [ select [] [ ViewGen.optionGen "TaxonNode" model.NodesByType ] ]
                                                    td [] [ select [] [ ViewGen.optionGen "BiodiversityDimensionNode" model.NodesByType ] ]
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