namespace BiodiversityCoder.Core

type NodeViewModel =
    | DU of string * NodeViewModel
    | Fields of Map<string,NodeViewModel>
    | FieldValue of string
    | NotEnteredYet

type FormMessage =
    | RelateNodes of string * string * GraphStructure.ProposedRelation
    | AddOrUpdateNode
    | AddProxiedTaxon of Population.ProxiedTaxon.ProxiedTaxonHyperEdge
    | EnterNodeCreationData of string * NodeViewModel

module Merge =

    /// Crawl the new view models by level from top to bottom, replacing
    /// data in the old master viewmodel or merging where appropriate.
    let rec updateNodeViewModel master (newMaster: NodeViewModel) = 
        match newMaster with
        | DU (case1,newValue) -> 
            match master with
            | DU(case2,oldValue) -> 
                if case1 = case2 
                then DU(case2, updateNodeViewModel oldValue newValue)
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

module ViewGen =

    open Bolero.Html

    let _class = attr.``class``

    /// Generate help text from the `Help` attribute if specified.
    let genHelp (field: System.Reflection.PropertyInfo) =
        cond (field.GetCustomAttributes(typeof<HelpAttribute>, true) |> Seq.isEmpty) <| function
        | true -> empty
        | false -> small [ _class "form-text" ] [ 
            text (field.GetCustomAttributes(typeof<HelpAttribute>, true) |> Seq.head:?> HelpAttribute).Text ]

    /// Generate a select box for all possible cases when a DU.
    let genSelect (field: System.Reflection.PropertyInfo option) t selected dispatch =
        div [ _class "row mb-3" ] [
            cond field <| function
            | Some f -> label [ attr.``for`` f.Name; _class "col-sm-2 col-form-label" ] [ text f.Name ]
            | None -> label [ attr.``for`` "Select type"; _class "col-sm-2 col-form-label" ] [ text "Select type" ]
            div [ _class "col-sm-10" ] [
                select [ _class "form-select"; bind.change.string selected (fun s -> s |> dispatch)] [
                    forEach (Reflection.FSharpType.GetUnionCases(t)) <| fun c ->
                    option [ attr.name c.Name ] [ text c.Name ]
                ]
                cond field <| function
                | Some f -> genHelp f
                | None -> empty
            ]
        ]

    let genField (field:System.Reflection.PropertyInfo) binding =
        div [ _class "row mb-3" ] [
            label [ attr.``for`` field.Name; _class "col-sm-2 col-form-label" ] [ text field.Name ]
            div [ _class "col-sm-10" ] [
                // TODO customise input depending on type
                // TODO Add units of measure to box.
                input [ binding; _class "form-control" ]
                genHelp field
            ]
        ]

    let rec renderPropertyInfo f (field: System.Reflection.PropertyInfo) (nestedVm:NodeViewModel -> NodeViewModel) dispatch makeField' formId =
        // Figure out if the field already has a value.
        cond (f |> Map.tryFind field.Name) <| function
        | Some v ->
            // Field has an existing value
            cond v <| function
            | DU _ -> 
                // Field is a DU with a select value already set. Render select box and possible value.
                makeField' (Some field) v nestedVm field.PropertyType dispatch
            | FieldValue existingValue -> 
                // Field is a simple type with a value already set. Render with value.
                genField field (bind.input.string (string existingValue) (fun s -> EnterNodeCreationData(formId,nestedVm(FieldValue(s))) |> dispatch))
            | Fields _
            | NotEnteredYet ->
                // Field does not have an existing value. Render cleanly.
                cond (Reflection.FSharpType.IsUnion(field.PropertyType)) <| function
                | true -> 
                    // A field is another DU. Make a nested DU view model.
                    makeField' (Some field) NotEnteredYet nestedVm field.PropertyType dispatch
                | false -> 
                    // A field is not a DU. 
                    cond (Reflection.FSharpType.IsRecord(field.PropertyType)) <| function
                    | true ->
                        // Is a record. Render fields nested.
                        forEach (Reflection.FSharpType.GetRecordFields(field.PropertyType)) <| fun field ->
                            renderPropertyInfo f field (fun vm -> nestedVm <| Fields([field.Name, vm] |> Map.ofList)) dispatch makeField' formId
                    | false ->
                        // Is not a DU or record. Render simple field.
                        genField field (bind.input.string "" (fun s -> EnterNodeCreationData(formId,nestedVm(FieldValue(s))) |> dispatch))
        | None ->
            // Field does not have an existing value. Render cleanly.
            cond (Reflection.FSharpType.IsUnion(field.PropertyType)) <| function
            | true -> 
                // A field is another DU. Make a nested DU view model.
                makeField' (Some field) NotEnteredYet nestedVm field.PropertyType dispatch
            | false -> 
                // A field is not a DU. 
                cond (Reflection.FSharpType.IsRecord(field.PropertyType)) <| function
                | true ->
                    // Is a record. Render fields nested.
                    forEach (Reflection.FSharpType.GetRecordFields(field.PropertyType)) <| fun field ->
                        renderPropertyInfo f field (fun vm -> nestedVm <| Fields([field.Name, vm] |> Map.ofList)) dispatch makeField' formId
                | false ->
                    // Is not a DU or record. Render simple field.
                    genField field (bind.input.string "" (fun s -> EnterNodeCreationData(formId,nestedVm(FieldValue(s))) |> dispatch))

    /// Generate form fields corresponding to a nested node view model.
    /// Each level may be a DU, which leads to generation of a select box with case options
    /// and - if a case is selected - generated fields.
    let rec makeField (formId: string) (field:System.Reflection.PropertyInfo option) viewModel (nestedVm:NodeViewModel -> NodeViewModel) nestedType dispatch =
        cond (Reflection.FSharpType.IsUnion(nestedType)) <| function
        | true -> 
            // Is a DU. Display a select box to select possible cases.
                concat [
                    cond viewModel <| function
                    | NotEnteredYet ->
                        // Nothing entered yet. Display an empty select box for the case.
                        // A DU case is not yet selected. Don't display any fields.
                        genSelect field nestedType "" (fun s -> EnterNodeCreationData(formId,nestedVm(DU(s,NotEnteredYet))) |> dispatch)
                    | DU (selectedCase: string, vm) ->
                        // A DU case is selected. Display its fields for editing.
                        concat [
                            genSelect field nestedType selectedCase (fun (s: string) -> EnterNodeCreationData(formId,nestedVm(DU(s,vm))) |> dispatch)
                            cond (Reflection.FSharpType.GetUnionCases(nestedType) |> Seq.tryFind(fun c -> c.Name = selectedCase)) <| function
                            | Some s ->
                                // Get all of the field's values
                                cond vm <| function
                                | Fields f ->
                                    // TODO May be a simple value OR a record type (with lots of fields. Render this as nested fields)...
                                    forEach (s.GetFields()) <| fun field -> renderPropertyInfo f field (fun vm -> nestedVm <| DU(selectedCase, Fields([field.Name, vm] |> Map.ofList))) dispatch (makeField formId) formId
                                | _ ->
                                    // None of the fields have any entered values yet. Render all of them cleanly.
                                    forEach (s.GetFields()) <| fun field -> renderPropertyInfo Map.empty field (fun vm -> nestedVm <| DU(selectedCase, Fields([field.Name, vm] |> Map.ofList))) dispatch (makeField formId) formId
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
        form [ _class "simple-box" ] [
            cond nodeViewModel <| function
            | Some vm -> makeField (typeof<'a>).Name None vm id typeof<'a> dispatch
            | None -> makeField (typeof<'a>).Name None NotEnteredYet id typeof<'a> dispatch
            button [ _class "btn btn-primary"; on.click (fun _ -> AddOrUpdateNode |> dispatch) ] [ text "Create" ]
        ]

    /// Generate a list of select options based on available nodes in the graph.
    let optionGen nodeType (nodes:Map<string, Map<string, string>>) =
        cond (nodes |> Map.tryFind nodeType) <| function
            | None -> empty
            | Some nodes ->
                forEach nodes <| fun node ->
                    option [ attr.value node.Key ] [ text node.Value ]

