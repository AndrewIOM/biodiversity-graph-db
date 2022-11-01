namespace BiodiversityCoder.Core

type NodeViewModel =
    | DU of string * NodeViewModel
    | Fields of Map<string,NodeViewModel>
    | FieldValue of SimpleValue
    | NotEnteredYet

type RelationViewModel =
    | ThisIsSource of sinkKey:Graph.UniqueKey * GraphStructure.ProposedRelation
    | ThisIsSink of sourceKey:Graph.UniqueKey * GraphStructure.ProposedRelation

type FormMessage =
    | EnterNodeCreationData of string * NodeViewModel
    | RelateNodes of Graph.UniqueKey * Graph.UniqueKey * GraphStructure.ProposedRelation
    | AddOrUpdateNode of System.Type * validateRelations:(seq<GraphStructure.ProposedRelation> -> bool) * requiredRelations:RelationViewModel list
    | EnterNodeRelationData of nodeType:string * toggle:string * GraphStructure.ProposedRelation * sinkKeys:Graph.UniqueKey list
    | ChangeNodeRelationToggle of nodeType:string * toggle:string

module Create =
    
    open Microsoft.FSharp.Reflection
    open System.Reflection

    let private (|SomeObj|_|) =
        let ty: System.Type = typedefof<option<_>>
        fun (a:obj) ->
            let aty = a.GetType()
            let v = aty.GetProperty("Value")
            if aty.IsGenericType && aty.GetGenericTypeDefinition() = ty then
                if a = null then None
                else Some(v.GetValue(a, [| |]))
            else None

    let private bind (x : obj) rest =   
        match x with    
        | SomeObj(x1) -> Ok (rest x1)
        | _ -> Error "Invalid type"

    let rec createFromViewModel (objType:System.Type) (viewModel: NodeViewModel) : Result<obj,string> = 
        printfn "Type passed in is %s" objType.Name
        match viewModel with
        | DU (case1,newValue) -> 
            // Find existing case with this name.
            match FSharpType.GetUnionCases(objType) |> Seq.tryFind(fun x -> x.Name = case1) with
            | Some caseInfo ->
                // Get value for each field in the union case.
                let args : Result<obj list, string> =
                    (match newValue with
                    | NotEnteredYet -> 
                        // The DU may have no fields, in which case this is valid.
                        match caseInfo.GetFields().Length with
                        | 0 -> Ok []
                        | _ -> Error "No DU information selected."
                    | DU (case2, newValue2) -> 
                        // Single DU field specified for making a single DU field.
                        match caseInfo.GetFields().Length with
                        | 1 -> 
                            let propInfo = caseInfo.GetFields().[0]
                            if FSharpType.IsUnion(propInfo.PropertyType) then
                                createFromViewModel propInfo.PropertyType (DU(case2, newValue2))
                                |> Result.lift (fun i -> [ i ])
                                // match FSharpType.GetUnionCases(propInfo.PropertyType) |> Seq.tryFind(fun x -> x.Name = case2) with
                                // | Some caseInfo -> 
                                //     // This is the case of the nested object.
                                //     createFromViewModel caseInfo newValue2
                                // | None -> Error "The DU case as specified in the view model does not exist"
                            else Error "A non-DU type was specified on a DU field, but only a DU view model was given"
                        | _ -> Error "A DU view model was specified for a DU, but the parent DU does not have a single field."
                    | FieldValue _ -> 
                        // Single simple type field specified.
                        match caseInfo.GetFields().Length with
                        | 1 -> 
                            createFromViewModel (caseInfo.GetFields().[0].PropertyType) newValue
                            |> Result.lift (fun i -> [ i ])
                        | _ -> Error "Only a single value was given for a multi- or -zero field DU case."
                    | Fields (newFields: Map<string,NodeViewModel>) ->
                        // Multiple fields specified for constructing DU case.
                        caseInfo.GetFields() |> Array.map(fun f ->
                            match newFields |> Map.tryFind f.Name with
                            | Some newField -> createFromViewModel f.PropertyType newField
                            | None -> Error (sprintf "Value not found for DU field %s" f.Name)
                        ) |> Array.toList |> Result.ofList
                    )
                args |> Result.lift(fun args -> Reflection.FSharpValue.MakeUnion(caseInfo, args |> List.toArray))
            | None -> Error (sprintf "The DU case %s does not exist on this type." case1)
        | NotEnteredYet -> Error "No data has been entered yet"
        | FieldValue v ->
            // If the type has a Create static method, use this to make the value.
            // Otherwise, just pass through the value directly (if there is a type match..)
            printfn "Need to create from type %s" objType.Name
            let create = objType.GetMember("TryCreate")
            match create.Length with
            | 1 ->
                printfn "Create method is %A" create.[0]
                printfn "Creating with type %s" objType.Name
                // Make using Create member.
                let created = objType.InvokeMember("TryCreate", BindingFlags.InvokeMethod, null, null, [|v|])
                bind created id
            | _ ->
                printfn "Could not find create method for type %s" objType.Name
                match v with
                | Number n -> n :> obj |> Ok
                | Text t -> t :> obj |> Ok
                | Date t -> t :> obj |> Ok
                | Time t -> t :> obj |> Ok
                | Boolean t -> t :> obj |> Ok
        | Fields newFields ->
            // Fields - when specified not under a DU - represent an F# record ONLY.
            let recordFields = Reflection.FSharpType.GetRecordFields(objType)
            let values = recordFields |> Array.map(fun f ->
                if newFields |> Map.containsKey f.Name 
                then 
                    match newFields.[f.Name] with
                    | FieldValue _ -> createFromViewModel f.PropertyType newFields.[f.Name]
                    | NotEnteredYet -> Error "A value was missing"
                    | Fields newFields ->
                        // A record within a record field.
                        createFromViewModel f.PropertyType (Fields newFields)
                    | DU (case, vm2) -> 
                        // Pass through to main method to handle as new type.
                        if FSharpType.IsUnion(f.PropertyType) then
                            match FSharpType.GetUnionCases(f.PropertyType) |> Seq.tryFind(fun x -> x.Name = case) with
                            | Some caseInfo -> 
                                // The case exists. Run this properly.
                                createFromViewModel caseInfo.DeclaringType (DU(case,vm2))
                            | None -> Error "The DU case as specified in the view model does not exist"
                        else Error "The type is not a DU as specified in the view model"
                else Error "Some properties are missing" ) |> Array.toList |> Result.ofList
            match values with
            | Ok values ->
                let created = Reflection.FSharpValue.MakeRecord(objType, values |> List.toArray)
                created |> Ok
            | Error e -> Error e

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
                // if newFields.Count <> 1 then Fields newFields
                // else
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

    /// Generate the name from the `Name` attribute if specified.
    let genName (field: System.Reflection.PropertyInfo) =
        cond (field.GetCustomAttributes(typeof<NameAttribute>, true) |> Seq.isEmpty) <| function
        | true -> text field.Name
        | false -> text (field.GetCustomAttributes(typeof<NameAttribute>, true) |> Seq.head:?> NameAttribute).value

    /// Generate a select box for all possible cases when a DU.
    let genSelect (field: System.Reflection.PropertyInfo option) t selected dispatch =
        div [ _class "row mb-3" ] [
            cond field <| function
            | Some f -> label [ attr.``for`` f.Name; _class "col-sm-2 col-form-label" ] [ genName f ]
            | None -> label [ attr.``for`` "Select type"; _class "col-sm-2 col-form-label" ] [ text "Select type" ]
            div [ _class "col-sm-10" ] [
                select [ _class "form-select"; bind.change.string selected (fun s -> s |> dispatch)] [
                    forEach (Reflection.FSharpType.GetUnionCases(t)) <| fun c ->
                    option [ attr.value c.Name ] [ text c.Name ]
                ]
                cond field <| function
                | Some f -> genHelp f
                | None -> empty
            ]
        ]

    let textValue = function
        | Some v ->
            match v with
            | Text t -> t
            | _ -> ""
        | None -> ""

    let numberValue = function
        | Some v ->
            match v with
            | Number t -> t
            | _ -> 0
        | None -> 0

    let boolValue = function
        | Some v ->
            match v with
            | Boolean t -> t.ToString()
            | _ -> ""
        | None -> ""

    let genStringInput existingValue maxLength dispatch =
        input [ attr.maxlength maxLength; bind.input.string (textValue existingValue) (fun s -> Text s |> FieldValue |> dispatch); _class "form-control" ]

    let genFloatInput existingValue dispatch =
        input [ bind.input.float (numberValue existingValue) (fun s -> Number s |> FieldValue |> dispatch); _class "form-control" ]

    let genTrueFalseToggle existingValue dispatch =
        select [ _class "form-select"; bind.change.string (boolValue existingValue) (fun b -> Boolean (bool.Parse(b)) |> FieldValue |> dispatch) ] [
            option [ attr.value "False" ] [ text "No" ]
            option [ attr.value "True" ] [ text "Yes" ]
        ]

    let genField (field:System.Reflection.PropertyInfo) existingValue dispatch =
        div [ _class "row mb-3" ] [
            label [ attr.``for`` field.Name; _class "col-sm-2 col-form-label" ] [ genName field ]
            div [ _class "col-sm-10" ] [
                // TODO Make dynamic based on information held by the type, rather than having to specify all types manually.
                (match field.PropertyType with
                | t when t = typeof<FieldDataTypes.Text.ShortText> -> genStringInput existingValue 100 dispatch
                | t when t = typeof<FieldDataTypes.Text.Text> -> genStringInput existingValue 9999 dispatch
                | t when t = typeof<FieldDataTypes.LanguageCode.LanguageCode> -> genStringInput existingValue 2 dispatch
                | t when t = typeof<float> -> genFloatInput existingValue dispatch
                | t when t = typeof<int> -> genFloatInput existingValue dispatch
                | t when t = typeof<bool> -> genTrueFalseToggle existingValue dispatch
                | _ -> genStringInput existingValue 9999 dispatch)
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
                genField field (Some existingValue) (fun s -> EnterNodeCreationData(formId,nestedVm s) |> dispatch)
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
                        genField field None (fun s -> EnterNodeCreationData(formId,nestedVm s) |> dispatch)
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
                    genField field None (fun s -> EnterNodeCreationData(formId,nestedVm s) |> dispatch)

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

    let makeNodeForm'<'a> (nodeViewModel: NodeViewModel option) buttonName dispatch validateRelations (requiredRelations:RelationViewModel list) =
        div [ _class "simple-box" ] [
            cond nodeViewModel <| function
            | Some vm -> makeField (typeof<'a>).Name None vm id typeof<'a> dispatch
            | None -> makeField (typeof<'a>).Name None NotEnteredYet id typeof<'a> dispatch
            button [ _class "btn btn-primary"; on.click (fun _ -> AddOrUpdateNode((typeof<'a>), validateRelations, requiredRelations) |> dispatch) ] [ text buttonName ]
        ]

    /// Given a node type (maybe a DU or record or normal type),
    /// generate a set of fields to use for inputting the data.
    /// Also, generate a placeholder area for the data to bind to in the
    /// view model, which can then be submitted.
    let makeNodeForm<'a> (nodeViewModel: NodeViewModel option) requiredRelations dispatch =
        makeNodeForm'<'a> nodeViewModel "Create" dispatch (fun _ -> true) requiredRelations

    /// Makes a form to create a new node where relations are also required.
    let makeNodeFormWithRelations<'a> validateRelations (nodeViewModel: NodeViewModel option) requiredRelations dispatch =
        makeNodeForm'<'a> nodeViewModel "Create" dispatch validateRelations requiredRelations

    /// Generate a list of select options based on available nodes in the graph.
    let optionGen<'node> (graph:Storage.FileBasedGraph<GraphStructure.Node,GraphStructure.Relation> option) =
        cond graph <| function
        | Some g ->
            cond (g.Nodes<'node>()) <| function
                | None -> empty
                | Some nodes ->
                    forEach nodes <| fun node ->
                        option [ attr.value node.Key.AsString ] [ text node.Value ]
        | None -> empty


    module RelationsForms =

        open Exposure
        open Microsoft.FSharp.Reflection

        ///Returns the case name of the object with union type 'ty.
        let unionCaseName (x:'a) = 
            match FSharpValue.GetUnionFields(x, typeof<'a>) with
            | case, _ -> case.Name  

        /// Render a node relation field item.
        let selectExistingNode<'sinkNodeType> name (rel:GraphStructure.ProposedRelation) toggle (relationValues:Map<GraphStructure.ProposedRelation, Graph.UniqueKey list>) graph dispatch =
            div [ _class "row mb-3" ] [
                label [ _class "col-sm-2 col-form-label" ] [ text name ]
                div [ _class "col-sm-10" ] [
                    cond (relationValues |> Map.tryFind rel) <| function
                    | Some v -> 
                        cond v.IsEmpty <| function
                        | true -> select [ _class "form-select"; bind.change.string "" (fun v -> EnterNodeRelationData(typeof<'sinkNodeType>.Name, toggle, rel, [Graph.stringToKey v])|> dispatch ) ] [ optionGen<'sinkNodeType> (Some graph) ]
                        | false -> select [ _class "form-select"; bind.change.string v.Head.AsString (fun v -> EnterNodeRelationData(typeof<'sinkNodeType>.Name, toggle, rel, [Graph.stringToKey v])|> dispatch ) ] [ optionGen<'sinkNodeType> (Some graph) ]
                    | None -> select [ _class "form-select"; bind.change.string "" (fun v -> EnterNodeRelationData(typeof<'sinkNodeType>.Name, toggle, rel, [Graph.stringToKey v]) |> dispatch ) ] [ optionGen<'sinkNodeType> (Some graph) ]
                ]
            ]
        
        /// Render a node relation field item.
        /// Allows entry of multiple sink nodes for this type of relation.
        let selectExistingNodeMulti<'sinkNodeType> name (rel:GraphStructure.ProposedRelation) toggle (relationValues:Map<GraphStructure.ProposedRelation, Graph.UniqueKey list>) graph dispatch =
            div [ _class "row mb-3" ] [
                label [ _class "col-sm-2 col-form-label" ] [ text name ]
                div [ _class "col-sm-10" ] [
                    cond (relationValues |> Map.tryFind rel) <| function
                    | Some all -> concat [
                        forEach all <| fun v ->
                            select [ _class "form-select"; bind.change.string v.AsString (fun v -> EnterNodeRelationData(typeof<'sinkNodeType>.Name, toggle, rel, (Graph.stringToKey v :: (all |> List.except [Graph.stringToKey v]))) |> dispatch ) ] [ optionGen<'sinkNodeType> (Some graph) ]
                        select [ _class "form-select"; bind.change.string "" (fun v -> EnterNodeRelationData(typeof<'sinkNodeType>.Name, toggle, rel, (Graph.stringToKey v :: (all |> List.except [Graph.stringToKey v]))) |> dispatch ) ] [ optionGen<'sinkNodeType> (Some graph) ] ]
                    | None -> select [ _class "form-select"; bind.change.string "" (fun v -> EnterNodeRelationData(typeof<'sinkNodeType>.Name, toggle, rel, [Graph.stringToKey v]) |> dispatch ) ] [ optionGen<'sinkNodeType> (Some graph) ]
                ]
            ]
        
        /// Render a toggle for different combinations of possible relations.
        let relationsToggle<'a> name (elements: (string * (string -> Map<GraphStructure.ProposedRelation,Graph.UniqueKey list> -> Storage.FileBasedGraph<GraphStructure.Node,GraphStructure.Relation> -> (FormMessage -> unit) -> Bolero.Node) list) list) (currentRelations: Map<string,string * Map<GraphStructure.ProposedRelation,list<Graph.UniqueKey>>>) graph dispatch =
                    cond (currentRelations |> Map.tryFind (typeof<'a>.Name)) <| function
                    | Some (toggleSet, relationValues) -> concat [
                                cond (elements.Length > 1) <| function
                                | true ->
                                    div [ _class "row mb-3" ] [
                                        div [ _class "col-sm-2 col-form-label" ] [ label [] [text name ] ]
                                        div [ _class "col-sm-10" ] [
                                            div [ _class "btn-group" ] (elements |> List.map(fun (toggle,_) ->
                                                cond (toggle = toggleSet) <| function
                                                | true -> button [ _class "btn btn-outline-primary active"; on.click (fun _ -> ChangeNodeRelationToggle(typeof<'a>.Name, toggle) |> dispatch) ] [ text toggle ]
                                                | false -> button [ _class "btn btn-outline-primary";  on.click (fun _ -> ChangeNodeRelationToggle(typeof<'a>.Name, toggle) |> dispatch) ] [ text toggle ]
                                            ))
                                        ]
                                    ]
                                | false -> empty
                                cond (elements |> Seq.tryFind(fun (e,_) -> e = toggleSet)) <| function
                                | Some (_,n) -> n |> List.map(fun n -> n toggleSet relationValues graph dispatch) |> concat
                                | None -> empty
                        ]
                    | None -> 
                        concat [
                            cond (elements.Length > 1) <| function
                            | true ->
                                div [ _class "row" ] [
                                        div [ _class "col-sm-2 col-form-label" ] [ label [] [text name ] ]
                                        div [ _class "col-sm-10" ] [
                                            div [ _class "btn-group"  ] (elements |> List.mapi(fun i (toggle,_) ->
                                                    cond (i = 0) <| function
                                                | true -> button [ _class "btn btn-outline-primary active"; on.click (fun _ -> ChangeNodeRelationToggle(typeof<'a>.Name, toggle) |> dispatch) ] [ text toggle ]
                                                | false -> button [ _class "btn btn-outline-primary";  on.click (fun _ -> ChangeNodeRelationToggle(typeof<'a>.Name, toggle) |> dispatch) ] [ text toggle ]
                                            ))
                                        ]
                                ]
                            | false -> empty
                            (elements.Head |> snd) |> List.map(fun n -> n (fst elements.Head) Map.empty graph dispatch) |> concat
                        ]

        module Validation =

            let hasOne i relations = relations |> Seq.where(fun r -> r = i) |> Seq.length = 1
