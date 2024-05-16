namespace BiodiversityCoder.Core

type NodeViewModel =
    | DU of string * NodeViewModel
    | Fields of Map<string,NodeViewModel>
    | FieldValue of SimpleValue
    | FieldList of (int * NodeViewModel) list
    | NotEnteredYet

type RelationViewModel =
    | ThisIsSource of sinkKey:Graph.UniqueKey * GraphStructure.ProposedRelation
    | ThisIsSink of sourceKey:Graph.UniqueKey * GraphStructure.ProposedRelation

type FormMessage =
    | EnterNodeCreationData of string * NodeViewModel
    | RelateNodes of Graph.UniqueKey * Graph.UniqueKey * GraphStructure.ProposedRelation
    | AddOrUpdateNode of System.Type * validateRelations:(seq<GraphStructure.ProposedRelation> -> bool) * requiredRelations:RelationViewModel list
    | EnterNodeRelationData of nodeType:string * toggle:string * GraphStructure.ProposedRelation * name:string * sinkKeys:Graph.UniqueKey list
    | ChangeNodeRelationToggle of nodeType:string * toggle:string
    /// Allows entry of a value from which to compute the relationship
    | EnterRelationCreationData of nodeType:string * toggle:string * fieldName:string * enteredValue:NodeViewModel * rel:GraphStructure.ProposedRelation option

module Create =
    
    open Microsoft.FSharp.Reflection
    open System.Reflection

    type ReflectiveListBuilder = 
        static member BuildList<'a> (args: obj list) = 
            [ for a in args do yield a :?> 'a ] 
        static member BuildTypedList lType (args: obj list) = 
            typeof<ReflectiveListBuilder>
                .GetMethod("BuildList")
                .MakeGenericMethod([|lType|])
                .Invoke(null, [|args|])

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

    let processField field propertyType createFromViewModel =
        match field with
        | FieldValue _ -> createFromViewModel propertyType field
        | NotEnteredYet -> Error "A value was missing"
        | Fields newFields ->
            // A record within a record field.
            createFromViewModel propertyType (Fields newFields)
        | DU (case, vm2) -> 
            // Pass through to main method to handle as new type.
            if FSharpType.IsUnion(propertyType) then
                match FSharpType.GetUnionCases(propertyType) |> Seq.tryFind(fun x -> x.Name = case) with
                | Some caseInfo -> 
                    // The case exists. Run this properly.
                    createFromViewModel caseInfo.DeclaringType (DU(case,vm2))
                | None -> Error "The DU case as specified in the view model does not exist"
            else Error "The type is not a DU as specified in the view model"
        | FieldList newFields -> createFromViewModel propertyType (FieldList newFields)

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
                        caseInfo.GetFields() |> Array.mapi(fun i f ->
                            // Multiple fields specified for constructing DU case.
                            match newFields |> Map.tryFind f.Name with
                            | Some newField -> createFromViewModel f.PropertyType newField
                            | None -> Error (sprintf "Value not found for DU field %s" f.Name)
                        ) |> Array.toList |> Result.ofList
                    | FieldList fl ->
                        match caseInfo.GetFields().Length with
                        | 1 -> 
                            if (objType.IsGenericType && objType.GetGenericTypeDefinition() = typedefof<_ list>)
                            then
                                // Is a list<'a>. Collect 'fields' into items for the list.
                                let listInstanceType = objType.GetProperty("Head").PropertyType
                                fl 
                                |> Seq.map(fun (i,vm) -> processField vm listInstanceType createFromViewModel)
                                |> Seq.toList
                                |> Result.ofList
                                |> Result.map(fun l -> [l :> obj])
                            else Error "Not a list type"
                        | _ -> Error "A list was specified but the DU does not have only one list type field"
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
                | Number n -> 
                    if objType = typeof<int> then int n :> obj |> Ok
                    else n :> obj |> Ok
                | Text t -> t :> obj |> Ok
                | Date t -> t :> obj |> Ok
                | Time t -> t :> obj |> Ok
                | Boolean t -> t :> obj |> Ok
        | Fields newFields ->
            // Fields - when specified not under a DU - represent an F# record.
            if Reflection.FSharpType.IsRecord(objType) then
                let recordFields = Reflection.FSharpType.GetRecordFields(objType)
                let values = recordFields |> Array.map(fun f ->
                    if newFields |> Map.containsKey f.Name 
                    then processField (newFields.[f.Name]) f.PropertyType createFromViewModel
                    else Error "Some properties are missing" ) |> Array.toList |> Result.ofList
                match values with
                | Ok values ->
                    let created = Reflection.FSharpValue.MakeRecord(objType, values |> List.toArray)
                    created |> Ok
                | Error e -> Error e
            else Error "Not a list or a record type"
        | FieldList fl ->
            if (objType.IsGenericType && objType.GetGenericTypeDefinition() = typedefof<_ list>)
            then
                // Is a list<'a>. Collect 'fields' into items for the list.
                let listInstanceType = objType.GetProperty("Head").PropertyType                
                fl 
                |> Seq.map(fun (i,vm) -> processField vm listInstanceType createFromViewModel)
                |> Seq.toList
                |> Result.ofList
                |> Result.map(fun ol -> ReflectiveListBuilder.BuildTypedList listInstanceType ol)
            else Error "Not a list type"
    

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
            | FieldList _
            | Fields _ -> DU(case1, updateNodeViewModel newValue newValue)
        | NotEnteredYet -> NotEnteredYet
        | FieldValue v -> FieldValue v
        | Fields (newFields: Map<string,NodeViewModel>) ->
            // Merge fields together if both field types.
            match master with
            | DU _
            | NotEnteredYet
            | FieldList _
            | FieldValue _ -> Fields(newFields |> Map.map(fun _ vm -> updateNodeViewModel NotEnteredYet vm))
            | Fields oldFields ->
                let k = newFields |> Seq.head
                Fields(
                    match oldFields |> Map.tryFind k.Key with
                    | Some oldFieldValue -> oldFields |> Map.add k.Key (updateNodeViewModel oldFieldValue k.Value)
                    | None -> oldFields |> Map.add k.Key k.Value )
        | FieldList fl ->
            match master with
            | FieldList oldFl ->
                let newData =
                    match oldFl |> Seq.tryFind(fun (i,_) -> i = fst fl.Head) with
                    | Some oldField -> 
                        if snd fl.Head = NotEnteredYet
                        then oldFl |> List.except [ oldField ]
                        else oldFl |> List.except [ oldField ] |> List.append [(fst oldField, updateNodeViewModel (snd oldField) (snd fl.Head))]
                    | None -> 
                        if snd fl.Head = NotEnteredYet 
                        then oldFl 
                        else oldFl |> List.append [ fl.Head ]
                    |> List.sortBy fst
                if newData.Length = 0 then NotEnteredYet else (FieldList newData)
            | _ ->  FieldList(fl |> List.map(fun (i,vm) -> i, updateNodeViewModel NotEnteredYet vm))

module ViewGen =

    open Bolero.Html

    let _class = attr.``class``

    let isList (t:System.Type) = t.IsGenericType && t.GetGenericTypeDefinition() = typedefof<_ list>

    /// Generate help text from the `Help` attribute if specified.
    let genUnits (field: System.Reflection.PropertyInfo) =
        cond (field.GetCustomAttributes(typeof<UnitAttribute>, true) |> Seq.isEmpty) <| function
        | true -> empty
        | false ->
            span [ _class "input-group-text" ] [
                text (field.GetCustomAttributes(typeof<UnitAttribute>, true) |> Seq.head:?> HelpAttribute).Text ]

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

    let genStringInput field existingValue maxLength dispatch =
        div [ _class "input-group mb-3" ] [
            input [ attr.maxlength maxLength; bind.input.string (textValue existingValue) (fun s -> Text s |> FieldValue |> dispatch); _class "form-control" ]
            genUnits field
        ]

    let genFloatInput field existingValue dispatch =
        div [ _class "input-group mb-3" ] [
            input [ bind.input.float (numberValue existingValue) (fun s -> Number s |> FieldValue |> dispatch); _class "form-control" ]
            genUnits field
        ]

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
                | t when t = typeof<FieldDataTypes.Text.ShortText> -> genStringInput field existingValue 100 dispatch
                | t when t = typeof<FieldDataTypes.Text.Text> -> genStringInput field existingValue 9999 dispatch
                | t when t = typeof<FieldDataTypes.LanguageCode.LanguageCode> -> genStringInput field existingValue 2 dispatch
                | t when t = typeof<FieldDataTypes.Geography.Polygon> -> genStringInput field existingValue 1000 dispatch
                | t when t = typeof<FieldDataTypes.Geography.CoordinateDMS> -> genStringInput field existingValue 100 dispatch
                | t when t = typeof<FieldDataTypes.Author.Author> -> genStringInput field existingValue 2000 dispatch
                | t when t = typeof<FieldDataTypes.DigitalObjectIdentifier.DigitalObjectIdentifier> -> genStringInput field existingValue 150 dispatch
                | t when t = typeof<FieldDataTypes.IntRange.IntRange> -> genStringInput field existingValue 50 dispatch
                | t when t = typeof<FieldDataTypes.StratigraphicSequence.Depth> -> genFloatInput field existingValue dispatch
                | t when t = typeof<float> -> genFloatInput field existingValue dispatch
                | t when t = typeof<int> -> genFloatInput field existingValue dispatch
                | t when t = typeof<bool> -> genTrueFalseToggle existingValue dispatch
                | _ -> genStringInput field existingValue 9999 dispatch)
                genHelp field
            ]
        ]

    let listGroup field items =
        div [ _class "row mb-3" ] [
            label [ attr.``for`` (genName field); _class "col-sm-2 col-form-label" ] [ genName field ]
            div [ _class "col-sm-10" ] [
                div [ _class "list-group" ] [
                    forEach items <| fun i -> div [ _class "list-group-item" ] [ i ]
                    genHelp field
                ]
            ]
        ]

    let rec renderPropertyInfo f (field: System.Reflection.PropertyInfo) (nestedVm:NodeViewModel -> NodeViewModel) (dispatch:NodeViewModel->unit) makeField' =
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
                genField field (Some existingValue) (fun s -> (nestedVm s) |> dispatch)
            | FieldList l ->
                listGroup field [
                    forEach l <| fun k -> concat [
                        makeField' (Some field) (snd k) (fun vm -> nestedVm <| FieldList([fst k, vm])) (field.PropertyType.GetProperty("Head").PropertyType) dispatch
                        small [ on.click(fun _ -> (nestedVm (FieldList [fst k, NotEnteredYet])) |> dispatch) ] [ text "Remove this one" ]
                    ]
                    button [ _class "btn btn-secondary-outline"; on.click(fun _ -> (nestedVm (FieldList [(l |> List.map fst |> List.max) + 1, FieldValue(Number 1)] )) |> dispatch) ] [ text "Add another" ] ]
            | Fields _
            | NotEnteredYet ->
                cond (isList field.PropertyType) <| function
                | true ->
                    listGroup field [ 
                        button [ _class "btn btn-secondary-outline"; on.click(fun _ -> (nestedVm (FieldList [(0, FieldValue(Number 1))])) |> dispatch) ] [ text "Add another" ] ]
                | false ->
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
                                renderPropertyInfo f field (fun vm -> nestedVm <| Fields([field.Name, vm] |> Map.ofList)) dispatch makeField'
                        | false ->
                            // Is not a DU or record. Render simple field.
                            genField field None (fun s -> (nestedVm s) |> dispatch)
        | None ->
            // Field does not have an existing value. Render cleanly.
            cond (isList field.PropertyType) <| function
                | true ->
                    listGroup field [
                        button [ _class "btn btn-secondary-outline"; on.click(fun _ -> (nestedVm (FieldList [(0, FieldValue(Number 1))] )) |> dispatch) ] [ text "Add another" ] ]
                | false ->
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
                                renderPropertyInfo f field (fun vm -> nestedVm <| Fields([field.Name, vm] |> Map.ofList)) dispatch makeField'
                        | false ->
                            // Is not a DU or record. Render simple field.
                            genField field None (fun s -> (nestedVm s) |> dispatch)

    /// Generate form fields corresponding to a nested node view model.
    /// Each level may be a DU, which leads to generation of a select box with case options
    /// and - if a case is selected - generated fields.
    let rec makeField (formId: string) (field:System.Reflection.PropertyInfo option) viewModel (nestedVm:NodeViewModel -> NodeViewModel) nestedType dispatch =
        cond (Reflection.FSharpType.IsUnion(nestedType)) <| function
        | true -> 
            // Is a DU. Display a select box to select possible cases.
                concat [
                    cond viewModel <| function
                    | DU (selectedCase: string, vm) ->
                        // A DU case is selected. Display its fields for editing.
                        concat [
                            genSelect field nestedType selectedCase (fun (s: string) -> (nestedVm(DU(s,vm))) |> dispatch)
                            cond (Reflection.FSharpType.GetUnionCases(nestedType) |> Seq.tryFind(fun c -> c.Name = selectedCase)) <| function
                            | Some s ->
                                // Get all of the field's values
                                cond vm <| function
                                | Fields f ->
                                    forEach (s.GetFields()) <| fun field -> 
                                        cond (f |> Map.tryFind field.Name) <| function
                                        | Some fv -> 
                                            cond fv <| function
                                            | Fields fv ->
                                                renderPropertyInfo fv field (fun vm -> nestedVm <| DU(selectedCase, Fields([field.Name, vm] |> Map.ofList))) dispatch (makeField formId)
                                            | _ -> renderPropertyInfo f field (fun vm -> nestedVm <| DU(selectedCase, Fields([field.Name, vm] |> Map.ofList))) dispatch (makeField formId)
                                        | None -> renderPropertyInfo f field (fun vm -> nestedVm <| DU(selectedCase, Fields([field.Name, vm] |> Map.ofList))) dispatch (makeField formId)
                                | _ ->
                                    // None of the fields have any entered values yet. Render all of them cleanly.
                                    forEach (s.GetFields()) <| fun field -> renderPropertyInfo Map.empty field (fun vm -> nestedVm <| DU(selectedCase, Fields([field.Name, vm] |> Map.ofList))) dispatch (makeField formId)
                            | None -> empty ]
                    | NotEnteredYet
                    | _ ->
                        if Reflection.FSharpType.GetUnionCases(nestedType).Length <> 1 then
                            // Nothing entered yet. Display an empty select box for the case.
                            // A DU case is not yet selected. Don't display any fields.
                            genSelect field nestedType "" (fun s -> (nestedVm(DU(s,NotEnteredYet))) |> dispatch)
                        else 
                            // There is only one DU case, so skip rendering the choice.
                            cond (Reflection.FSharpType.GetUnionCases(nestedType) |> Seq.tryFind(fun c -> c.Name = (Reflection.FSharpType.GetUnionCases(nestedType).[0].Name))) <| function
                            | Some s ->
                                // None of the fields have any entered values yet. Render all of them cleanly.
                                forEach (s.GetFields()) <| fun field -> renderPropertyInfo Map.empty field (fun vm -> nestedVm <| DU((Reflection.FSharpType.GetUnionCases(nestedType).[0].Name), Fields([field.Name, vm] |> Map.ofList))) dispatch (makeField formId)
                            | None -> empty
                        ]
        | false -> // Is not a DU.
            cond (Reflection.FSharpType.IsRecord(nestedType)) <| function
            | true -> // Is an F# record. Generate display for all fields.
                cond viewModel <| function
                | Fields f ->
                    // Fields already have some data entered. Render with existing values.
                    forEach (Reflection.FSharpType.GetRecordFields(nestedType)) <| fun field ->
                        renderPropertyInfo f field (fun vm -> nestedVm <| Fields([field.Name, vm] |> Map.ofList)) dispatch (makeField formId)
                | _ ->
                    // Fields have no data entered yet. Render using blank field view model.
                    forEach (Reflection.FSharpType.GetRecordFields(nestedType)) <| fun field ->
                        renderPropertyInfo Map.empty field (fun vm -> nestedVm <| Fields([field.Name, vm] |> Map.ofList)) dispatch (makeField formId)
            | false -> empty // Unsupported type (not a record or DU).

    let makeNodeForm'<'a> (nodeViewModel: NodeViewModel option) buttonName dispatch validateRelations (requiredRelations:RelationViewModel list) =
        div [ _class "simple-box" ] [
            cond nodeViewModel <| function
            | Some vm -> makeField (typeof<'a>).Name None vm id typeof<'a> (fun vm -> EnterNodeCreationData((typeof<'a>).Name,vm) |> dispatch)
            | None -> makeField (typeof<'a>).Name None NotEnteredYet id typeof<'a> (fun vm -> EnterNodeCreationData((typeof<'a>).Name,vm) |> dispatch)
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

    /// Generate a list of select options based on available nodes in the graph.
    let optionGenFiltered<'node> filter (graph:Storage.FileBasedGraph<GraphStructure.Node,GraphStructure.Relation> option) =
        cond graph <| function
        | Some g ->
            cond (g.Nodes<'node>()) <| function
                | None -> empty
                | Some nodes ->
                    forEach nodes <| fun node ->
                        cond (filter node.Key) <| function
                        | true -> option [ attr.value node.Key.AsString ] [ text node.Value ]
                        | false -> empty
        | None -> empty


    module RelationsForms =

        open Exposure
        open Microsoft.FSharp.Reflection

        ///Returns the case name of the object with union type 'ty.
        let unionCaseName (x:'a) = 
            match FSharpValue.GetUnionFields(x, typeof<'a>) with
            | case, _ -> case.Name  

        /// Render a node relation field item.
        let selectExistingNode<'sinkNodeType> name helpText (rel:GraphStructure.ProposedRelation) sourceNodeName toggle (relationValues:Map<string * GraphStructure.ProposedRelation, Graph.UniqueKey list>) graph dispatch =
            div [ _class "row mb-3" ] [
                label [ _class "col-sm-2 col-form-label" ] [ text name ]
                div [ _class "col-sm-10" ] [
                    cond (relationValues |> Map.tryFind (name, rel)) <| function
                    | Some v -> 
                        cond v.IsEmpty <| function
                        | true -> select [ _class "form-select"; bind.change.string "" (fun v -> EnterNodeRelationData(sourceNodeName, toggle, rel, name, [Graph.stringToKey v])|> dispatch ) ] [ optionGen<'sinkNodeType> (Some graph) ]
                        | false -> select [ _class "form-select"; bind.change.string v.Head.AsString (fun v -> EnterNodeRelationData(sourceNodeName, toggle, rel, name, [Graph.stringToKey v])|> dispatch ) ] [ optionGen<'sinkNodeType> (Some graph) ]
                    | None -> select [ _class "form-select"; bind.change.string "" (fun v -> EnterNodeRelationData(sourceNodeName, toggle, rel, name, [Graph.stringToKey v]) |> dispatch ) ] [ optionGen<'sinkNodeType> (Some graph) ]
                    small [ _class "form-text" ] [ text helpText ]
                ]
            ]

        /// Allows selecting an existing node by the entry of a specific data. Data is passed
        /// to the relation, allowing for construction of complex relation data.
        let selectExistingBy<'sinkNodeType, 'relArg> name helpText (rel:'relArg -> GraphStructure.ProposedRelation) (tryCompute:'relArg -> (Graph.UniqueKey * 'relArg) option) (relationVms:Map<string * string,Map<string, NodeViewModel * GraphStructure.ProposedRelation option>>) sourceNodeName toggle (relationValues:Map<string * GraphStructure.ProposedRelation, Graph.UniqueKey list>) graph dispatch =
            div [ _class "row mb-3" ] [
                div [ _class "col-sm-2 col-form-label" ] [ label [] [ text name ] ]
                div [ _class "col-sm-10" ] [
                    cond (relationVms |> Map.tryFind (sourceNodeName, toggle)) <| function
                    | Some existing ->
                        cond (existing |> Map.tryFind name) <| function
                        | Some (vm, proposed) ->
                            cond proposed <| function
                            | Some p ->
                                // There is a set value for this field. Only show the selected value with destroy option.
                                cond (relationValues |> Map.tryFind (name,p)) <| function
                                | Some all ->
                                    concat [
                                        forEach all <| fun v ->
                                            textf "Selected node is %s" v.AsString
                                        button [ _class "btn btn-secondary"; on.click (fun _ -> 
                                            EnterNodeRelationData(sourceNodeName, toggle, p, name, []) |> dispatch
                                            EnterRelationCreationData(sourceNodeName, toggle, name, NotEnteredYet, None) |> dispatch)
                                        ] [ text "Change" ]
                                    ]
                                | None -> text "Error finding selected node"
                            | None ->
                                // There is no existing relation. Try and form one.
                                concat [
                                    makeField (typeof<'relArg>).Name None vm id typeof<'relArg> (fun vm -> EnterRelationCreationData(sourceNodeName, toggle, name, vm, None) |> dispatch)
                                    button [
                                        _class "btn btn-secondary"
                                        on.click(fun _ -> 
                                            Create.createFromViewModel typeof<'relArg> vm
                                            |> Result.lift (fun r -> r :?> 'relArg)
                                            |> Result.bind (tryCompute >> Result.ofOption "Could not create a link")
                                            |> Result.mapError(fun e ->
                                                EnterRelationCreationData(sourceNodeName, toggle, e, vm, None) |> dispatch
                                                )
                                            |> Result.iter(fun (key, r) -> 
                                                EnterRelationCreationData(sourceNodeName, toggle, name, vm, Some (rel r)) |> dispatch
                                                EnterNodeRelationData(sourceNodeName, toggle, rel r, name, [key]) |> dispatch))
                                    ] [ text "Link" ]
                                ]
                        | None -> makeField (typeof<'relArg>).Name None NotEnteredYet id typeof<'relArg> (fun vm -> EnterRelationCreationData(sourceNodeName, toggle, name, vm, None) |> dispatch)
                    | None -> makeField (typeof<'relArg>).Name None NotEnteredYet id typeof<'relArg> (fun vm -> EnterRelationCreationData(sourceNodeName, toggle, name, vm, None) |> dispatch)
                    small [ _class "form-text" ] [ text helpText ]
                ]
            ]


        /// Render a node relation field item.
        /// Allows entry of multiple sink nodes for this type of relation.
        let selectExistingNodeMulti<'sinkNodeType> (name: string) helpText (rel:GraphStructure.ProposedRelation) sourceNodeName toggle (relationValues:Map<string * GraphStructure.ProposedRelation, Graph.UniqueKey list>) graph dispatch =
            div [ _class "row mb-3" ] [
                label [ _class "col-sm-2 col-form-label" ] [ text name ]
                div [ _class "col-sm-10" ] [
                    cond (relationValues |> Map.tryFind (name, rel)) <| function
                    | Some all -> concat [
                        forEach all <| fun v ->
                            select [ _class "form-select"; bind.change.string v.AsString (fun v -> EnterNodeRelationData(sourceNodeName, toggle, rel, name, (Graph.stringToKey v :: (all |> List.except [Graph.stringToKey v]))) |> dispatch ) ] [ optionGen<'sinkNodeType> (Some graph) ]
                        select [ _class "form-select"; bind.change.string "" (fun v -> EnterNodeRelationData(sourceNodeName, toggle, rel, name, (Graph.stringToKey v :: (all |> List.except [Graph.stringToKey v]))) |> dispatch ) ] [ optionGen<'sinkNodeType> (Some graph) ] ]
                    | None -> select [ _class "form-select"; bind.change.string "" (fun v -> EnterNodeRelationData(sourceNodeName, toggle, rel, name, [Graph.stringToKey v]) |> dispatch ) ] [ optionGen<'sinkNodeType> (Some graph) ]
                    small [ _class "form-text" ] [ text helpText ]
                ]
            ]
        
        /// Render a toggle for different combinations of possible relations.
        let relationsToggle<'a> name (elements: (string * (string -> string -> Map<string * GraphStructure.ProposedRelation,Graph.UniqueKey list> -> Storage.FileBasedGraph<GraphStructure.Node,GraphStructure.Relation> -> (FormMessage -> unit) -> Bolero.Node) list) list) (currentRelations: Map<string,string * Map<string*GraphStructure.ProposedRelation,list<Graph.UniqueKey>>>) graph dispatch =
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
                                | Some (_,n) -> n |> List.map(fun n -> n (typeof<'a>.Name) toggleSet relationValues graph dispatch) |> concat
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
                            (elements.Head |> snd) |> List.map(fun n -> n (typeof<'a>.Name) (fst elements.Head) Map.empty graph dispatch) |> concat
                        ]

        module Validation =

            /// Assumes that relations are nested once.
            let hasOneByCase (case:string) (relations:GraphStructure.ProposedRelation seq) = 
                relations |> Seq.where(fun r ->
                    let info2, values = FSharpValue.GetUnionFields(r, typeof<GraphStructure.ProposedRelation>)
                    let info3 = info2.GetFields()
                    if info3.Length <> 1 then false
                    else
                        let nestedCase, _ = FSharpValue.GetUnionFields(values.[0], info3.[0].PropertyType)
                        case = nestedCase.Name
                ) |> Seq.length = 1

            let hasOne i relations = relations |> Seq.where(fun r -> r = i) |> Seq.length = 1
