﻿namespace BiodiversityCoder.Core

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
            NodeCreationRelations: Map<string, string * Map<GraphStructure.ProposedRelation, Graph.UniqueKey list>>
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
        AddBioticHyperedge: Map<Graph.UniqueKey, Graph.UniqueKey option * Graph.UniqueKey option * Graph.UniqueKey option * Graph.UniqueKey option>
    }
    
    and EligbilityCriteria =
        | Include
        | Exclude of because:Sources.ExclusionReason * notes:FieldDataTypes.Text.Text

    let initModel =
        match Storage.loadOrInitGraph "/Users/andrewmartin/Desktop/test-graph/" with
        | Ok g ->  
            match (g.Nodes<Exposure.TemporalIndex.CalYearNode> ()) with
            | Some _ -> { TaxonLookup = { Rank = "Genus"; Family = ""; Genus = ""; Species = ""; Authorship = ""; Result = None }; NodeCreationRelations = Map.empty; SelectedSource = None; Graph = Some g; Import = ""; Error = None; Page = Extract; NodeCreationViewModels = Map.empty; NodeCreationValidationErrors = Map.empty }, Cmd.none
            | None ->
                match Storage.seedGraph g with
                | Ok seeded -> { TaxonLookup = { Rank = "Genus"; Family = ""; Genus = ""; Species = ""; Authorship = ""; Result = None }; NodeCreationRelations = Map.empty; SelectedSource = None; Graph = Some seeded; Import = ""; Error = None; Page = Extract; NodeCreationViewModels = Map.empty; NodeCreationValidationErrors = Map.empty }, Cmd.none
                | Error e -> { TaxonLookup = { Rank = "Genus"; Family = ""; Genus = ""; Species = ""; Authorship = ""; Result = None }; NodeCreationRelations = Map.empty; SelectedSource = None; Graph = None; Import = ""; Error = Some e; Page = Extract; NodeCreationViewModels = Map.empty; NodeCreationValidationErrors = Map.empty }, Cmd.none
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
        | SelectSource of key:Graph.UniqueKey
        | LookupTaxon of LookupTaxonMessage
        | ChangeProxiedTaxonVm of timeline:Graph.UniqueKey * proxy:Graph.UniqueKey option * inference:Graph.UniqueKey option * taxon:Graph.UniqueKey option * measure:Graph.UniqueKey option
        | SubmitProxiedTaxon of timeline:Graph.UniqueKey

    and LookupTaxonMessage =
        | ChangeFormFields of TaxonomicLookupModel
        | RunLookup
        | SaveTaxonResult

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
                    match Storage.addNodes g nodes with
                    | Ok g -> { model with Graph = Some (fst g) }, Cmd.none
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
                        match Storage.addNodes g nodes with
                        | Ok g -> { model with Graph = Some (fst g) }, Cmd.none
                        | Error e -> { model with Error = Some e }, Cmd.none
                with e -> { model with Error = Some e.Message }, Cmd.none
            | None -> { model with Error = Some "There was no graph" }, Cmd.none
        | SelectSource k ->
            match model.Graph with
            | None -> { model with Error = Some <| "Can't select a source when no graph is loaded." }, Cmd.none
            | Some g ->
                match g |> Storage.atomByKey k with
                | Some atom -> 
                    { model with SelectedSource = Some { AddBioticHyperedge = Map.empty; SelectedSource = atom; LinkToSecondarySource = None; Screening = NotEnteredYet } }, Cmd.none
                | None -> { model with Error = Some <| sprintf "Could not find source with key %s [%A]" k.AsString k }, Cmd.none
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
                                    | Sources.SourceNode.Included s
                                    | Sources.SourceNode.Unscreened s ->
                                        match (node :?> EligbilityCriteria) with
                                        | Include -> Sources.SourceNode.Included s |> Ok
                                        | Exclude (because, notes) -> Sources.SourceNode.Excluded (s, because, notes) |> Ok
                                | _ -> Error "No source was selected"
                            Create.createFromViewModel nodeType source.Screening
                            |> Result.bind updateNode
                            |> Result.lift GraphStructure.Node.SourceNode
                            |> Result.bind (fun n -> Storage.updateNode g (source.SelectedSource |> fst |> fst, n))
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
                                |> Result.bind (fun n -> Storage.addNodes g [ n ])
                                |> Result.bind(fun (graph, addedNodes) ->
                                    // Add relations sink nodes were specified in EnterNodeRelationData
                                    let proposedRels =
                                        model.NodeCreationRelations 
                                        |> Map.tryFind nodeType.Name 
                                        |> Option.map(fun (_,rels) ->
                                            rels |> Map.toList |> List.collect(fun (r,sinks) ->
                                                sinks |> List.map(fun s -> ThisIsSource(s,r))))
                                        |> (fun o -> match o with | Some o -> o | None -> [])
                                        |> List.append requiredRelations
                                    // Add relations that are required as specified when creating each form.
                                    Seq.fold(fun fbg (relation:RelationViewModel) -> 
                                        fbg |> Result.bind(fun fbg ->
                                            match relation with
                                            | ThisIsSink (sourceKey, proposed) -> Storage.addRelationByKey fbg sourceKey ((addedNodes.Head |> fst |> fst)) proposed
                                            | ThisIsSource (sinkKey, proposed) -> Storage.addRelationByKey fbg ((addedNodes.Head |> fst |> fst)) sinkKey proposed
                                        )) (Ok graph) proposedRels)
                                |> Result.lift (fun g -> { model with Graph = Some g })
                                |> Result.lower (fun r -> r, Cmd.none) (fun e -> { model with Error = Some e }, Cmd.none)
                            | None -> { model with Error = Some "Cannot make node as graph is not loaded." }, Cmd.none
                    | None -> { model with Error = Some (sprintf "Could not find type of %s" nodeType.Name) }, Cmd.none
            | EnterNodeRelationData(nodeType, toggleset, proposed, sinkKeys) -> 
                let x =
                    match model.NodeCreationRelations |> Map.tryFind nodeType with
                    | Some (toggleset, existing) -> model.NodeCreationRelations |> Map.add nodeType (toggleset, existing |> Map.add proposed sinkKeys)
                    | None -> model.NodeCreationRelations |> Map.add nodeType (toggleset, Map.ofList [proposed, sinkKeys])
                { model with NodeCreationRelations = x }, Cmd.none
            | ChangeNodeRelationToggle(nodeType, toggle) ->
                { model with NodeCreationRelations = model.NodeCreationRelations |> Map.add nodeType (toggle, Map.empty) }, Cmd.none
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
                | Error e -> { model with Error = Some e }, Cmd.none
            | SaveTaxonResult ->
                match model.TaxonLookup.Result with
                | None -> { model with Error = Some "Cannot save taxon, as none was found" }, Cmd.none
                | Some (taxon, relations) ->
                    match model.Graph with
                    | None -> { model with Error = Some "Cannot save taxon, as graph is not loaded" }, Cmd.none
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

                                let! updatedGraph, updatedNodes = Storage.addNodes g nodes

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
                        | Error e -> { model with Error = Some e }, Cmd.none
        | ChangeProxiedTaxonVm(timeline, proxy, inference, taxon, measure) -> 
            match model.SelectedSource with
            | None -> { model with Error = Some "Source was not selected." }, Cmd.none
            | Some source ->
                { model with SelectedSource = Some { source with AddBioticHyperedge = source.AddBioticHyperedge |> Map.add timeline (proxy, inference, taxon, measure) } }, Cmd.none
        | SubmitProxiedTaxon timelineId -> 
            match model.Graph with
            | Some g ->
                match model.SelectedSource with
                | Some source -> 
                    match source.AddBioticHyperedge |> Map.tryFind timelineId with
                    | Some (proxyId, inferId, taxonId, measureId) ->

                        let saveEdge () = result {
                            // Unique keys to nodes
                            let! proxy = 
                                Storage.atomByKey proxyId.Value g 
                                |> Result.ofOption "Could not find proxy node"
                                |> Result.bind(fun ((i,n),adj) ->
                                    match n with
                                    | GraphStructure.Node.PopulationNode p ->
                                        match p with
                                        | GraphStructure.BioticProxyNode x -> Ok x
                                        | _ -> Error "Not a biotic proxy node"
                                    | _ -> Error "Not a biotic proxy node" )
                            let! infer = 
                                Storage.atomByKey inferId.Value g 
                                |> Result.ofOption "Could not find inference node"
                                |> Result.bind(fun ((i,n),adj) ->
                                    match n with
                                    | GraphStructure.Node.PopulationNode p ->
                                        match p with
                                        | GraphStructure.InferenceMethodNode x -> Ok x
                                        | _ -> Error "Not an inference method node"
                                    | _ -> Error "Not an inference method node" )
                            let! taxon = 
                                Storage.atomByKey taxonId.Value g 
                                |> Result.ofOption "Could not find taxon node"
                                |> Result.bind(fun ((i,n),adj) ->
                                    match n with
                                    | GraphStructure.Node.PopulationNode p ->
                                        match p with
                                        | GraphStructure.TaxonomyNode x -> Ok x
                                        | _ -> Error "Not a taxonomy node"
                                    | _ -> Error "Not a taxonomy node" )
                            
                            let! updatedGraph, hyperEdgeId = Storage.addProxiedTaxon proxy taxon infer g
                            
                            // Save (1) outcome measure from hyperedge to outcome node, and (2) relation from timeline to hyperedge.
                            let! timeline = Storage.atomByKey timelineId updatedGraph |> Result.ofOption "Could not find timeline"
                            let! hyperedge = Storage.atomByKey hyperEdgeId updatedGraph |> Result.ofOption "Could not find new hyperedge"
                            let! outcomeNode = 
                                measureId
                                |> Result.ofOption "No outcome measure ID specified"
                                |> Result.bind(fun key -> Storage.atomByKey key updatedGraph |> Result.ofOption "Could not find new hyperedge")
                            let! updatedGraphWithRelations = 
                                Storage.addRelation hyperedge outcomeNode (GraphStructure.ProposedRelation.Population Population.PopulationRelation.MeasuredBy) updatedGraph
                                |> Result.bind (Storage.addRelation timeline hyperedge (GraphStructure.ProposedRelation.Exposure Exposure.ExposureRelation.HasProxyInfo))
                            return updatedGraphWithRelations
                        }
                        match saveEdge () with
                        | Ok saved -> { model with Graph = Some saved; Error = Some "Ran OK! ??" }, Cmd.none
                        | Error e -> { model with Error = Some e }, Cmd.none
                    | None -> { model with Error = Some "No hyper edge details found" }, Cmd.none
                | None -> { model with Error = Some "No source loaded" }, Cmd.none
            | None -> { model with Error = Some "No graph loaded" }, Cmd.none


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
                                p [] [ 
                                    text "Use the forms on this page to add new options for taxonomic nodes."
                                    text "Plant taxa are created by validating names against a taxonomic backbone." ]

                                cond model.Error <| function
                                | Some e -> div [ _class "alert alert-danger" ] [ 
                                        text e
                                        button [ _class "btn"; on.click (fun _ -> DismissError |> dispatch) ] [ text "Dismiss" ] ]
                                | None -> empty

                                // Allow linking to taxonomic backbone here.
                                div [ _class "card text-bg-secondary" ] [
                                    div [ _class "card-header" ] [ text "Add a plant taxonomic name" ]
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

                                h3 [] [ text "Context" ]
                                hr []
                                p [] [ text "You can relate `timelines` to a spatial context, represented by a Context node." ]
                                ViewGen.makeNodeForm<Population.Context.ContextNode> (model.NodeCreationViewModels |> Map.tryFind "ContextNode") [] (FormMessage >> dispatch)
                                h3 [] [ text "Taxonomy Node" ]
                                hr []
                                p [] [ text "In our systematic map, biotic proxies are related to real taxa by an inference method. Here, the 'Taxonomy Node' represents a *real* botanical or other taxon. We have pre-populated plant names using a taxonomic backbone." ]
                                ViewGen.makeNodeForm<Population.Taxonomy.TaxonNode> (model.NodeCreationViewModels |> Map.tryFind "TaxonNode") [] (FormMessage >> dispatch)
                                p [] [ text "In addition to the taxon node, there are nodes representing the common or vernacular names of species, genera, or families. Adding these is purely for the purposes of public interpretation of the graph database." ]
                                ViewGen.makeNodeForm<Population.Taxonomy.VernacularTaxonLabelNode> (model.NodeCreationViewModels |> Map.tryFind "VernacularTaxonLabelNode") [] (FormMessage >> dispatch)
                                h3 [] [ text "Biotic proxies" ]
                                hr []
                                p [] [ text "Biotic proxies are used to represent morphotypes, fossil remains etc. that are used to proxy species presence, but require further interpretation to connect to a *real* taxon. For example, pollen morphotypes require inference to connect to botanical taxa." ]
                                ViewGen.makeNodeForm<Population.BioticProxies.BioticProxyNode> (model.NodeCreationViewModels |> Map.tryFind "BioticProxyNode") [] (FormMessage >> dispatch)
                                h3 [] [ text "Inference method" ]
                                hr []
                                p [] [ text "Inference methods are used to translate from morphotypes or biotic proxies to real taxa." ]
                                ViewGen.makeNodeForm<Population.BioticProxies.InferenceMethodNode> (model.NodeCreationViewModels |> Map.tryFind "InferenceMethodNode") [] (FormMessage >> dispatch)
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
                                            select [ _class "form-select"; bind.change.string ((s.SelectedSource |> fst |> fst).AsString) (fun k -> SelectSource (Graph.stringToKey k) |> dispatch) ] [(
                                                cond (g.Nodes<Sources.SourceNode>()) <| function
                                                | Some sources ->
                                                    sources
                                                    |> Seq.map(fun k ->
                                                        option [ attr.value k.Key.AsString ] [ text k.Value ])
                                                    |> Seq.toList
                                                    |> concat
                                                | None -> empty
                                            )]
                                        | None ->
                                            select [ _class "form-select"; bind.change.string "" (fun k -> SelectSource (Graph.stringToKey k) |> dispatch) ] [(
                                                cond (g.Nodes<Sources.SourceNode>()) <| function
                                                | Some sources ->
                                                    sources
                                                    |> Seq.map(fun k ->
                                                        option [ attr.value k.Key.AsString ] [ text k.Value ])
                                                    |> Seq.toList
                                                    |> concat
                                                | None -> empty
                                            )]
                                    ]
                                ]

                                cond model.SelectedSource <| function
                                | None -> text "Select a source to continue"
                                | Some source -> concat [

                                    // Allow coding if included, or show exclusion reasons / options.
                                    cond (source.SelectedSource |> fst |> snd) <| function
                                    | GraphStructure.Node.SourceNode sn ->
                                        cond sn <| function
                                        | Sources.SourceNode.Unscreened s -> 
                                            concat [
                                                // Full-text screening options.
                                                div [ _class "card" ] [
                                                    div [ _class "card-header" ] [ text "Q: Is the source relevant?" ]
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
                                        | Sources.SourceNode.Included s -> concat [
                                            p [] [ text "This source has been included at full-text level. Please code information as stated below." ]
                                            div [ _class "card" ] [
                                                div [ _class "card-header" ] [ text "Q: Is it a primary or secondary source?" ]
                                                div [ _class "card-body" ] [
                                                    p [] [ 
                                                        text "A source may be 'secondary' if it does not contain any new information, but references information in other publications."
                                                        text "You can link this source to the primary sources by selecting an existing source, or adding a new one. Please check that the source does not already exist before creating a new one." ]
                                                    select [ _class "form-select"; bind.change.string (if model.SelectedSource.IsSome then (model.SelectedSource.Value.SelectedSource |> fst |> fst).AsString else "") (fun k -> SelectSource (Graph.stringToKey k) |> dispatch) ] [(
                                                        cond (g.Nodes<Sources.SourceNode>()) <| function
                                                        | Some sources ->
                                                            sources
                                                            |> Seq.map(fun k ->
                                                                option [ attr.value k.Key.AsString ] [ text k.Value ])
                                                            |> Seq.toList
                                                            |> concat
                                                        | None -> empty
                                                    )]
                                                    button [ _class "btn btn-primary" ] [ text "Link to this primary source." ]
                                                    p [] [ text "You may alternatively specify a source we do not already have listed using the below fields. This will be linked to the selected source." ]
                                                    div [ _class "card" ] [
                                                        div [ _class "card-header" ] [ text "Add a new source" ]
                                                        text "You may need to reference another source from this source that isn't already in our included sources."
                                                        ViewGen.makeNodeForm<Sources.SourceNode> (model.NodeCreationViewModels |> Map.tryFind "SourceNode") [
                                                            ThisIsSink ((source.SelectedSource |> fst |> fst), GraphStructure.ProposedRelation.Source(Sources.SourceRelation.UsesPrimarySource))
                                                        ] (FormMessage >> dispatch)
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
                                                        div [ _class "card" ] [
                                                            div [ _class "card-header" ] [ text "Add an additional timeline" ]
                                                            div [ _class "card-body" ] [
                                                                ViewGen.RelationsForms.relationsToggle<Exposure.StudyTimeline.IndividualTimelineNode> [
                                                                    ("Year", [
                                                                        ViewGen.RelationsForms.selectExistingNode<Exposure.TemporalIndex.CalYearNode> "Temporal extent: youngest date" (Exposure.ExposureRelation.ExtentEarliest |> GraphStructure.ProposedRelation.Exposure) // TODO Uncertainty
                                                                        ViewGen.RelationsForms.selectExistingNode<Exposure.TemporalIndex.CalYearNode> "Temporal extent: oldest date" (Exposure.ExposureRelation.ExtentLatest |> GraphStructure.ProposedRelation.Exposure) ])
                                                                    ("Qualitative", [
                                                                        ViewGen.RelationsForms.selectExistingNodeMulti<Exposure.TemporalIndex.QualitativeLabelNode> "Intersects this time period" (Exposure.ExposureRelation.IntersectsTime |> GraphStructure.ProposedRelation.Exposure)
                                                                    ])
                                                                ] model.NodeCreationRelations g (FormMessage >> dispatch)
                                                                ViewGen.makeNodeFormWithRelations<Exposure.StudyTimeline.IndividualTimelineNode> (fun savedRelations ->
                                                                    (ViewGen.RelationsForms.Validation.hasOne (GraphStructure.ProposedRelation.Exposure Exposure.ExposureRelation.ExtentEarliest) savedRelations 
                                                                    && ViewGen.RelationsForms.Validation.hasOne (GraphStructure.ProposedRelation.Exposure Exposure.ExposureRelation.ExtentLatest) savedRelations)
                                                                    || ViewGen.RelationsForms.Validation.hasOne (GraphStructure.ProposedRelation.Exposure Exposure.ExposureRelation.IntersectsTime) savedRelations
                                                                ) (model.NodeCreationViewModels |> Map.tryFind "IndividualTimelineNode") [
                                                                    ThisIsSink ((source.SelectedSource |> fst |> fst), GraphStructure.ProposedRelation.Source(Sources.SourceRelation.HasTemporalExtent))
                                                                ] (FormMessage >> dispatch)
                                                            ]
                                                        ]
                                                        hr []
                                                        forEach (source.SelectedSource |> GraphStructure.Relations.nodeIdsByRelation<Sources.SourceRelation> Sources.SourceRelation.HasTemporalExtent |> Storage.atomsByKey g ) <| fun timeline ->
                                                            // TODO only allow one source node to be linked. Hide form when done and only display data.
                                                            concat [
                                                                h4 [] [ textf "Timeline (ID = %A)" timeline]
                                                                hr []
                                                                p [] [ 
                                                                    text "Specify a single spatial context for the timeline."
                                                                    text "Individual study timelines have their own spatial contexts. Therefore, time-series from different spatial locations should be classed as seperate time-series (e.g. multiple sediment cores)."
                                                                ]
                                                                div [ _class "card text-bg-secondary mb-3"] [
                                                                    div [ _class "card-header" ] [ text "Required: Describe the study's spatial context" ]
                                                                    div [ _class "card-body" ] [
                                                                        ViewGen.makeNodeForm<Population.Context.ContextNode> (model.NodeCreationViewModels |> Map.tryFind "ContextNode") [
                                                                            ThisIsSink ((source.SelectedSource |> fst |> fst), Exposure.ExposureRelation.IsLocatedAt |> GraphStructure.ProposedRelation.Exposure)
                                                                        ] (FormMessage >> dispatch)
                                                                        p [] [ 
                                                                            text "Each study timeline is defined by a dating method. Explain these here."
                                                                        ]
                                                                    ]
                                                                ]

                                                                label [] [ text "Associate dating techniques with this time-series" ]
                                                                p [] [ 
                                                                    text "Describe the individul dates used to form this individual time-series. For example, these may be radiocarbon dates for a sedimentary sequence. "
                                                                    text "For living specimen data (e.g. tree-ring data), use the 'Collection Date' option to specify the time of sampling (that forms the origin point of the time-series - e.g. growth year zero)." ]
                                                                ViewGen.makeNodeForm<Exposure.StudyTimeline.IndividualDateNode> (model.NodeCreationViewModels |> Map.tryFind "IndividualDateNode") [
                                                                    ThisIsSink ((timeline |> fst |> fst), Exposure.ExposureRelation.ConstructedWithDate |> GraphStructure.ProposedRelation.Exposure)
                                                                ] (FormMessage >> dispatch)
                                                                p [] [ text "There are the following individual dates saved for this time-series:" ]
                                                                forEach (timeline |> GraphStructure.Relations.nodeIdsByRelation<Exposure.ExposureRelation> "ConstructedWithDate" |> (fun n -> Storage.atomsByKey g n)) <| fun dateAtom ->
                                                                    p [] [ text ((dateAtom |> fst |> snd).DisplayName()) ]
                                                            ] // end timeline details card.
                                                    ]
                                                ]
                                            ] // end timelines card.                                                

                                            div [ _class "card" ] [
                                                div [ _class "card-header" ] [ text "Q: Which biodiversity outcomes are associated with the timelines in this source?" ]
                                                div [ _class "card-body" ] [
                                                    p [] [
                                                        text "Each temporal-spatial context (as specified above) may have one or more biodiversity outcomes associated with it."
                                                        text "A biodiversity outcome here means a combination of a biotic proxy (proxy -> inference method -> taxon) and an outcome measurement."
                                                        text "For example, an outcome may be 'abundance', and the biotic proxy may be [ acacia-type (pollen morphotype) -> African pollen atlas -> Acacia (genus) ]"
                                                    ]
                                                    p [] [
                                                        text "If a taxon is not present that you need, you can use the taxonomic lookup (requires internet connection) to create new taxon nodes."
                                                        text "Similarly, if you need a new biotic proxy node or inference node, make these manually in the 'Population' tab to the left."
                                                    ]

                                                    // 3. For each study timeline, add proxied taxa.
                                                    forEach (source.SelectedSource |> GraphStructure.Relations.nodeIdsByRelation<Sources.SourceRelation> Sources.SourceRelation.HasTemporalExtent |> Storage.atomsByKey g ) <| fun timelineNode ->
                                                        // Display existing and add new proxied taxa
                                                        // Proxied taxa are related to an outcome too.
                                                        cond (timelineNode |> fst |> snd) <| function
                                                        | GraphStructure.Node.ExposureNode e ->
                                                            cond e <| function
                                                            | Exposure.TimelineNode (timeline: Exposure.StudyTimeline.IndividualTimelineNode) ->
                                                                concat [
                                                                    div [] [
                                                                        p [] [ textf "Timeline" ]
                                                                        table [ _class "table" ] [
                                                                            thead [] [
                                                                                tr [] [
                                                                                    th [ attr.scope "column" ] [ text "Biotic Proxy" ]
                                                                                    th [ attr.scope "column" ] [ text "Inference Method" ]
                                                                                    th [ attr.scope "column" ] [ text "Botanical Taxon / Taxa" ]
                                                                                    th [ attr.scope "column" ] [ text "Measured by" ]
                                                                                    th [ attr.scope "column" ] [ text "Actions" ]
                                                                                ]
                                                                            ]
                                                                            tbody [] [
                                                                                // Add a new proxied taxon and outcome measure.
                                                                                tr [] [        
                                                                                    cond (source.AddBioticHyperedge |> Map.tryFind (timelineNode |> fst |> fst)) <| function
                                                                                    | Some (proxy,infer,taxon,outcome) -> concat [
                                                                                        td [] [ select [ _class "form-select"; bind.change.string (if proxy.IsSome then proxy.Value.AsString else "") (fun s -> ChangeProxiedTaxonVm((timelineNode |> fst |> fst),Some (Graph.stringToKey s),infer,taxon,outcome) |> dispatch) ] [ ViewGen.optionGen<Population.BioticProxies.BioticProxyNode> model.Graph ] ]
                                                                                        td [] [ select [ _class "form-select"; bind.change.string (if infer.IsSome then infer.Value.AsString else "") (fun s -> ChangeProxiedTaxonVm((timelineNode |> fst |> fst),proxy,Some (Graph.stringToKey s),taxon,outcome) |> dispatch) ] [ ViewGen.optionGen<Population.BioticProxies.InferenceMethodNode> model.Graph ] ]
                                                                                        td [] [ select [ _class "form-select"; bind.change.string (if taxon.IsSome then taxon.Value.AsString else "") (fun s -> ChangeProxiedTaxonVm((timelineNode |> fst |> fst),proxy,infer,Some (Graph.stringToKey s),outcome) |> dispatch) ] [ ViewGen.optionGen<Population.Taxonomy.TaxonNode> model.Graph ] ]
                                                                                        td [] [ select [ _class "form-select"; bind.change.string (if outcome.IsSome then outcome.Value.AsString else "") (fun s -> ChangeProxiedTaxonVm((timelineNode |> fst |> fst),proxy,infer,taxon,Some (Graph.stringToKey s)) |> dispatch) ] [ ViewGen.optionGen<Outcomes.Biodiversity.BiodiversityDimensionNode> model.Graph ] ] ]
                                                                                    | None -> concat [
                                                                                        td [] [ select [ _class "form-select"; bind.change.string "" (fun s -> ChangeProxiedTaxonVm((timelineNode |> fst |> fst),Some (Graph.stringToKey s),None,None,None) |> dispatch) ] [ ViewGen.optionGen<Population.BioticProxies.BioticProxyNode> model.Graph ] ]
                                                                                        td [] [ select [ _class "form-select"; bind.change.string "" (fun s -> ChangeProxiedTaxonVm((timelineNode |> fst |> fst),None,Some (Graph.stringToKey s),None,None) |> dispatch) ] [ ViewGen.optionGen<Population.BioticProxies.InferenceMethodNode> model.Graph ] ]
                                                                                        td [] [ select [ _class "form-select"; bind.change.string "" (fun s -> ChangeProxiedTaxonVm((timelineNode |> fst |> fst),None,None,Some (Graph.stringToKey s),None) |> dispatch) ] [ ViewGen.optionGen<Population.Taxonomy.TaxonNode> model.Graph ] ]
                                                                                        td [] [ select [ _class "form-select"; bind.change.string "" (fun s -> ChangeProxiedTaxonVm((timelineNode |> fst |> fst),None,None,None,Some (Graph.stringToKey s)) |> dispatch) ] [ ViewGen.optionGen<Outcomes.Biodiversity.BiodiversityDimensionNode> model.Graph ] ] ]
                                                                                    td [] [
                                                                                        button [ _class "btn btn-primary"; on.click (fun _ -> SubmitProxiedTaxon (timelineNode |> fst |> fst) |> dispatch) ] [ text "Save" ]
                                                                                    ]
                                                                                ]
                                                                                forEach (source.SelectedSource |> GraphStructure.Relations.nodeIdsByRelation<Exposure.ExposureRelation> Exposure.ExposureRelation.HasProxyInfo |> Storage.atomsByKey g ) <| fun proxiedTaxa -> concat [
                                                                                    cond (proxiedTaxa |> fst |> snd) <| function
                                                                                    | GraphStructure.Node.PopulationNode e ->
                                                                                        cond e <| function
                                                                                        | GraphStructure.ProxiedTaxonNode ->
                                                                                            tr [] [
                                                                                                td [] [ text "A proxied taxon entry..." ]
                                                                                                td [] [ text "..." ]
                                                                                                td [] [ text "..." ]
                                                                                                td [] [ text "..." ]
                                                                                                td [] []
                                                                                            ]
                                                                                        | _ -> empty
                                                                                ]
                                                                            ]
                                                                        ]
                                                                    ]
                                                                ]
                                                            | _ -> empty
                                                        | _ -> empty

                                                ]
                                            ] // end outcomes card

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