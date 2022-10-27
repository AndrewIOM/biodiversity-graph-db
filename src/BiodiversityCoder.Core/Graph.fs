namespace BiodiversityCoder.Core

/// Simple representation of a graph data structure
/// (adapted from https://orbifold.net/default/fsharp-graphs/).
[<RequireQualifiedAccess>]
module Graph =
    
    type Node<'TData> = System.Guid * 'TData
    
    type Connection<'TData> =
        System.Guid * // source Id
        System.Guid * // sink id
        int * // weight
        'TData // connection data
    
    /// Lists all connections
    type Adjacency<'TStructon> =
        Connection<'TStructon> list
        
    /// A single node with its outgoing connections
    type Atom<'TNodeDaton, 'TConnectionDaton> =
        Node<'TNodeDaton> * Adjacency<'TConnectionDaton>

    type Graph<'TNodeDaton, 'TConnectionDaton> =
        Atom<'TNodeDaton, 'TConnectionDaton> list

    let empty : Graph<_,_> = []

    let getAtomId atom =
        atom |> fst |> fst

    let getAtom id graph =
        graph
        |> List.tryFind(fun atom -> atom |> getAtomId = id)

    let getNode id graph =
        match getAtom id graph with
        | Some a -> a |> fst |> Some
        | None -> None

    let addNode node graph : Graph<'nodeData,_> =
        let id = fst node
        match getNode id graph with
        | None ->
            let newAdjacency = []
            let newAtom = node, newAdjacency
            graph @ [ newAtom ] //|> Ok
        | _ -> failwith "A node already exists with that ID"

    let addNodeData (items:'nodeData seq) (graph:Graph<'nodeData,_>) =
        Seq.fold(fun (acc,acc2) data ->
                    let newNode = System.Guid.NewGuid(), data
                    addNode newNode acc, newNode :: acc2
                     ) (graph, []) items

    let pointsTo id atom =
        atom 
        |> snd
        |> List.exists(fun (_,y,_,_) -> y = id)

    let getConnectionSinkId ((_,sinkId,_,_):Connection<_>) =
        sinkId

    let private filterHasNotSinkId id =
        fun conn -> getConnectionSinkId conn <> id

    /// Remove a node and its connections from the graph.
    /// Identifies and removes all connections associated with the node.
    let removeNode id graph =
        List.fold(fun acc (n,a) ->
            if (fst n) = id 
            then acc
            else
                if pointsTo id (n,a) 
                then
                    let newAd = a |> List.filter (filterHasNotSinkId id)
                    let newAtom = n, newAd
                    acc @ [ newAtom ]
                else acc @ [ n, a ]
            ) [] graph

    /// Connect two nodes together given a relationship.
    let addRelation (sourceId:System.Guid) sinkId weight connData (graph:Graph<_,_>) : Result<Graph<'nodeData,'connData>,string> =
        let source = graph |> List.tryFind(fun n -> fst (fst n) = sourceId)
        let sink = graph |> List.tryFind(fun n -> fst (fst n) = sourceId)
        if source.IsNone || sink.IsNone
        then Error "The connection was not between valid nodes"
        else
            graph
            |> List.map(fun i ->
                if i = source.Value
                then
                    let conn : Connection<'connData> = (sourceId, sinkId, weight, connData)
                    let adjacency = conn :: snd source.Value
                    (fst source.Value, adjacency)
                else i)
            |> Ok

    let tryFind cond (graph:Graph<_,_>) =
        graph |> Seq.tryFind(fun g -> cond g)

/// The core structure of nodes and relations that governs
/// the ecology mapper database.
module GraphStructure =

    open Sources
    open Population
    open Exposure
    open Outcomes

    type PopulationNode =
        | TaxonomyNode of Taxonomy.TaxonNode
        | BioticProxyNode of BioticProxies.BioticProxyNode
        | InferenceMethodNode of BioticProxies.InferenceMethodNode
        | ProxiedTaxonNode // Intermediate node that represents a hyperedge

    type OutcomeNode =
        | MeasureNode of Biodiversity.BiodiversityDimensionNode

    /// Routing type to represent all possible nodes within the
    /// evidence graph.
    type Node =
        | SourceNode of SourceNode
        | PopulationNode of PopulationNode
        | ExposureNode of ExposureNode
        | OutcomeNode of OutcomeNode

    /// Routing type to represent all possible relations within
    /// the evidence graph.
    type NodeRelation =
        | FromSource of Sources.SourceNodeRelation
        | FromPopulation of Population.PopulationNodeRelation
        | FromExposure of Exposure.ExposureNodeRelation

    /// Routing type to represent all possible relations within
    /// the evidence graph. Can only be created using `makeRelation`,
    /// which constrains relations to valid node types only.
    type Relation =
        private
        | Source of SourceRelation
        | Population of PopulationRelation
        | Exposure of ExposureRelation

    type ProposedRelation =
        | Source of SourceRelation
        | Population of PopulationRelation
        | Exposure of ExposureRelation

    type Node with

        /// Provides a (non-unique) 'pretty' name for use in user interfaces to display
        /// the node, e.g. in a dropdown list or table.
        member this.DisplayName () =
            match this with
            | PopulationNode p ->
                match p with
                | BioticProxyNode n -> n.ToString() // TODO
                | TaxonomyNode n -> n.ToString() // TODO
                | InferenceMethodNode n -> n.ToString() // TODO
                | ProxiedTaxonNode -> "[Proxied taxon hyper-edge]"
            | SourceNode s ->
                match s with
                | Bibliographic n -> 
                    sprintf "%s (%i). %s" 
                        (if n.Author.IsSome then n.Author.Value.Value else "?") n.Year
                        (if n.Title.IsSome then n.Title.Value.Value else "?")
                | GreyLiterature n -> sprintf "Grey literature source: %s" n.Title.Value
                | DarkData n -> sprintf "'Dark data' from %s" n.Contact.LastName.Value
            | ExposureNode e ->
                match e with
                | YearNode y -> sprintf "%i cal yr BP" y.Year
                | SliceLabelNode n -> n.Name
            | OutcomeNode o ->
                match o with
                | MeasureNode n -> n.ToString()

        member this.Key () = 
            match this with
            | PopulationNode p ->
                match p with
                | BioticProxyNode n -> n.ToString() // TODO
                | TaxonomyNode n -> n.ToString() // TODO
                | InferenceMethodNode n -> n.ToString() // TODO
                | ProxiedTaxonNode -> "[Proxied taxon hyper-edge]"
            | SourceNode s ->
                match s with
                | Bibliographic n -> 
                    String.concat "_" [
                        "pub"
                        (if n.Author.IsSome then n.Author.Value.Value else "unknown")
                        (if n.Title.IsSome then 
                            (n.Title.Value.Value.Split(" ") |> Seq.map (Seq.head >> string) |> String.concat "")
                            else "notitle")
                        string n.Year ]
                | GreyLiterature n -> 
                    sprintf "grey_%s_%s_%s"
                        n.Contact.LastName.Value
                        (n.Contact.FirstName.Value.Split(" ") |> Seq.map (Seq.head >> string) |> String.concat "")
                        (n.Title.Value.Split(" ") |> Seq.map (Seq.head >> string) |> String.concat "")
                | DarkData n -> sprintf "darkdata_%s" n.Contact.LastName.Value
            | ExposureNode e ->
                match e with
                | YearNode y -> sprintf "%iybp" y.Year
                | SliceLabelNode n -> n.Name
            | OutcomeNode o ->
                match o with
                | MeasureNode n -> n.ToString()

    /// Functions to tryFind specific node types based on their
    /// inherent indexes or other conditions.
    module Nodes =

        let asPopnNode = function | PopulationNode n -> Some n | _ -> None
        let asExposureNode = function | ExposureNode n -> Some n | _ -> None
        let asSourceNode = function | SourceNode n -> Some n | _ -> None
        let asOutcomeNode = function | OutcomeNode n -> Some n | _ -> None

        let internal whereTaxon' cond = function
            | TaxonomyNode t -> if cond t then Some t else None
            | _ -> None

        let internal whereBioticProxy' cond = function
            | BioticProxyNode t -> if cond t then Some t else None
            | _ -> None

        let internal whereInferenceMethod' cond = function
            | InferenceMethodNode t -> if cond t then Some t else None
            | _ -> None

        let internal whereTime' label = function
            | SliceLabelNode y -> if y.Name = label then Some y else None
            | _ -> None

        let internal whereYear' year = function
            | YearNode y -> if y.Year = year then Some y else None
            | _ -> None

        let internal tryFind nodeType cond2 cond graph = 
            graph
            |> Graph.tryFind (fun (d,_) -> snd d |> nodeType |> Option.bind(fun x -> cond2 cond x) |> Option.isSome)
            |> Option.map(fun ((i,d),adj) -> ((i, (d |> nodeType |> Option.get |> cond2 cond |> Option.get)), adj))
        
        let tryFindTaxon cond graph = tryFind asPopnNode whereTaxon' cond graph
        let tryFindProxy cond graph = tryFind asPopnNode whereBioticProxy' cond graph
        let tryFindInferenceMethod cond graph = tryFind asPopnNode whereInferenceMethod' cond graph
        let tryFindTimePeriod label graph = tryFind asExposureNode whereTime' label graph
        let tryFindYear year graph = tryFind asExposureNode whereYear' year graph

        let tryMakeNode t (n:obj) =
            try
                match t with
                // Source nodes:
                | (t: System.Type) when t = typeof<Sources.SourceNode> -> n :?> Sources.SourceNode |> SourceNode |> Ok
                // Population nodes:
                | (t: System.Type) when t = typeof<BioticProxies.BioticProxyNode> -> n :?> BioticProxies.BioticProxyNode |> BioticProxyNode |> PopulationNode |> Ok
                | (t: System.Type) when t = typeof<Taxonomy.TaxonNode> -> n :?> Taxonomy.TaxonNode |> TaxonomyNode |> PopulationNode |> Ok
                | (t: System.Type) when t = typeof<BioticProxies.InferenceMethodNode> -> n :?> BioticProxies.InferenceMethodNode |> InferenceMethodNode |> PopulationNode |> Ok
                // | (t: System.Type) when t = typeof<Population.Taxonomy.VernacularTaxonLabelNode> -> n :?> Population.Taxonomy.VernacularTaxonLabelNode |> TaxonomyNode |> PopulationNode
                // | t when t = typeof<Population.Context.ContextNode> -> n :?> Population.Context.ContextNode |> 
                // Exposure node:
                | (t: System.Type) when t = typeof<TemporalIndex.CalYearNode> -> n :?> TemporalIndex.CalYearNode |> YearNode |> ExposureNode |> Ok
                | (t: System.Type) when t = typeof<TemporalIndex.QualitativeLabelNode> -> n :?> TemporalIndex.QualitativeLabelNode |> SliceLabelNode |> ExposureNode |> Ok
                // Outcome node:
                | (t: System.Type) when t = typeof<TemporalIndex.QualitativeLabelNode> -> n :?> TemporalIndex.QualitativeLabelNode |> Exposure.SliceLabelNode |> ExposureNode |> Ok
                | _ -> Error <| sprintf "Not a known node type (%s)" t.Name
            with | e -> Error <| sprintf "Failed to make a node: %s" e.Message

    module Relations =

        type ValidRelation<'TData> = private ValidRelation of System.Guid * System.Guid * 'TData
        let private unwrap (ValidRelation (a,b,c)) = a,b,c

        /// Helpers to read F# cases
        module Cases =

            open FSharp.Reflection

            let getUnionCaseName (x:'a) = 
                match FSharpValue.GetUnionFields(x, typeof<'a>) with
                | case, _ -> case.Name  

            /// Use an equivalent 'NodeRelation' discriminated union to lookup
            /// the source and sink type constraints for a given relation.
            /// Returns true if the case parameter at the specified index is
            /// of the same type as 'node.
            let compareTypes (node:System.Guid * 'node) rel paramIndex = 
                printfn "type is %A" (rel.GetType().FullName)
                let lookupType = 
                    rel.GetType().FullName.Replace("Relation", "NodeRelation")
                    |> System.Type.GetType
                printfn "type2 is %A" lookupType
                let caseName = getUnionCaseName rel
                printfn "case is %A" caseName
                let constraintCase =
                    Reflection.FSharpType.GetUnionCases(lookupType)
                    |> Seq.tryFind (fun uc -> uc.Name = caseName)
                printfn "constraint is %A" constraintCase
                match constraintCase with
                | Some (c: UnionCaseInfo) -> 
                    let field: System.Reflection.PropertyInfo = c.GetFields().[paramIndex]
                    let nodeType =
                        let baseType = (snd node).GetType().BaseType
                        if baseType = typeof<System.Object> then (snd node).GetType() else baseType
                    field.PropertyType = nodeType
                | None -> false
                
        let compare source sink rel ifTrue =
            if Cases.compareTypes source rel 0 && Cases.compareTypes sink rel 1
            then Ok <| ValidRelation(fst source, fst sink, ifTrue)
            else Error "Node didn't match"

        /// Makes a master relation from defined relation DU types.
        /// This effectively constrains relations to a specific
        /// node type combination. May also implement additional graph
        /// validation constraints (beyond type signature) here.
        let makeRelation source sink (rel:ProposedRelation) : Result<ValidRelation<Relation>,string> =
            match rel with
            | Exposure rel ->
                match rel with
                | EarliestTime -> compare source sink rel (Relation.Exposure EarliestTime)
                | Next -> compare source sink rel (Relation.Exposure Next)
                | Contains -> compare source sink rel (Relation.Exposure Contains)
                | LatestTime -> compare source sink rel (Relation.Exposure LatestTime)
                | TimeEstimate t -> compare source sink rel (Relation.Exposure <| TimeEstimate t)
                | OccursWithin t -> compare source sink rel (Relation.Exposure <| OccursWithin t)
                | UncertaintyOldest t -> compare source sink rel (Relation.Exposure <| UncertaintyOldest t)
                | UncertaintyYoungest t -> compare source sink rel (Relation.Exposure <| UncertaintyYoungest t)
                | ExtentEarliest -> compare source sink rel (Relation.Exposure ExtentEarliest)
                | ExtentEarliestUncertainty -> compare source sink rel (Relation.Exposure ExtentEarliestUncertainty)
                | ExtentLatest -> compare source sink rel (Relation.Exposure ExtentLatest)
                | ExtentLatestUncertainty -> compare source sink rel (Relation.Exposure ExtentLatestUncertainty)
                | IntersectsTime -> compare source sink rel (Relation.Exposure IntersectsTime)
                | HasProxyInfo -> compare source sink rel (Relation.Exposure HasProxyInfo)
                | HasOrphanProxy -> compare source sink rel (Relation.Exposure HasOrphanProxy)
                | IsLocatedAt -> compare source sink rel (Relation.Exposure IsLocatedAt)
            | Population rel ->
                match rel with
                | InferredFrom -> compare source sink rel (Relation.Population InferredFrom)
                | IsA -> compare source sink rel (Relation.Population IsA)
                | HasLabel -> compare source sink rel (Relation.Population HasLabel)
                | InferredUsing -> compare source sink rel (Relation.Population InferredUsing)
                | InferredAs -> compare source sink rel (Relation.Population InferredAs)
              | Source rel ->
                match rel with
                | HasTemporalExtent -> compare source sink rel (Relation.Source HasTemporalExtent)


        /// Add a node relation to the graph, validating that the relation
        /// can only occur on valid node sources / sinks in the process.
        let addRelation (atom1:Graph.Atom<'data,'conn>) (atom2:Graph.Atom<'data2,'conn2>) rel weight (graph:Graph.Graph<'a,Relation>) =
            result {
                let! validated = makeRelation (fst atom1) (fst atom2) rel
                let sourceId,sinkId,data = unwrap validated
                return! Graph.addRelation sourceId sinkId weight data graph
            }

        /// Adds a 'proxied taxon' intermediary node to the graph. This node represents
        /// an 'occurrence' of a combination of inference method, biotic proxy, and taxon
        /// that are only valid for one time and study.
        let addProxiedTaxon (edge:ProxiedTaxon.ProxiedTaxonHyperEdge) graph = result {
            // Get the three included nodes:
            let! existingProxy = graph |> Nodes.tryFindProxy (fun n -> n = edge.InferredFrom), "proxy doesn't exist"
            let! existingTaxon = graph |> Nodes.tryFindTaxon (fun n -> n = edge.InferredAs), "taxon doesn't exist"
            let! existingInfer = graph |> Nodes.tryFindInferenceMethod (fun n -> n = edge.InferredUsing), "infer doesn't exist"
            let proxiedGraph = Graph.addNodeData [PopulationNode ProxiedTaxonNode] graph
            let! proxiedTaxon = Graph.getAtom (snd proxiedGraph |> List.last |> fst) (fst proxiedGraph), "no intermediate node"
            // Add relations that make the intermediate node encode the hyper-edge:
            return!
                proxiedGraph |> fst
                |> addRelation proxiedTaxon existingProxy (Population InferredFrom) 1
                |> Result.bind (addRelation proxiedTaxon existingProxy (Population InferredFrom) 1)
                |> Result.bind (addRelation proxiedTaxon existingInfer (Population InferredUsing) 1)
                |> Result.bind (addRelation proxiedTaxon existingTaxon (Population InferredAs) 1)
        }
