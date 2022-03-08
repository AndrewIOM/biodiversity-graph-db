namespace BiodiversityCoder.Core

module FieldDataTypes =

    let shortText = string

    let longText = string

    let date = System.DateTime

    // TODO implement types that can be rendered as fields,
    // and also hold values without having to validate in all
    // the node and relation types individually.


/// Simple representation of a graph data structure
/// (adapted from https://orbifold.net/default/fsharp-graphs/).
[<RequireQualifiedAccess>]
module Graph =
    
    type Node<'TData> = int * 'TData
    
    type Connection<'TData> =
        int * // source Id
        int * // sink id
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

        // 1. Find the current maximum (sequential) node ID
        let max =
            { contents = if (graph.Length = 0) then -1
                         else graph |> List.maxBy(fun atom -> getAtomId atom) |> getAtomId }

        // 2. Insert a new node with the specified ID
        Seq.fold(fun acc data ->
                    max.Value <- max.Value + 1
                    let newNode = max.Value, data
                    addNode newNode acc
                     ) graph items, max.Value


    // Remove nodes:

    /// Is true if the atom has a sink
    /// towards the specified ID.
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
    /// TODO Make more efficient.
    let addRelation sourceId sinkId weight connData (graph:Graph<_,_>) : Result<Graph<'nodeData,'connData>,string> =
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
        | TaxonomyNode of Taxonomy.Taxon

    type OutcomeNode =
        | MeasureNode of BiodiversityMeasures.Measure

    /// Routing type to represent all possible nodes within the
    /// evidence graph.
    type Node =
        | SourceNode of Source
        | PopulationNode of PopulationNode
        | ExposureNode of ExposureNode
        | OutcomeNode of OutcomeNode

    /// Routing type to represent all possible relations within
    /// the evidence graph.
    type NodeRelation =
        | FromExposure of Exposure.ExposureNodeRelation
        | FromPopulation of Taxonomy.TaxonRelation

    /// Routing type to represent all possible relations within
    /// the evidence graph. Can only be created using `makeRelation`,
    /// which constrains relations to valid node types only.
    type Relation =
        private
        | Exposure of ExposureRelation

    type ValidRelation<'TData> = private ValidRelation of int * int * 'TData
    let private unwrap (ValidRelation (a,b,c)) = a,b,c

    // Use the name of this relation e.g. 'EarliestTime'
    // to lookup case on 'ExposureNodeRelation', extract
    // the first parameter and get its type. Compare this
    // type to the node's data type. If the same, they are
    // true. TODO
    let compareTypes node rel paramIndex = 
        let nodeType = node.GetType().FullName
        true

    let compare source sink rel ifTrue =
        if compareTypes source rel 0 && compareTypes sink rel 1
        then Ok <| ValidRelation(fst source, fst sink, ifTrue)
        else Error "Node didn't match"

    /// Makes a master relation from defined relation DU types.
    /// This effectively constrains relations to a specific
    /// node type combination.
    let makeRelation source sink (rel:ExposureRelation) =
        match rel with
        | EarliestTime testData -> compare source sink rel (Exposure(EarliestTime testData))
        | _ -> failwith "not done"

    /// Add a node relation to the graph, validating that the relation
    /// can only occur on valid node sources / sinks in the process.
    let addRelation node1 node2 rel weight (graph:Graph.Graph<'a,Relation>) =
        let validated = makeRelation node1 node2 rel
        validated |> Result.map(fun r ->
            let sourceId,sinkId,data = unwrap r
            Graph.addRelation sourceId sinkId weight data graph)


    let tryFindExposure cond (graph:Graph.Graph<Node,_>) =
        graph |> List.choose(fun n -> 
            match fst n with 
            | i, ExposureNode e -> Some (i, e) 
            | _ -> None)
        |> Seq.tryFind (fun (_,e) -> cond e)

    let tryFindYear year g = 
        g |> tryFindExposure (fun n -> 
            match n with
            | YearNode y -> y.Year = year
            | _ -> false
        )

    let tryFindTimeLabel label g = 
        g |> tryFindExposure (fun n -> 
            match n with
            | SliceLabelNode y -> y.Name = label
            | _ -> false
        )


module List =

    let retn = Ok
    let rec mapResult f list =
        let cons head tail = head :: tail
        match list with
        | [] -> 
            retn []
        | head::tail ->
            retn cons <*> (f head) <*> (mapResult f tail)

module Seed =

    open GraphStructure
    open Exposure
    open Exposure.TemporalIndex

    let initGraph () =
        let g : Graph.Graph<Node,Relation> = Graph.empty

        // Population: life node
        let taxonRoot = Population.Taxonomy.Life

        // Exposure: Holocene time index
        let timeIndexNodes = 
            [ 0 .. 14000 ] 
            |> List.map createTimeNode 
            |> List.mapResult id
            |> Result.map(List.map(fun n -> ExposureNode(YearNode n)))
        let holoceneLabel = SliceLabelNode { Name = "Holocene" }
        
        // Outcomes: basic measures
        let outcomes = [
            OutcomeNode(MeasureNode Outcomes.BiodiversityMeasures.Abundance)
            OutcomeNode(MeasureNode Outcomes.BiodiversityMeasures.PresenceAbsence)
        ]

        let result =
            g
            |> Graph.addNodeData [
                PopulationNode (TaxonomyNode taxonRoot)
                ExposureNode holoceneLabel
            ] |> fst
            |> Graph.addNodeData outcomes |> fst

        // Make 2x relations to define time period.
        let source = 
            let a = (g |> tryFindTimeLabel "Holocene").Value
            match snd a with
            | SliceLabelNode b -> fst a, b
            | _ -> failwith "fail"
        let sink1 = 
            let a = (g |> tryFindYear 11650<calYearBP>).Value
            match snd a with
            | YearNode b -> fst a, b
            | _ -> failwith "fail"
        let sink2 = 
            let a = (g |> tryFindYear 0<calYearBP>).Value
            match snd a with
            | YearNode b -> fst a, b
            | _ -> failwith "fail"
        ()
        // let r1 = 
        //     FromExposure(EarliestTime (snd source, snd sink1))
        //     |> makeRelation source sink1
        // let r2 = FromExposure(LatestTime (snd source, snd sink2))

        // let added1 =
        //     timeIndexNodes
        //     |> lift (fun i -> Graph.addNodeData i result)
        
        // let added2 =
        //     added1
        //     |> addRelation

        // ()

        // added1
        // |> lift (Graph.addRelation (fst holoceneStart) (fst holoceneEnd) 1 r1)