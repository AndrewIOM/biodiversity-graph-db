namespace BiodiversityCoder.Core

/// Custom compact serialiser that saves each record in a list on a single line
module SingleLineCompactSerialiser =
    open Newtonsoft.Json
    open Microsoft.FSharpLu.Json

    let inline settings< ^T> =
        let settings =
            JsonSerializerSettings(
                NullValueHandling = NullValueHandling.Ignore,
                MissingMemberHandling = MissingMemberHandling.Error
            )
        settings.Converters.Add(CompactUnionJsonConverter(true, true))
        settings

    let serialiseToStream (index: 'a seq) (stream:System.IO.StreamWriter) =
        let serialiser = JsonSerializer.Create(settings)
        use writer = new JsonTextWriter(stream)
        let l = index |> Seq.length
        writer.WriteRaw("[\n")
        index |> Seq.iteri(fun n i ->
            let o = Newtonsoft.Json.Linq.JObject.FromObject(i, serialiser)
            writer.WriteRaw("\t")
            o.WriteTo(writer)
            if n = l - 1 then writer.WriteRaw("\n\n")
            else writer.WriteRaw(",\n\n")
            writer.Flush() )
        writer.WriteRaw("]")


/// IO for storing the graph database.
/// Nodes are stored as individual files.
module Storage =

    open System.IO
    open Microsoft.FSharpLu.Json

    /// A file-based graph structure consisting of individual json
    /// files and a json index contained within a local file system folder.
    type FileBasedGraph<'node,'rel> = private FileBasedGraph of CachedGraph<'node, 'rel>
    and CachedGraph<'node,'rel> = {
        Graph: Graph.Graph<'node, 'rel>
        NodesByType: Map<string, Map<Graph.UniqueKey, string>>
        Directory: string
    }

    let unwrap (FileBasedGraph f) = f

    type FileBasedGraph<'a,'b> with
        /// List nodes from the node index by key and pretty name.
        member this.Nodes<'c> () =
            (this |> unwrap).NodesByType |> Map.tryFind (typeof<'c>).Name
        member this.Directory = (unwrap this).Directory

    let indexFile = "atom-index.json"

    let inline loadCacheFile< ^T> directory filename : Result< ^T,string> =
        if Directory.Exists directory
        then 
            if File.Exists(Path.Combine(directory, filename))
            then 
                try
                    Path.Combine(directory, filename)
                    |> Compact.deserializeFile
                    |> Ok
                with | e -> Error <| sprintf "Could not deserialise a file as %s (%s). %s" (typeof< ^T>.ToString()) filename e.Message
            else Error <| sprintf "File does not exist 1: %s" filename
        else Error <| sprintf "Directory does not exist 3: %s" directory

    let makeCacheFile<'a> directory filename (entity:'a) =
        if Directory.Exists directory
        then Compact.serializeToFile (Path.Combine(directory, filename)) entity |> Ok
        else Error <| sprintf "Directory does not exist 2: %s" directory        

    type NodeIndexItem = {
        NodeId: Graph.UniqueKey
        NodeTypeName: string
        PrettyName: string
    }

    /// Output a json file with each object of a collection on a single line
    let makeCacheFileCompactList directory filename (entity:NodeIndexItem seq) =
        if Directory.Exists directory
        then 
            try
                use file = File.Open(Path.Combine(directory, filename), FileMode.Create)
                use stream = new StreamWriter(file, System.Text.Encoding.UTF8)
                SingleLineCompactSerialiser.serialiseToStream entity stream |> Ok
            with
            | ex -> Error <| sprintf "%s - %s" ex.Message ex.StackTrace
        else Error <| sprintf "Directory does not exist 2: %s" directory

    /// Types that are indexed in a single file rather than individual files.
    let typesToIndex = [ 
        typeof<Exposure.TemporalIndex.CalYearNode>.Name ]

    let loadIndex directory : Result<NodeIndexItem list,string> =
        loadCacheFile directory indexFile

    /// Allow saving specific types into a combined file rather than individual files.
    /// Useful for time nodes, which are plentiful and very small data-wise.
    let loadAtomsFromIndex directory (atomType:string) : Result<Map<Graph.UniqueKey,Graph.Atom<'a, 'b>>,string> =
        loadCacheFile directory (sprintf "atom-%s-index.json" (atomType.ToLower()))
        |> Result.lift Map.ofArray

    let loadAtom directory (atomType:string) (atomKey:Graph.UniqueKey) : Result<Graph.Atom<'a, 'b>,string> =
        if typesToIndex |> List.contains atomType
        then 
            loadAtomsFromIndex directory atomType 
            |> Result.bind(fun m -> m.TryFind atomKey |> Result.ofOption (sprintf "Could not find atom %s" atomKey.AsString))
        else loadCacheFile directory (sprintf "atom-%s.json" (atomKey.AsString.ToLower()))

    let loadAtoms directory (atomType:string) (atomKeys:Graph.UniqueKey list) : Result<list<Graph.Atom<'a, 'b>>,string> =
        if typesToIndex |> List.contains atomType
        then 
            loadAtomsFromIndex directory atomType
            |> Result.lift(fun map -> map |> Map.toList |> List.map snd)
        else 
            atomKeys
            |> List.map(fun atomKey -> loadCacheFile directory (sprintf "atom-%s.json" (atomKey.AsString.ToLower())))
            |> Result.ofList

    let saveAtomToIndex directory (atomType:string) atomKey item =
        let file = (sprintf "atom-%s-index.json" (atomType.ToLower()))
        if File.Exists <| Path.Combine(directory, file) then
            loadAtomsFromIndex directory atomType
            |> Result.lift (Map.add atomKey item)
            |> Result.bind (makeCacheFile directory file)
        else Map.empty |> Map.add atomKey item |> makeCacheFile directory file

    let saveAtom directory (atomType:string) (atomKey:Graph.UniqueKey) (item:(Graph.UniqueKey * GraphStructure.Node) * Graph.Adjacency<GraphStructure.Relation>) =
        if typesToIndex |> List.contains atomType
        then saveAtomToIndex directory atomType atomKey item
        else makeCacheFile directory (sprintf "atom-%s.json" (atomKey.AsString.ToLower())) item

    let saveAtomsToIndex directory (atomType:string) (items:seq<(Graph.UniqueKey * GraphStructure.Node) * Graph.Adjacency<GraphStructure.Relation>>) =
        let file = (sprintf "atom-%s-index.json" (atomType.ToLower()))
        let addItemsToMap m = Seq.fold (fun map (i: (Graph.UniqueKey * GraphStructure.Node) * Graph.Adjacency<GraphStructure.Relation>) -> Map.add (i |> fst |> fst) i map) m items
        if File.Exists <| Path.Combine(directory, file) then
            loadAtomsFromIndex directory atomType
            |> Result.lift (addItemsToMap >> Map.toArray)
            |> Result.bind (makeCacheFile directory file)
        else Map.empty |> addItemsToMap |> Map.toArray |> makeCacheFile directory file

    let saveAtoms directory (atomType:string) (items:seq<(Graph.UniqueKey * GraphStructure.Node) * Graph.Adjacency<GraphStructure.Relation>>) =
        if typesToIndex |> List.contains atomType
        then saveAtomsToIndex directory atomType items
        else 
            items
            |> Seq.map(fun a -> makeCacheFile directory (sprintf "atom-%s.json" ((a |> fst |> fst).AsString.ToLower())) a)
            |> Seq.toList
            |> Result.ofList
            |> Result.lift(fun _ -> ())

    let initIndex directory =
        [] |> makeCacheFile directory indexFile
        |> Result.lift(fun _ -> [])

    let mergeIntoIndex directory (nodes:list<NodeIndexItem>) =
        loadIndex directory
        |> Result.bind(fun r -> 
            [ r; nodes ] 
            |> Seq.concat
            |> Seq.sortBy(fun n -> n.NodeTypeName, n.NodeId)
            |> Seq.distinct
            |> makeCacheFileCompactList directory indexFile
            |> Result.lift(fun _ -> [r; nodes] |> List.concat ))

    let replaceIndex directory (nodes:list<NodeIndexItem>) =
        nodes |> makeCacheFileCompactList directory indexFile

    /// Scans the index for changed pretty names. If changed, will update
    /// the pretty name on the index.
    let integrityCheckIndex directory =
        loadIndex directory
        |> Result.bind(fun r -> 
            r |> List.toArray |> Array.Parallel.map(fun indexItem ->
                if indexItem.NodeTypeName = "SourceNode" || indexItem.NodeTypeName = "TaxonNode" || indexItem.NodeTypeName = "BioticProxyNode"
                then
                    loadAtom directory indexItem.NodeTypeName indexItem.NodeId
                    |> Result.lift(fun (atom: (Graph.UniqueKey * GraphStructure.Node) * Graph.Adjacency<GraphStructure.Relation>) -> { indexItem with PrettyName = (atom |> fst |> snd).DisplayName() })
                else Ok indexItem
            ) |> Array.toList |> Result.ofList
        ) |> Result.bind (replaceIndex directory)

    // Reorganise index into desired lookup.
    let nodesByType index =
        index
        |> Seq.groupBy(fun i -> i.NodeTypeName)
        |> Seq.map(fun (g,l) -> 
            g, (l |> Seq.map(fun n -> n.NodeId, n.PrettyName) |> Map.ofSeq))
        |> Map.ofSeq

    let loadOrInitGraph<'node, 'rel> directory =
        if not <| Directory.Exists directory
        then Error <| sprintf "Directory does not exist 1 (%s)" directory
        else
            let index =
                if File.Exists (Path.Combine(directory, indexFile))
                then loadIndex directory
                else initIndex directory

            // Reorganise index into desired lookup.
            let nodesByType = Result.lift nodesByType index

            let graph : Result<Graph.Graph<'node,'rel>,string> =
                index 
                |> Result.bind(fun i ->
                    i 
                    |> List.groupBy(fun i -> i.NodeTypeName)
                    |> List.map(fun (nodeType, nodes) ->
                        loadAtoms directory nodeType (nodes |> List.map(fun n -> n.NodeId)))
                    |> Result.ofList
                    |> Result.map List.concat
                )
            
            (fun g i -> FileBasedGraph { Graph = g; NodesByType = i; Directory = directory })
            <!> graph
            <*> nodesByType

    /// Fetch a node by it's key
    let atomByKey<'c> key (graph:FileBasedGraph<GraphStructure.Node,GraphStructure.Relation>) =
        (unwrap graph).Graph |> Seq.tryFind(fun (n,_) -> fst n = key)

    /// Fetch a node by it's key
    let atomFriendlyNameByKey<'c> key (graph:FileBasedGraph<GraphStructure.Node,GraphStructure.Relation>) =
        //(unwrap graph).NodesByType |> Map.tryFind (typeof<'c>).Name |> Option.bind (Map.tryFind key)
        // TODO Use index (but not working at the monent?)
        (unwrap graph).Graph |> Graph.getAtom key |> Option.map(fun n -> (n |> fst |> snd).DisplayName())

    /// Fetch a node by it's key
    let atomsByKey<'c> (graph:FileBasedGraph<GraphStructure.Node,GraphStructure.Relation>) keys =
        keys
        |> Seq.choose(fun guid ->
            (unwrap graph).Graph |> Seq.tryFind(fun (n,_) -> fst n = guid)
        )

    let tryFindRelationFriendlyName<'rel> relation store atom = 
        atom
        |> GraphStructure.Relations.nodeIdsByRelation<'rel> relation
        |> Seq.tryHead
        |> Option.bind (fun k -> atomFriendlyNameByKey k store)

    let tryFindRelationNode<'rel> relation store atom = 
        atom
        |> GraphStructure.Relations.nodeIdsByRelation<'rel> relation
        |> Seq.tryHead
        |> Option.bind (fun k -> atomByKey k store)

    let tryFindRelationNodes<'rel> relation store atom =
        atom
        |> GraphStructure.Relations.nodeIdsByRelation<'rel> relation
        |> atomsByKey store


    let updateNode' updatedGraph (updatedAtom:(Graph.UniqueKey * GraphStructure.Node) * Graph.Adjacency<GraphStructure.Relation>) fileGraph =
        result {
            // 1. Update the index item (only if the pretty name has changed).
            let nodeType = (updatedAtom |> fst |> snd).NodeType()
            let! oldIndexedPrettyName = 
                (unwrap fileGraph).NodesByType 
                |> Map.tryFind nodeType 
                |> Option.bind (Map.tryFind (updatedAtom |> fst |> fst)) 
                |> Result.ofOption "Could not find old index item"
            let! newIndex =
                if oldIndexedPrettyName <> (updatedAtom |> fst |> snd).DisplayName()
                then
                    result {
                        let! oldIndex = loadIndex (unwrap fileGraph).Directory
                        let! oldIndexItem = 
                            oldIndex |> Seq.tryFind(fun i -> i.NodeId = (updatedAtom |> fst |> fst)) 
                            |> Result.ofOption (sprintf "Could not locate node id '%A' (%s) in node index" (updatedAtom |> fst |> fst) oldIndexedPrettyName)
                        let newIndexItem = {
                                NodeId = (updatedAtom |> fst |> fst)
                                NodeTypeName = (updatedAtom |> fst |> snd).NodeType()
                                PrettyName = (updatedAtom |> fst |> snd).DisplayName() }
                        return Some (oldIndex |> List.except [oldIndexItem] |> List.append [ newIndexItem ] |> List.sortBy(fun n -> n.NodeTypeName, n.NodeId))
                    }
                else Ok None
            if newIndex.IsSome
            then do! replaceIndex (unwrap fileGraph).Directory newIndex.Value
            else do! Ok()

            // 2. Update the individual cached file.
            do! saveAtom (unwrap fileGraph).Directory ((updatedAtom |> fst |> snd).NodeType()) ((updatedAtom |> fst |> fst)) updatedAtom
            
            // 3. Update file-based graph record.
            let updatedNodesByType =
                if newIndex.IsSome then nodesByType newIndex.Value
                else (unwrap fileGraph).NodesByType
            return FileBasedGraph { (unwrap fileGraph) with NodesByType = updatedNodesByType; Graph = updatedGraph }
        }

    /// Update the data associated with a node.
    let updateNode (fileGraph:FileBasedGraph<GraphStructure.Node, GraphStructure.Relation>) (node:Graph.UniqueKey * GraphStructure.Node) =
        result {
            let! updatedGraph = (unwrap fileGraph).Graph |> Graph.replaceNodeData node
            let! updatedAtom = updatedGraph |> Graph.getAtom (fst node) |> Result.ofOption (sprintf "Could not find atom %O" (fst node))
            return! updateNode' updatedGraph updatedAtom fileGraph
        }

    /// Add a relationship between two nodes.
    let addRelation sourceAtom sinkAtom relation (fileGraph:FileBasedGraph<GraphStructure.Node, GraphStructure.Relation>) =
        result {
            let! updatedGraph = GraphStructure.Relations.addRelation sourceAtom sinkAtom relation 1 (unwrap fileGraph).Graph
            let! updatedSourceAtom = updatedGraph |> Graph.getAtom (sourceAtom |> fst |> fst) |> Result.ofOption (sprintf "Could not find atom: %O" (sourceAtom |> fst |> fst))
            return! updateNode' updatedGraph updatedSourceAtom fileGraph
        }

    /// Add a relationship between two nodes.
    let addRelationByKey (fileGraph:FileBasedGraph<GraphStructure.Node, GraphStructure.Relation>) (source:Graph.UniqueKey) (sink:Graph.UniqueKey) relation =
        result {
            let! sourceAtom = atomByKey source fileGraph |> Result.ofOption "Source node did not exist"
            let! sinkAtom = atomByKey sink fileGraph |> Result.ofOption "Sink node did not exist"
            let! updatedGraph = GraphStructure.Relations.addRelation sourceAtom sinkAtom relation 1 (unwrap fileGraph).Graph
            let! updatedSourceAtom = updatedGraph |> Graph.getAtom (sourceAtom |> fst |> fst) |> Result.ofOption (sprintf "Could not find atom: %O" (sourceAtom |> fst |> fst))
            return! updateNode' updatedGraph updatedSourceAtom fileGraph
        }

    /// Add nodes - updating the file-based graph index and individual node files.
    let addNodes' addFn (fileGraph:FileBasedGraph<GraphStructure.Node, GraphStructure.Relation>) nodes =
        
        addFn nodes (unwrap fileGraph).Graph
        |> Result.bind(fun (updatedGraph, addedNodes) ->

            // Save nodes to cache (individual files).
            let newAtoms = 
                addedNodes
                |> List.map fst
                |> List.choose(fun i -> updatedGraph |> Graph.getAtom i)

            if newAtoms.Length <> (nodes |> Seq.length)
            then Error "Problem saving new graph nodes."
            else
                let saveNodes () =
                    newAtoms
                    |> List.groupBy(fun ((_,(n:GraphStructure.Node)),_) -> n.NodeType())
                    |> List.map (fun (nodeType, atoms) -> 
                        saveAtoms (unwrap fileGraph).Directory nodeType atoms
                        )
                    |> Result.ofList

                // Save nodes to cached index:
                let saveIndex () =
                    newAtoms
                    |> List.map (fun atom -> 
                        { NodeId = (fst (fst atom))
                          NodeTypeName = ((atom |> fst |> snd).NodeType())
                          PrettyName = (atom |> fst |> snd).DisplayName() })
                    |> mergeIntoIndex (unwrap fileGraph).Directory

                saveNodes ()
                |> Result.bind (fun _ -> saveIndex())
                |> Result.lift(fun index -> nodesByType index)
                |> Result.lift (fun nodesByType -> 
                    FileBasedGraph { unwrap fileGraph with Graph = updatedGraph; NodesByType = nodesByType }, newAtoms)
        )

    /// Add nodes - updating the file-based graph index and individual node files.
    let addNodes fileGraph nodes = 
        addNodes' (Graph.addNodeData GraphStructure.makeUniqueKey) fileGraph nodes

    /// Add nodes - but skip over them if they already exist.
    let addOrSkipNodes fileGraph nodes =
        addNodes' (Graph.addNodeDataOrSkip GraphStructure.makeUniqueKey) fileGraph nodes

    /// For adding an atom manually (from seeding)
    let private addAtomsUnsafe fileGraph nodes =
        addNodes' (fun i graph -> (graph |> List.append i, i |> List.map fst) |> Ok) fileGraph nodes

    /// Add Seed data to an existing graph database. This should ideally be blank
    /// to avoid any conflicts occurring.
    let seedGraph (fileGraph:FileBasedGraph<GraphStructure.Node, GraphStructure.Relation>) = 
        result {
            let! seeded = Seed.initGraph()
            let groupedByType = 
                seeded
                |> Seq.groupBy(fun (node,_) -> (snd node).NodeType())
            return! Seq.fold(fun state (_, atoms) ->
                state
                |> Result.bind(fun s -> addAtomsUnsafe s (atoms |> Seq.toList) |> Result.map fst)
                ) (Ok fileGraph) groupedByType
        }

    let tryFindTaxonByName taxon graph =
        let latinName = function
            | Population.Taxonomy.Life -> "Life" |> FieldDataTypes.Text.createShort
            | Population.Taxonomy.Kingdom l -> Ok l
            | Population.Taxonomy.Phylum l -> Ok l
            | Population.Taxonomy.Class l -> Ok l
            | Population.Taxonomy.Clade l -> Ok l
            | Population.Taxonomy.Order l -> Ok l
            | Population.Taxonomy.Family l -> Ok l
            | Population.Taxonomy.Subfamily l -> Ok l
            | Population.Taxonomy.Tribe l -> Ok l
            | Population.Taxonomy.Subtribe l -> Ok l
            | Population.Taxonomy.Genus l -> Ok l
            | Population.Taxonomy.Subgenus l -> Ok l
            | Population.Taxonomy.Species (g,s,a) -> sprintf "%s %s" g.Value s.Value |> FieldDataTypes.Text.createShort
            | Population.Taxonomy.Subspecies (g,s,ssp,a) -> sprintf "%s %s ssp. %s" g.Value s.Value ssp.Value |> FieldDataTypes.Text.createShort
            | Population.Taxonomy.Variety (g,s,ssp,a) -> sprintf "%s %s var. %s" g.Value s.Value ssp.Value |> FieldDataTypes.Text.createShort
        (unwrap graph).Graph |> GraphStructure.Nodes.tryFindTaxon(fun t ->
            match latinName t with
            | Ok t -> t = taxon
            | Error _ -> false )

    let tryFindProxy proxy graph =
        (unwrap graph).Graph |> GraphStructure.Nodes.tryFindProxy (fun t -> t = proxy)

    /// Adds a 'proxied taxon' intermediary node to the graph. This node represents
    /// an 'occurrence' of a combination of inference method, biotic proxy, and taxon
    /// that are only valid for one time and study.
    let addProxiedTaxon' (edge:Population.ProxiedTaxon.ProxiedTaxonHyperEdge) graph = result {
        // Get the three included nodes:
        let! existingProxy = (unwrap graph).Graph |> GraphStructure.Nodes.tryFindProxy (fun n -> n = edge.InferredFrom), "proxy doesn't exist"
        let! existingInfer = (unwrap graph).Graph |> GraphStructure.Nodes.tryFindInferenceMethod (fun n -> n = edge.InferredUsing), "infer doesn't exist"
        let existingTaxa = 
            (edge.InferredAs :: edge.InferredAsAdditional)
            |> List.choose (fun t -> (unwrap graph).Graph |> GraphStructure.Nodes.tryFindTaxon (fun n -> n = t))
        let! proxiedGraph, addedNodes = addNodes graph [GraphStructure.PopulationNode GraphStructure.ProxiedTaxonNode]
        // Add relations that make the intermediate node encode the hyper-edge:
        return!
            proxiedGraph
            |> addRelation addedNodes.Head existingProxy (GraphStructure.ProposedRelation.Population Population.PopulationRelation.InferredFrom)
            |> Result.bind (addRelation addedNodes.Head existingProxy (GraphStructure.ProposedRelation.Population Population.PopulationRelation.InferredFrom))
            |> Result.bind (addRelation addedNodes.Head existingInfer (GraphStructure.ProposedRelation.Population Population.PopulationRelation.InferredUsing))
            |> Result.bind (fun graph ->
                existingTaxa |> List.fold(fun g t -> 
                    g |> Result.bind (addRelation addedNodes.Head t (GraphStructure.ProposedRelation.Population Population.PopulationRelation.InferredAs))
                ) (Ok graph)
            )
            |> Result.lift(fun g -> g, addedNodes.Head |> fst |> fst)
    }

    let addProxiedTaxon existingProxy existingTaxon existingAdditionalTaxa existingInfer fileGraph =
        if (existingTaxon :: existingAdditionalTaxa) |> List.distinct |> List.length <> ((existingTaxon :: existingAdditionalTaxa) |> List.length)
        then Error "Cannot add proxied taxon inferred as the same taxon more than once"
        else addProxiedTaxon' {InferredFrom = existingProxy; InferredUsing = existingInfer; InferredAs = existingTaxon; InferredAsAdditional = existingAdditionalTaxa } fileGraph