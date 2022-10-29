namespace BiodiversityCoder.Core

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
        NodesByType: Map<string, Map<string, string>>
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
        NodeId: System.Guid
        NodeTypeName: string
        NodeKey: string
        PrettyName: string
    }

    /// Types that are indexed in a single file rather than individual files.
    let typesToIndex = [ 
        typeof<Exposure.TemporalIndex.CalYearNode>.Name ]

    let loadIndex directory : Result<NodeIndexItem list,string> =
        loadCacheFile directory indexFile

    /// Allow saving specific types into a combined file rather than individual files.
    /// Useful for time nodes, which are plentiful and very small data-wise.
    let loadAtomsFromIndex directory (atomType:string) : Result<Map<string,Graph.Atom<'a,'b>>,string> =
        loadCacheFile directory (sprintf "atom-%s-index.json" (atomType.ToLower()))

    let loadAtom directory (atomType:string) (atomKey:string) : Result<Graph.Atom<'a,'b>,string> =
        if typesToIndex |> List.contains atomType
        then 
            loadAtomsFromIndex directory atomType 
            |> Result.bind(fun m -> m.TryFind atomKey |> Result.ofOption)
        else loadCacheFile directory (sprintf "atom-%s-%s.json" (atomType.ToLower()) (atomKey.ToLower()))

    let saveAtomToIndex directory (atomType:string) atomKey item =
        match loadAtomsFromIndex directory (sprintf "atom-%s-index.json" (atomType.ToLower())) with
        | Ok items -> items |> Map.add atomKey item
        | Error _ -> Map.empty |> Map.add atomKey item
        |> makeCacheFile directory (sprintf "atom-%s-index.json" (atomType.ToLower()))

    let saveAtom directory (atomType:string) (atomKey:string) item =
        if typesToIndex |> List.contains atomType
        then saveAtomToIndex directory atomType atomKey item
        else makeCacheFile directory (sprintf "atom-%s-%s.json" (atomType.ToLower()) (atomKey.ToLower())) item

    let initIndex directory =
        [] |> makeCacheFile directory indexFile
        |> Result.lift(fun _ -> [])

    let mergeIntoIndex directory (nodes:list<NodeIndexItem>) =
        loadIndex directory
        |> Result.bind(fun r -> 
            [ r; nodes ] 
            |> Seq.concat 
            |> makeCacheFile directory indexFile
            |> Result.lift(fun _ -> [r; nodes] |> List.concat ))

    let replaceIndex directory (nodes:list<NodeIndexItem>) =
        nodes |> makeCacheFile directory indexFile

    // Reorganise index into desired lookup.
    let nodesByType index =
        index
        |> Seq.groupBy(fun i -> i.NodeTypeName)
        |> Seq.map(fun (g,l) -> 
            g, (l |> Seq.map(fun n -> n.NodeKey, n.PrettyName) |> Map.ofSeq))
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
                    i |> List.map(fun ni ->
                        loadAtom directory ni.NodeTypeName ni.NodeKey
                    ) |> Result.ofList
                )
            
            (fun g i -> FileBasedGraph { Graph = g; NodesByType = i; Directory = directory })
            <!> graph
            <*> nodesByType

    /// Fetch a node by it's key
    let atomByKey<'c> key (graph:FileBasedGraph<GraphStructure.Node,GraphStructure.Relation>) =
        (unwrap graph).Graph |> Seq.tryFind(fun (n,_) -> (snd n).Key() = key)

    /// Fetch a node by it's key
    let atomsByGuid<'c> (graph:FileBasedGraph<GraphStructure.Node,GraphStructure.Relation>) guids =
        guids
        |> Seq.choose(fun guid ->
            (unwrap graph).Graph |> Seq.tryFind(fun (n,_) -> fst n = guid)
        )

    /// Update the data associated with a node.
    let updateNode (fileGraph:FileBasedGraph<GraphStructure.Node, GraphStructure.Relation>) nodeType (node:System.Guid * GraphStructure.Node) =
        result {
            // What do we need to do?
            // 1. Update the graph itself.
            let! updatedGraph = (unwrap fileGraph).Graph |> Graph.replaceNodeData node
            let! updatedAtom = updatedGraph |> Graph.getAtom (fst node) |> Result.ofOption
            
            // 2. Update the index item (the pretty name may have changed).
            let! oldIndex = loadIndex (unwrap fileGraph).Directory
            let oldIndexItem = oldIndex |> Seq.find(fun i -> i.NodeId = fst node)
            let newIndex =
                oldIndex |> List.except [oldIndexItem] |> List.append [{
                    NodeId = (updatedAtom |> fst |> fst)
                    NodeTypeName = nodeType
                    NodeKey = (updatedAtom |> fst |> snd).Key()
                    PrettyName = (updatedAtom |> fst |> snd).DisplayName()
                }]
            let! _ = replaceIndex (unwrap fileGraph).Directory newIndex
            
            // 3. Update the individual cached file.
            let! _ = saveAtom (unwrap fileGraph).Directory nodeType ((snd node).Key()) updatedAtom
            
            // 4. Update file-based graph record.
            let newNodesByType = nodesByType newIndex
            return FileBasedGraph { (unwrap fileGraph) with NodesByType = newNodesByType; Graph = updatedGraph }
        }

    /// Add nodes - updating the file-based graph index and individual node files.
    let addNodes' addFn (fileGraph:FileBasedGraph<GraphStructure.Node, GraphStructure.Relation>) nodeType nodes = 
        let updatedGraph, addedNodes = 
            addFn nodes (unwrap fileGraph).Graph
        
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
                |> List.map (fun (atom: (System.Guid * GraphStructure.Node) * Graph.Adjacency<GraphStructure.Relation>) -> 
                    saveAtom (unwrap fileGraph).Directory nodeType ((atom |> fst |> snd).Key()) atom
                    )
                |> Result.ofList

            // Save nodes to cached index:
            let saveIndex () =
                newAtoms
                |> List.map (fun atom -> 
                    { NodeId = (fst (fst atom))
                      NodeTypeName = nodeType
                      NodeKey = (atom |> fst |> snd).Key()
                      PrettyName = (atom |> fst |> snd).DisplayName() })
                |> mergeIntoIndex (unwrap fileGraph).Directory

            saveNodes ()
            |> Result.bind (fun _ -> saveIndex())
            |> Result.lift(fun index -> nodesByType index)
            |> Result.lift (fun nodesByType -> 
                FileBasedGraph { unwrap fileGraph with Graph = updatedGraph; NodesByType = nodesByType })

    /// Add nodes - updating the file-based graph index and individual node files.
    let addNodes: FileBasedGraph<GraphStructure.Node,GraphStructure.Relation> -> string -> seq<GraphStructure.Node> -> Result<FileBasedGraph<GraphStructure.Node,GraphStructure.Relation>,string> = 
        addNodes' Graph.addNodeData

    /// For adding an atom manually (from seeding)
    let private addAtomsUnsafe =
        addNodes' (fun i graph ->  graph |> List.append i, i |> List.map fst)

    /// Add Seed data to an existing graph database. This should ideally be blank
    /// to avoid any conflicts occurring.
    let seedGraph (fileGraph:FileBasedGraph<GraphStructure.Node, GraphStructure.Relation>) = 
        result {
            let! seeded = Seed.initGraph()
            let groupedByType = 
                seeded
                |> Seq.groupBy(fun (node,_) -> (snd node).NodeType())
            return! Seq.fold(fun state (nodeType, atoms) ->
                state
                |> Result.bind(fun s -> addAtomsUnsafe s nodeType (atoms |> Seq.toList))
                ) (Ok fileGraph) groupedByType
        }
