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

    let indexFile = "atom-index.json"

    let loadCacheFile<'a> directory filename : Result<'a,string> =
        if Directory.Exists directory
        then 
            if File.Exists(Path.Combine(directory, filename))
            then 
                Path.Combine(directory, filename)
                |> Compact.deserializeFile
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

    let loadIndex directory : Result<NodeIndexItem list,string> =
        loadCacheFile directory indexFile

    let loadAtom directory (atomType:string) (atomKey:string) : Result<Graph.Atom<'a,'b>,string> =
        loadCacheFile directory (sprintf "atom-%s-%s.json" (atomType.ToLower()) (atomKey.ToLower()))

    let saveAtom directory (atomType:string) (atomKey:string) item =
        makeCacheFile directory (sprintf "atom-%s-%s.json" (atomType.ToLower()) (atomKey.ToLower())) item

    let initIndex directory =
        [] |> makeCacheFile directory indexFile
        |> Result.lift(fun _ -> [])

    let replaceIndex directory (nodes:list<NodeIndexItem>) =
        nodes |> makeCacheFile directory indexFile
        |> Result.lift(fun _ -> [])

    /// Read all nodes of the type given in the folder.
    let getAllNodes<'T> folder =
        // 1. Translate 'T into a node label.
        invalidOp "Not implemented"

    let loadOrInitGraph<'node, 'rel> directory =
        if not <| Directory.Exists directory
        then Error <| sprintf "Directory does not exist 1 (%s)" directory
        else
            let index =
                if File.Exists (Path.Combine(directory, indexFile))
                then loadIndex directory
                else initIndex directory

            // Reorganise index into desired lookup.
            let lookup =
                index
                |> Result.lift(fun r ->
                    r 
                    |> Seq.groupBy(fun i -> i.NodeTypeName)
                    |> Seq.map(fun (g,l) -> 
                        g, (l |> Seq.map(fun n -> n.NodeKey, n.PrettyName) |> Map.ofSeq))
                    |> Map.ofSeq
                )

            let graph : Result<Graph.Graph<'node,'rel>,string> =
                index 
                |> Result.bind(fun i ->
                    i |> List.map(fun ni ->
                        loadAtom directory ni.NodeTypeName ni.NodeKey
                    ) |> Result.ofList
                )
            
            (fun g i -> FileBasedGraph { Graph = g; NodesByType = i; Directory = directory })
            <!> graph
            <*> lookup

    let addNodes (fileGraph:FileBasedGraph<GraphStructure.Node, GraphStructure.Relation>) (nodes:GraphStructure.Node list) = 
        let updatedGraph, addedNodes = 
            Graph.addNodeData nodes (unwrap fileGraph).Graph
        
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
                |> List.zip nodes
                |> List.map (fun (n, atom) -> 
                    saveAtom (unwrap fileGraph).Directory (n.GetType().Name) (n.Key()) atom
                    )
                |> Result.ofList

            // Save nodes to cached index:
            let saveIndex () =
                newAtoms
                |> List.zip nodes
                |> List.map (fun ((n: ^T),atom) -> 
                    { NodeId = (fst (fst atom))
                      NodeTypeName = (n.GetType().Name)
                      NodeKey = n.Key()
                      PrettyName = n.DisplayName() })
                |> replaceIndex (unwrap fileGraph).Directory

            saveNodes ()
            |> Result.bind (fun _ -> saveIndex())
            |> Result.lift (fun _ -> FileBasedGraph { unwrap fileGraph with Graph = updatedGraph })
