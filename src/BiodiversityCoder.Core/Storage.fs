namespace BiodiversityCoder.Core

/// IO for storing the graph database.
/// Nodes are stored as individual files.
module Storage =

    // Given a working folder, entities and relations are serialised into
    // files where file names are keys and json are values.
    let workingFolder = "~/Desktop/test-graph"
    
    /// Read all nodes of the type given in the folder.
    let getAllNodes<'T> folder =
        // 1. Translate 'T into a node label.
        invalidOp "Not implemented"

    /// A file-based graph structure consisting of individual json
    /// files and a json index contained within a local file system folder.
    type FileBasedGraph = {
        Graph: Graph.Graph<GraphStructure.Node, GraphStructure.Relation>
    } with
        member this.NodesByType<'a> () = this.Graph.Head |> fst |> snd

    open System.IO

    let loadIndex fileName =
        failwith "not finished"

    let initIndex fileName =
        failwith "not finished"

    let loadOrInitGraph directory =
        if not <| Directory.Exists directory
        then Error "Directory does not exist"
        else
            let index =
                if File.Exists (Path.Combine(directory, "node-index.json"))
                then loadIndex (Path.Combine(directory, "node-index.json"))
                else initIndex (Path.Combine(directory, "node-index.json"))

            index