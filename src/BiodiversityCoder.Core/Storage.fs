namespace BiodiversityCoder.Core

/// IO for storing the graph database.
/// Nodes are stored as individual files; each file is updated
/// when 
module Storage =

    // Given a working folder, entities and relations are serialised into
    // files where file names are keys and json are values.
    let workingFolder = "/Users/andrewmartin/Desktop/test-graph"
    
    /// Read all nodes of the type given in the folder.
    let getAllNodes<'T> folder =
        // 1. Translate 'T into a node label.
        invalidOp "Not implemented"
