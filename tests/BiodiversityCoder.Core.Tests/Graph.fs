module TaxonomicIdentityTests

open System
open Xunit
open BiodiversityCoder.Core    
open BiodiversityCoder.Core.GraphStructure

module ``When making a graph`` =

    let g : Graph.Graph<Node,Relation> = Graph.empty
    let outcomeNode = OutcomeNode(MeasureNode(Outcomes.BiodiversityMeasures.Abundance))

    [<Fact>]
    let ``it starts with no nodes`` () =
        Assert.Empty g

    [<Fact>]
    let ``nodes are added with sequential ids`` () =
        let result = g |> Graph.addNodeData (Seq.init 150 (fun _ -> outcomeNode))
        let ids = result |> fst |> Seq.map (fst >> fst)
        Assert.Equal([0 .. 149], ids)
        Assert.Equal(149, result |> snd)

    [<Fact>]
    let ``nodes cannot have duplicate ids`` () =
        Assert.Throws<Exception> (
            fun () ->
                g 
                |> Graph.addNode (1, outcomeNode)
                |> Graph.addNode (1, outcomeNode) |> ignore)
        

module ``when seeding the graph`` =

    let x = 2

// Auto-scaffold the interface based on types?


// module TestGraph =

//     open Graph
//     open GraphStructure

//     open Exposure.TemporalIndex

//     let seedGraph () =

//         let g : Graph<Node,Relation> ref = ref Graph.empty

//         // Population: life node
//         let taxonRoot = Population.Taxonomy.Life

//         // Exposure: Holocene time index
//         let timeIndexNodes = [ 0 .. 14000 ] |> List.map createTimeNode
//         let holoceneLabel = SliceLabelNode <| { Name = "Holocene" }

//         // Outcomes: basic measures
//         let outcomes = [
//             Outcomes.BiodiversityMeasures.Abundance
//             Outcomes.BiodiversityMeasures.PresenceAbsence
//         ]

//         g
//         //|> Graph.addNodeData timeIndexNodes
//         //|> Graph.addRelation holocene

    

    //let makeGraph =

    //    let g = ref Graph.empty

    //    let linkedIds = ref []

    //    let nodes = [ SourceDocumentNode; TimePointNode; SourceDocumentNode ]

    //    g.Value |>

//module Example =
    
//    let holocene = { Name = "Holocene" }
    
//    let timeNodes =
//        [ 0 .. 14000 ]
//        |> List.map(fun i -> CalYear <| i * 1<calYearBP>)

//    let rel =
//        timeNodes |> List.map(fun n -> Contains (holocene,n))

