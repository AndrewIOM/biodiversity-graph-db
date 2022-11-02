module TaxonomicIdentityTests

open System
open Xunit
open BiodiversityCoder.Core    
open BiodiversityCoder.Core.GraphStructure

module ``When making a graph`` =

    let g : Graph.Graph<Node,Relation> = Graph.empty
    let outcomeNode = OutcomeNode(MeasureNode(Outcomes.Biodiversity.Abundance))

    [<Fact>]
    let ``it starts with no nodes`` () =
        Assert.Empty g

    [<Fact>]
    let ``nodes cannot have duplicate ids`` () =
        let result = 
            g 
            |> Graph.addNode (Graph.FriendlyKey("a","v"), outcomeNode)
            |> Result.bind (Graph.addNode (Graph.FriendlyKey("a","v"), outcomeNode))
        Assert.True(match result with | Error _ -> true | _ -> false)

module ``When seeding a graph`` =

    [<Fact>]
    let ``there are no errors`` () =
        let result = Seed.initGraph()
        match result with
        | Ok _ -> Assert.True(true)
        | Error e -> Assert.True(false, e)


module ``When lookup up relation constraints`` =

    open BiodiversityCoder.Core.Population

    let g : Graph.Graph<Node,Relation> = Graph.empty

    // let relate relation g =
    //     result {
    //             let! plantae = FieldDataTypes.Text.createShort "Plantae"
    //             let graph = 
    //                 g |> Graph.addNodeData [
    //                     PopulationNode (TaxonomyNode Taxonomy.Life)
    //                     PopulationNode (TaxonomyNode <| Taxonomy.Kingdom plantae)
    //                 ] |> fst
    //             let! source = Nodes.tryFindTaxon (fun n -> n = Taxonomy.Kingdom plantae) graph, "Could not get source"
    //             let! sink = Nodes.tryFindTaxon (fun n -> n = Taxonomy.Life) graph, "Could not get sink"
    //             return! Relations.addRelation source sink relation 1 graph
    //         }

    // [<Fact>]
    // let ``can add a valid relation`` () =
    //     match g |> relate (Population IsA) with
    //     | Ok g -> Assert.True(g |> Seq.head |> snd |> Seq.isEmpty)
    //     | Error _ -> Assert.True(false)

    // [<Fact>]
    // let ``errors on invalid relation`` () =
    //     match g |> relate (Population HasLabel) with
    //     | Ok _ -> Assert.True(false)
    //     | Error _ -> Assert.True(true)
