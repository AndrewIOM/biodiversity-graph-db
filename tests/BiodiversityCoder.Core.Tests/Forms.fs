module FormTests

open System
open Xunit
open BiodiversityCoder.Core    
open BiodiversityCoder.Core.GraphStructure

let assertOk r =
    Assert.True(match r with | Ok _ -> true | Error _ -> false)

let assertResult f r =
    match r with | Ok o -> f o | Error _ -> ()

let getOk r = match r with | Ok r -> r | Error _ -> failwith "Not an OK result"

module ``When converting a view model to an instance`` =
    
    open BiodiversityCoder.Core.Create
    open BiodiversityCoder.Core.Population.Taxonomy
    open BiodiversityCoder.Core.FieldDataTypes
    open BiodiversityCoder.Core.Population.BioticProxies
    open BiodiversityCoder.Core.Sources

    [<Fact>]
    let ``works with a DU with no fields`` () =
        let vm: Result<obj,string> = 
            DU("Life", NotEnteredYet)
            |> createFromViewModel (typeof<TaxonNode>)
        match vm with
        | Ok r -> 
            Assert.IsType(typeof<TaxonNode>, r)
            Assert.Equal(Life, r :?> TaxonNode)
        | Error (e: string) -> Assert.True(false, e)

    [<Fact>]
    let ``works with a DU with single field`` () =
        let vm: Result<obj,string> = 
            DU("Family", Fields([ "name", FieldValue(Text "Betula") ] |> Map.ofList))
            |> createFromViewModel (typeof<TaxonNode>)
        match vm with
        | Ok r -> Assert.Equal(Family((Text.ShortText.TryCreate(SimpleValue.Text "Betula").Value)), r :?> TaxonNode)
        | Error e -> Assert.True(false, e)
    
    [<Fact>]
    let ``works with a DU with multiple fields`` () =
        let vm: Result<obj,string> = 
            DU("Subspecies", Fields([ 
                "generic", FieldValue(Text "Betula")
                "specific", FieldValue(Text "pendula")
                "subspecific", FieldValue(Text "mandshurica") ] |> Map.ofList))
            |> createFromViewModel (typeof<TaxonNode>)
        match vm with
        | Ok r -> Assert.Equal(Subspecies(
            (Text.ShortText.TryCreate(SimpleValue.Text "Betula").Value),
            (Text.ShortText.TryCreate(SimpleValue.Text "pendula").Value),
            (Text.ShortText.TryCreate(SimpleValue.Text "mandshurica").Value)), r :?> TaxonNode)
        | Error e -> Assert.True(false, e)

    [<Fact>]
    let ``works with nested DU type`` () =
        let vm: Result<obj,string> = 
            DU("Morphotype", 
                DU("Microfossil", Fields([
                    "proxyGroup", DU("Pollen", NotEnteredYet)
                    "morphotypeName", FieldValue(Text "Salix-type")
                ] |> Map.ofList))
            ) |> createFromViewModel (typeof<BioticProxyNode>)
        match vm with
        | Ok r -> 
            let expected = Morphotype <| Microfossil(Pollen, (Text.ShortText.TryCreate(SimpleValue.Text "Salix-type").Value))
            Assert.Equal(expected, r :?> BioticProxyNode)
        | Error e -> Assert.True(false, e)

    [<Fact>]
    let ``works with DU containing fields of records and DU types`` () =
        let vm: Result<obj,string> = 
            DU("GreyLiterature", Fields([
                "Item", Fields([
                    "Contact", Fields([
                        "FirstName", FieldValue (Text "Hello")
                        "LastName", FieldValue (Text "Hello")
                    ] |> Map.ofList)
                    "License", DU("Creative Commons Attribution 4 International", NotEnteredYet)
                    "Title", FieldValue (Text "Piece of paper found in a cupboard")
                    ] |> Map.ofList)
            ] |> Map.ofList )) |> createFromViewModel (typeof<SourceNode>)
        match vm with
        | Ok r -> 
            let expected = 
                GreyLiterature {
                    Contact = {
                        FirstName = (Text.ShortText.TryCreate(SimpleValue.Text "Hello").Value)
                        LastName = (Text.ShortText.TryCreate(SimpleValue.Text "Hello").Value)
                    }
                    License = ``Creative Commons Attribution 4 International``
                    Title = (Text.Text.TryCreate(SimpleValue.Text "Piece of paper found in a cupboard").Value)
                }
            Assert.Equal(expected, r :?> SourceNode)
        | Error e -> Assert.True(false, e)