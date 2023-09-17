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
                "subspecific", FieldValue(Text "mandshurica")
                "authorship", FieldValue(Text "L.") ] |> Map.ofList))
            |> createFromViewModel (typeof<TaxonNode>)
        match vm with
        | Ok r -> Assert.Equal(Subspecies(
            (Text.ShortText.TryCreate(SimpleValue.Text "Betula").Value),
            (Text.ShortText.TryCreate(SimpleValue.Text "pendula").Value),
            (Text.ShortText.TryCreate(SimpleValue.Text "mandshurica").Value),
            (Text.ShortText.TryCreate(SimpleValue.Text "L.").Value)), r :?> TaxonNode)
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
            let expected = Morphotype <| Morphotype.Microfossil(Pollen, (Text.ShortText.TryCreate(SimpleValue.Text "Salix-type").Value))
            Assert.Equal(expected, r :?> BioticProxyNode)
        | Error e -> Assert.True(false, e)

    open Exposure.StudyTimeline
    open BiodiversityCoder.Core.Population.Context

    [<Fact>]
    let ``works with list fields`` () =
        let vm: Result<obj,string> = 
            DU("Discontinuous",
                Fields([
                    "resolution", DU("Irregular", NotEnteredYet)
                    "hiatuses", FieldList [
                        0, DU("Hiatus", Fields([
                            "oldest", FieldValue(Number 122.)
                            "youngest", FieldValue(Number 143.)
                        ] |> Map.ofList))
                    ]
                ] |> Map.ofList)
                ) |> createFromViewModel (typeof<Exposure.StudyTimeline.IndividualTimelineNode>)
        match vm with
        | Ok r -> 
            let expected =
                Discontinuous(Irregular, [
                    Hiatus (122.<OldDate.calYearBP>, 143.<OldDate.calYearBP>)
                ])
            Assert.Equal(expected, r :?> Exposure.StudyTimeline.IndividualTimelineNode)
        | Error e -> Assert.True(false, e)


    [<Fact>]
    let ``works with option types`` () =
        let vm: Result<obj,string> = 
                Fields([
                    "Date", DU("RadiocarbonCalibrated", Fields([
                        "calibratedDate", Fields([
                            "CalibratedDate", FieldValue(Number 2000.)
                            "CalibrationCurve", FieldValue(Text "IntCal17")
                            "UncalibratedDate", DU("None", NotEnteredYet) ] |> Map.ofList)
                    ] |> Map.ofList))
                    "MeasurementError", DU("NoDatingErrorSpecified", NotEnteredYet)
                    "Discarded", FieldValue (Boolean false)
                    "MaterialDated", FieldValue (Text "leaves")
                    "SampleDepth", DU("Some", DU("DepthPoint", Fields(["depth", FieldValue (Number 20.)
                    ] |> Map.ofList)))
                ] |> Map.ofList
                ) |> createFromViewModel (typeof<IndividualDateNode>)
        match vm with
        | Ok r -> 
            let expected = {
                Date = OldDate.RadiocarbonCalibrated({
                    CalibratedDate = 2000.<OldDate.calYearBP>
                    CalibrationCurve = Text.createShort("IntCal17") |> Result.forceOk
                    UncalibratedDate = None})
                MaterialDated = Text.createShort("leaves") |> Result.forceOk
                SampleDepth = StratigraphicSequence.createDepth 20. |> Result.map StratigraphicSequence.DepthInCore.DepthPoint |> Result.forceOk |> Some
                Discarded = false
                MeasurementError = OldDate.NoDatingErrorSpecified
            }
            Assert.Equal(expected, r :?> IndividualDateNode)
        | Error e -> Assert.True(false, e)


    [<Fact>]
    let ``works for spatial types`` () =
        let vm: Result<obj,string> = 
                Fields([
                    "Name", FieldValue (Text "Brooks Range")
                    "SamplingLocation", DU("Site", Fields([
                        "latitude", FieldValue(Text "56")
                        "longitude", FieldValue(Text "-170") ] |> Map.ofList))
                    "SampleOrigin", DU ("LakeSediment", DU ("DepthRangeNotStated", NotEnteredYet))
                    "SampleLocationDescription", DU("None", NotEnteredYet)
                ] |> Map.ofList
                ) |> createFromViewModel (typeof<ContextNode>)
        match vm with
        | Ok r -> 
            let expected = {
                Name = Text.createShort("Brooks Range") |> Result.forceOk
                SamplingLocation = Geography.Site(Geography.createLatitude 56. |> forceOk, Geography.createLongitude -170. |> forceOk)
                SampleOrigin = LakeSediment StratigraphicSequence.DepthExtent.DepthRangeNotStated
                SampleLocationDescription = None
            }
            Assert.Equal(expected, r :?> ContextNode)
        | Error e -> Assert.True(false, e)



    // [<Fact>]
    // let ``works with DU containing fields of records and DU types`` () =
    //     let vm: Result<obj,string> = 
    //         DU("GreyLiterature", Fields([
    //             "Item", Fields([
    //                 "Contact", Fields([
    //                     "FirstName", FieldValue (Text "Hello")
    //                     "LastName", FieldValue (Text "Hello")
    //                 ] |> Map.ofList)
    //                 "License", DU("Creative Commons Attribution 4 International", NotEnteredYet)
    //                 "Title", FieldValue (Text "Piece of paper found in a cupboard")
    //                 ] |> Map.ofList)
    //         ] |> Map.ofList )) |> createFromViewModel (typeof<SourceNode>)
    //     match vm with
    //     | Ok r -> 
    //         let expected = 
    //             GreyLiterature {
    //                 Contact = {
    //                     FirstName = (Text.ShortText.TryCreate(SimpleValue.Text "Hello").Value)
    //                     LastName = (Text.ShortText.TryCreate(SimpleValue.Text "Hello").Value)
    //                 }
    //                 License = ``Creative Commons Attribution 4 International``
    //                 Title = (Text.Text.TryCreate(SimpleValue.Text "Piece of paper found in a cupboard").Value)
    //             }
    //         Assert.Equal(expected, r :?> SourceNode)
    //     | Error e -> Assert.True(false, e)

module ``When getting relations`` =

    [<Fact>]
    let works () =
        let nodeData = Population.Taxonomy.Life |> PopulationNode.TaxonomyNode |> Node.PopulationNode
        let node = makeUniqueKey nodeData, nodeData
        let graph =
            Graph.empty
            |> Graph.addNode node |> forceOk
            |> Graph.addRelation (fst node) (fst node) 1 (Population.PopulationRelation.IsA |> GraphStructure.Relation.Population)
            |> forceOk
        let atom = graph |> Graph.getAtom (fst node) |> Result.ofOption "" |> forceOk
        
        let result =
            Relations.nodeIdsByRelation<Population.PopulationRelation> Population.PopulationRelation.IsA atom
            |> Seq.tryHead
        Assert.Equal(result, Some(fst node))

