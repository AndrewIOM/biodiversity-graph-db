module FormTests

open System
open Xunit
open BiodiversityCoder.Core    

module ``When querying the Global Pollen Project`` =
    
    open TaxonomicBackbone.GlobalPollenProject

    [<Fact>]
    let ``works for Salix herbacea`` () =
        let result = lookup "Species" "" "Salix" "herbacea" ""
        match result with
        | Ok r -> 
            Assert.True(true)
            // Assert.IsType(typeof<TaxonNode>, r)
            // Assert.Equal(Life, r :?> TaxonNode)
        | Error (e: string) -> Assert.True(false, e)
