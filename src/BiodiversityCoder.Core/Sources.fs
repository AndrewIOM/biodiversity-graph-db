namespace BiodiversityCoder.Core

[<AutoOpen>]
module Result =

    let succeed x = 
        Ok x

    let apply f result =
        match f,result with
        | Ok f, Ok x -> 
            f x |> Ok 
        | Error e, Ok _ 
        | Ok _, Error e -> 
            e |> Error
        | Error e1, Error e2 -> 
            e1 |> Error 

    let lift f result =
        let f' =  f |> succeed
        apply f' result

    let (<*>) = apply
    let (<!>) = lift

    let switch f = 
        f >> succeed


/// Contains types for working literature and
/// other data sources.
module Sources =

    type StudyId = Guid

    type ArticleMetadata = {
        //Id: StudyId
        Author: string
        Title: string
        Journal: string
        Year: int
        Volume: int
        Number: int
        Pages: int * int
        Month: string
    }

    type Source =
        | PeerReviewedPaper of ArticleMetadata
        | GreyLiterature
        | BookChapter
        | DarkData


module BibtexParser =
    
    open System.Text.RegularExpressions
    open Sources
    
    let articleRegex = "@article{(.*),\nauthor = {(.*)},\ntitle = {{(.*)}},\njournal = {(.*)},\nyear = {(.*)},\nvolume = {(.*)},\nnumber = {(.*)},\npages = {(.*)--(.*)},\nmonth = (.*\n)}"
    
    let parse (bibText:string) =
        if Regex.IsMatch(bibText, articleRegex)
        then
            let matches = Regex.Matches(bibText, articleRegex)
            matches
            |> Seq.cast<Match>
            |> Seq.map(fun m ->
                PeerReviewedPaper {
                    //Id = System.Guid.NewGuid()
                    Author = m.Groups.[2].Value
                    Title = m.Groups.[3].Value
                    Journal = m.Groups.[4].Value
                    Year = int m.Groups.[5].Value
                    Volume = int m.Groups.[6].Value
                    Number = int m.Groups.[7].Value
                    Pages = int m.Groups.[8].Value, int m.Groups.[9].Value
                    Month = m.Groups.[10].Value
                }) |> Seq.toList |> Ok
        else Error "No sources identified"
