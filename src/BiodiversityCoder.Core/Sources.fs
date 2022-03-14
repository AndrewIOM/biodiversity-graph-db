namespace BiodiversityCoder.Core

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

    type SourceNode =
        | Source

    type SourceNodeRelation =
        | HasTemporalExtent of SourceNode * Exposure.StudyTimeline.IndividualTimelineNode

    and SourceRelation =
        | HasTemporalExtent

    // TODO Use this somewhere.
    type DataAvailability =
        | Unavailable
        | FullOnline of System.Uri
        | Paywall of System.Uri
        | Doi of System.Uri
        | OnRequest



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
