namespace BiodiversityCoder.Core

open FieldDataTypes

/// Contains types for working literature and
/// other data sources.
module Sources =

    type ArticleMetadataNode = {
        Author: Text.Text option
        Title: Text.Text option
        Journal: Text.ShortText option
        Year: int
        Volume: int
        Number: int
        Pages: int * int
        Month: string
        DataAvailability: DataAvailability option
    }

    and DataAvailability =
        | AtDoiClosed of System.Uri
        | AtDoiOpen of System.Uri
        | NotAttachedToSource

    type GreySourceNode = {
        Contact: Person
        License: License
        Title: Text.Text
    }

    type DarkDataNode = {
        Contact: Person
        License: License
        Details: Text.Text
    }

    /// A graph database node representing a source of information.
    type SourceNode =
        | Bibliographic of ArticleMetadataNode
        | GreyLiterature of GreySourceNode
        | DarkData of DarkDataNode

    type SourceNodeRelation =
        | HasTemporalExtent of SourceNode * Exposure.StudyTimeline.IndividualTimelineNode

    and SourceRelation =
        | HasTemporalExtent


module BibtexParser =
    
    open System.Text.RegularExpressions
    open Sources
    
    let articleRegex = "@article{(.*),\nauthor = {(.*)},\ntitle = {(.*)},\njournal = {(.*)},\nyear = (.*),\nvolume = {(.*)},\nnumber = {(.*)},\npages = {(.*)--(.*)},\nmonth = {(.*)}}"
    
    let parse (bibText:string) =
        if Regex.IsMatch(bibText, articleRegex)
        then
            let matches = Regex.Matches(bibText, articleRegex)
            matches
            |> Seq.cast<Match>
            |> Seq.map(fun m ->
                Bibliographic {
                    Author = Text.create m.Groups.[2].Value |> Result.toOption
                    Title = Text.create m.Groups.[3].Value |> Result.toOption
                    Journal = Text.createShort m.Groups.[4].Value |> Result.toOption
                    Year = int m.Groups.[5].Value
                    Volume = int m.Groups.[6].Value
                    Number = int m.Groups.[7].Value
                    Pages = int m.Groups.[8].Value, int m.Groups.[9].Value
                    Month = m.Groups.[10].Value
                    DataAvailability = None
                }) |> Seq.toList |> Ok
        else Error "No sources identified"
