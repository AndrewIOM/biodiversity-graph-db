namespace BiodiversityCoder.Core

open FieldDataTypes

/// Contains types for working literature and
/// other data sources.
module Sources =

    type ArticleMetadataNode = {
        Author: Text.Text option
        Title: Text.Text option
        Journal: Text.ShortText option
        Year: int option
        Volume: int option
        Number: int option
        Pages: (int * int) option
        Month: string option
        DataAvailability: DataAvailability
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
    type Source =
        | Bibliographic of ArticleMetadataNode
        | GreyLiterature of GreySourceNode
        | DarkData of DarkDataNode

    type SourceNode =
        | Included of Source
        | Unscreened of Source
        | Excluded of Source * because:ExclusionReason * notes:Text.Text

    and ExclusionReason =
        | BiotaNotPresent
        | NotArcticRegion
        | NoYearsInHolocene
        | NotTimeSeries
        | NoBiodiversityMeasure
        | IsNotProxyMethod
        | IsExperimentalDesign
        | IsOceanic
        | IsSpaceForTime
        | OtherExclusionReason

    type SourceNodeRelation =
        | HasTemporalExtent of SourceNode * Exposure.StudyTimeline.IndividualTimelineNode
        | ContainsPrimarySource of SourceNode * SourceNode

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
                    Year = Some <| int m.Groups.[5].Value
                    Volume = Some <| int m.Groups.[6].Value
                    Number = Some <| int m.Groups.[7].Value
                    Pages = Some (int m.Groups.[8].Value, int m.Groups.[9].Value)
                    Month = Some <| m.Groups.[10].Value
                    DataAvailability = NotAttachedToSource
                }) |> Seq.toList |> Ok
        else Error "No sources identified"

module ColandrParser =

    open FSharp.Data
    open System.IO
    open Sources

    type Colandr = CsvProvider<"colandr-output.csv">

    let private tryToInt (s:string) = 
        match System.Int32.TryParse s with
        | true, v -> Some v
        | false, _ -> None

    let syncColandr file =
        if File.Exists(file) then
            let colandr = Colandr.Load file
            colandr.Rows 
            |> Seq.where(fun row -> row.Citation_screening_status = "included")
            |> Seq.map(fun row ->
                Bibliographic {
                    Author = Text.create row.Citation_authors |> Result.toOption
                    Title = Text.create row.Citation_title |> Result.toOption
                    Journal = Text.createShort row.Citation_journal_name |> Result.toOption
                    Year = tryToInt row.Citation_pub_year
                    Volume = if row.Citation_journal_volume.HasValue then Some (row.Citation_journal_volume.Value) else None
                    Number = None
                    Pages = None
                    Month = None
                    DataAvailability = NotAttachedToSource
                }) |> Ok
        else Error "Colandr file does not exist"