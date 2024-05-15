namespace BiodiversityCoder.Core

open FieldDataTypes

/// Contains types for working literature and
/// other data sources.
module Sources =

    module RecordTypes =

        type JournalArticle = {
            FirstAuthor: Author.Author
            AdditionalAuthors: Author.Author list
            Title: Text.Text option
            Journal: Text.ShortText option
            Year: int option
            Volume: int option
            Number: int option
            PageRange: IntRange.IntRange option
            Month: Month option
            DOI: DigitalObjectIdentifier.DigitalObjectIdentifier option
        }

        type JournalArticleObsolete = {
            Author: Text.Text option
            Title: Text.Text option
            Journal: Text.ShortText option
            Year: int option
            Volume: int option
            Number: int option
            Pages: (int * int) option
            Month: string option
            DataAvailability: DataAvailabilityUnverified
        }

        and DataAvailabilityUnverified =
            | AtDoiClosed of System.Uri
            | AtDoiOpen of System.Uri
            | NotAttachedToSource

        type Book = {
            BookTitle: Text.Text
            BookSubtitle: Text.Text option
            BookFirstAuthor: Author.Author
            BookAdditionalAuthors: Author.Author list
            BookCopyrightYear: int
            Editor: Author.Author option
            ISBN: Text.Text option
            ISSNDOI: Text.Text option
            Publisher: Text.Text option
        }

        type BookChapter = {
            ChapterFirstAuthor: Author.Author
            ChapterAdditionalAuthors: Author.Author list
            ChapterTitle: Text.Text
            FirstPage: int
        }

        type Database = {
            Abbreviation: Text.ShortText
            FullName: Text.Text
            Location: System.Uri
        }

        and DatabaseDataset = {
            DatabaseAbbreviation: Text.ShortText
            UniqueIdentifierInDatabase: Text.ShortText
            Investigators: Person list
            Title: Text.ShortText option
            WebLocation: System.Uri option
        }

        type GreyFormat =
            | Blog | ConferencePaper | ConferenceProceeding | Dataset | GovernmentDocument
            | GovernmentReport | Newsletter | Pamphet | PolicyStatement | Preprint
            | PressRelease | ResearchReport | StatisticalReport | WorkingPaper
            | Other of Text.ShortText

        type GreySource = {
            Format: GreyFormat
            Contributors: Author.Author list
            Title: Text.Text
            Institution: Text.ShortText option
            License: License
            PostedYear: int option
            DOI: DigitalObjectIdentifier.DigitalObjectIdentifier option
            Description: Text.Text option
        }

        type IndividualPublishedDataset = {
            Contributors: Author.Author list
            Title: Text.Text
            YearPublished: int option
            Institution: Text.ShortText option
            DOI: DigitalObjectIdentifier.DigitalObjectIdentifier option
            License: License
        }

        type Dissertation = {
            Author: Author.Author
            Title: Text.Text
            Institution: Text.ShortText
            InstitutionDocumentID: Text.ShortText option
            CompletionYear: int
            DOI: DigitalObjectIdentifier.DigitalObjectIdentifier option
        }

        type DarkData = {
            Investigator: Author.Author
            AdditionalInvestigators: Author.Author list
            CompilationYear: int option
            Title: Text.ShortText option
            Details: Text.Text
            Context: Geography.SamplingLocation option
            License: License
        }

        type GreySourceObsolete = {
            Contact: Person
            License: License
            Title: Text.Text
        }

        type DarkDataObsolete = {
            Contact: Person
            License: License
            Details: Text.Text
        }


    type PublishedSource =
        | Book of RecordTypes.Book
        | BookChapter of RecordTypes.BookChapter
        | IndividualDataset of RecordTypes.IndividualPublishedDataset
        | Dissertation of RecordTypes.Dissertation
        | JournalArticle of RecordTypes.JournalArticle

    /// A graph database node representing a source of information.
    type Source =
        | PublishedSource of PublishedSource
        | GreyLiteratureSource of RecordTypes.GreySource
        | DarkDataSource of RecordTypes.DarkData
        | Database of RecordTypes.Database
        | DatabaseEntry of RecordTypes.DatabaseDataset
        // Obsolete:
        | Bibliographic of RecordTypes.JournalArticleObsolete
        | GreyLiterature of RecordTypes.GreySourceObsolete
        | DarkData of RecordTypes.DarkDataObsolete

    type SourceNode =
        | Included of Source * CodingProgress
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

    and CodingProgress =
        | CompletedNone
        | InProgress of completedSections:Text.ShortText list
        | Stalled of completedSections:Text.ShortText list * stalledOnSection:Text.ShortText * reason:Text.Text
        | CompletedAll

    type SourceNodeRelation =
        | HasTemporalExtent of SourceNode * Exposure.StudyTimeline.IndividualTimelineNode
        | UsesPrimarySource of SourceNode * SourceNode
        | UsedDatabase of SourceNode * SourceNode
        | HasDataset of SourceNode * SourceNode

    and SourceRelation =
        | HasTemporalExtent
        | UsesPrimarySource
        | UsedDatabase of accessDate:System.DateOnly option * subset:DatabaseSubset
        | HasDataset
        | IsChapterIn

    and DatabaseSubset =
        | AllRecordsInStudyScope
        | SpecificRecords of firstId:Text.ShortText * additionalIds:Text.ShortText list
        | ComplexSubset of methodDescription:Text.Text


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
                    DataAvailability = RecordTypes.NotAttachedToSource
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
                    DataAvailability = RecordTypes.NotAttachedToSource
                }) |> Ok
        else Error "Colandr file does not exist"