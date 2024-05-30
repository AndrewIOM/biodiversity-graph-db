namespace BiodiversityCoder.Core

open FieldDataTypes

/// Contains types for working literature and
/// other data sources.
module Sources =

    module RecordTypes =

        type JournalArticle =
            { FirstAuthor: Author.Author
              AdditionalAuthors: Author.Author list
              Title: Text.Text
              Journal: Text.ShortText
              Year: int
              Volume: int option
              Number: int option
              [<Name "Page Range">]
              [<Help "If known, the page range in the format X - Y e.g. 20 - 24">]
              PageRange: IntRange.IntRange option
              Month: Month option
              [<Name "DOI (Digital Object Identifier)">]
              [<Help "If this article has a DOI, you can specify it either without or with the full URL (e.g. 10.1080/10509585.2015.1092083 or https://doi.org/10.1080/10509585.2015.1092083).">]
              DOI: DigitalObjectIdentifier.DigitalObjectIdentifier option }

        type JournalArticleObsolete =
            { Author: Text.Text option
              Title: Text.Text option
              Journal: Text.ShortText option
              Year: int option
              Volume: int option
              Number: int option
              Pages: (int * int) option
              Month: string option
              DataAvailability: DataAvailabilityUnverified }

        and DataAvailabilityUnverified =
            | AtDoiClosed of System.Uri
            | AtDoiOpen of System.Uri
            | NotAttachedToSource

        type Book =
            { BookTitle: Text.Text
              BookSubtitle: Text.Text option
              BookFirstAuthor: Author.Author
              [<Name "More authors">]
              [<Help "Add each non-first author in the order that they appear in the author list.">]
              BookAdditionalAuthors: Author.Author list
              BookCopyrightYear: int
              Editor: Author.Author option
              ISBN: Text.Text option
              ISSNDOI: Text.Text option
              Publisher: Text.Text option }

        type BookChapter =
            { ChapterFirstAuthor: Author.Author
              ChapterAdditionalAuthors: Author.Author list
              ChapterTitle: Text.Text
              [<Name "Number of chapter's first page">]
              FirstPage: int }

        type Database =
            { [<Help "A short identifier - often the one commonly used in literature. For example, EPD (for the European Pollen Database).">]
              Abbreviation: Text.ShortText
              FullName: Text.Text
              [<Help "The full web URL of the online database.">]
              Location: System.Uri }

        and DatabaseDataset =
            { DatabaseAbbreviation: Text.ShortText
              UniqueIdentifierInDatabase: Text.ShortText
              Investigators: Person list
              Title: Text.ShortText option
              WebLocation: System.Uri option }

        type GreyFormat =
            | Blog
            | ConferencePaper
            | ConferenceProceeding
            | Dataset
            | GovernmentDocument
            | GovernmentReport
            | Newsletter
            | Pamphet
            | PolicyStatement
            | Preprint
            | PressRelease
            | ResearchReport
            | StatisticalReport
            | WorkingPaper
            | Other of Text.ShortText

        type GreySource =
            { [<Help "Select an appropriate type of grey literature. If a suitable entry doesn't exist, enter a new one in Other.">]
              Format: GreyFormat
              [<Help "Enter the authors of this source in the order they appear in the author list.">]
              Contributors: Author.Author list
              Title: Text.Text
              [<Help "The institution that either sponsored the work or where the majority of the work was undertaken.">]
              Institution: Text.ShortText option
              License: License
              [<Name "Year Posted">]
              [<Help "The year in which this source was first released.">]
              PostedYear: int option
              [<Name "DOI (Digital Object Identifier)">]
              [<Help "If this article has a DOI, you can specify it either without or with the full URL (e.g. 10.1080/10509585.2015.1092083 or https://doi.org/10.1080/10509585.2015.1092083).">]
              DOI: DigitalObjectIdentifier.DigitalObjectIdentifier option
              [<Help "Briefly describe the contents of the grey literature source that may be of relevance to our project.">]
              Description: Text.Text option }

        type IndividualPublishedDataset =
            { [<Help "Enter the authors of this source in the order they appear in the author list.">]
              Contributors: Author.Author list
              Title: Text.Text
              [<Help "The year in which this dataset was published to a data repository. Note: If the source was placed in a database, use the database entry source type instead.">]
              YearPublished: int option
              [<Help "The institution that either sponsored the work or where the majority of the work was undertaken.">]
              Institution: Text.ShortText option
              [<Name "DOI (Digital Object Identifier)">]
              [<Help "This dataset likely has a DOI. If so, you can specify it either without or with the full URL (e.g. 10.1080/10509585.2015.1092083 or https://doi.org/10.1080/10509585.2015.1092083).">]
              DOI: DigitalObjectIdentifier.DigitalObjectIdentifier option
              License: License }

        type Dissertation =
            { Author: Author.Author
              Title: Text.Text
              Institution: Text.ShortText
              [<Name "Institutional Document ID">]
              [<Help "An internal reference specific to the University, which may be a library index number for example.">]
              InstitutionDocumentID: Text.ShortText option
              CompletionYear: int
              [<Name "DOI (Digital Object Identifier)">]
              [<Help "You can specify a DOI either without or with the full URL (e.g. 10.1080/10509585.2015.1092083 or https://doi.org/10.1080/10509585.2015.1092083).">]
              DOI: DigitalObjectIdentifier.DigitalObjectIdentifier option }

        type DarkData =
            { Investigator: Author.Author
              AdditionalInvestigators: Author.Author list
              [<Name "Year of Compilation">]
              [<Help "The year - if known - in which the dataset was made.">]
              CompilationYear: int option
              Title: Text.ShortText option
              Details: Text.Text
              [<Name "Spatial Context">]
              [<Help "If the data has a specific spatial context, you may specify it here. For example: X Lake, Pyramid Hills, ON, Canada">]
              Context: Geography.SamplingLocation option
              License: License }

        type GreySourceObsolete =
            { Contact: Person
              License: License
              Title: Text.Text }

        type DarkDataObsolete =
            { Contact: Person
              License: License
              Details: Text.Text }


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
        | Excluded of Source * because: ExclusionReason * notes: Text.Text

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
        | InProgress of completedSections: Text.ShortText list
        | Stalled of completedSections: Text.ShortText list * stalledOnSection: Text.ShortText * reason: Text.Text
        | CompletedAll

    type SourceNodeRelation =
        | HasTemporalExtent of SourceNode * Exposure.StudyTimeline.IndividualTimelineNode
        | UsesPrimarySource of SourceNode * SourceNode
        | UsedDatabase of SourceNode * SourceNode
        | HasDataset of SourceNode * SourceNode

    and SourceRelation =
        | HasTemporalExtent
        | UsesPrimarySource
        | UsedDatabase of accessDate: System.DateOnly option * subset: DatabaseSubset
        | HasDataset
        | IsChapterIn

    and DatabaseSubset =
        | AllRecordsInStudyScope
        | SpecificRecords of firstId: Text.ShortText * additionalIds: Text.ShortText list
        | ComplexSubset of methodDescription: Text.Text


    module BibtexParser =

        open System.Text.RegularExpressions

        let articleRegex =
            "@article{(.*),\nauthor = {(.*)},\ntitle = {(.*)},\njournal = {(.*)},\nyear = (.*),\nvolume = {(.*)},\nnumber = {(.*)},\npages = {(.*)--(.*)},\nmonth = {(.*)}}"

        let parse (bibText: string) =
            if Regex.IsMatch(bibText, articleRegex) then
                let matches = Regex.Matches(bibText, articleRegex)

                matches
                |> Seq.cast<Match>
                |> Seq.map (fun m ->
                    Bibliographic
                        { Author = Text.create m.Groups.[2].Value |> Result.toOption
                          Title = Text.create m.Groups.[3].Value |> Result.toOption
                          Journal = Text.createShort m.Groups.[4].Value |> Result.toOption
                          Year = Some <| int m.Groups.[5].Value
                          Volume = Some <| int m.Groups.[6].Value
                          Number = Some <| int m.Groups.[7].Value
                          Pages = Some(int m.Groups.[8].Value, int m.Groups.[9].Value)
                          Month = Some <| m.Groups.[10].Value
                          DataAvailability = RecordTypes.NotAttachedToSource })
                |> Seq.toList
                |> Ok
            else
                Error "No sources identified"

    /// <summary>Access the CrossRef API to match references and easily import them.</summary>
    module CrossRef =

        open FSharp.Data
        open RecordTypes

        type CrossRef = JsonProvider<"crossref-example.json">

        let lookup reference =
            sprintf "https://api.crossref.org/works?query.bibliographic=%s&rows=2&mailto=am2288@cam.ac.uk" reference
            |> CrossRef.Load

        let private intials (s: string) = s.Split(' ') |> Seq.map Seq.head

        let bestMatch (result: CrossRef.Root) =
            if result.Status = "ok" then
                if result.Message.Items.Length = 2 then
                    let isMatch =
                        abs (result.Message.Items.[0].Score - result.Message.Items.[1].Score) > 1.0m

                    if isMatch then
                        printfn "Matched %s: %s" result.Message.Items.[0].Type result.Message.Items.[0].Title.[0]
                        let m = result.Message.Items.[0]

                        match m.Type with
                        | "journal-article" ->
                            Result.result {
                                let! firstAuthor =
                                    let a = m.Author |> Seq.find (fun s -> s.Sequence = "first")

                                    Author.create (
                                        a.Family + ", " + (intials a.Given |> Seq.map string |> String.concat ". ")
                                    )

                                let! additionalAuthors =
                                    m.Author
                                    |> Seq.filter (fun s -> s.Sequence = "additional")
                                    |> Seq.map (fun a ->
                                        Author.create (
                                            a.Family + ", " + (intials a.Given |> Seq.map string |> String.concat ". ")
                                        ))
                                    |> Seq.toList
                                    |> Result.ofList

                                let! title =
                                    if m.Title.Length > 0 then
                                        Text.create m.Title.[0]
                                    else
                                        Error "No title given by CrossRef"

                                let! journal =
                                    if m.ContainerTitle.Length > 0 then
                                        Text.createShort m.ContainerTitle.[0]
                                    else
                                        Error "No journal given by CrossRef"

                                let! year, month =
                                    if m.Published.DateParts.Length > 0 then
                                        if m.Published.DateParts.[0].Length >= 2 then
                                            Ok(m.Published.DateParts.[0].[0], asMonth m.Published.DateParts.[0].[1])
                                        else
                                            Error "No publication date specified"
                                    else
                                        Error "No publication date specified"

                                let volume = Int.tryParse m.Volume
                                let number = Int.tryParse m.Issue

                                let! doi =
                                    if System.String.IsNullOrEmpty m.Doi then
                                        Ok None
                                    else
                                        m.Doi |> DigitalObjectIdentifier.create |> Result.lift Some

                                let j =
                                    { FirstAuthor = firstAuthor
                                      AdditionalAuthors = additionalAuthors
                                      Title = title
                                      Journal = journal
                                      Year = year
                                      Volume = volume
                                      Number = number
                                      PageRange = None
                                      Month = month
                                      DOI = doi }

                                return (j |> JournalArticle |> PublishedSource |> Some)
                            }

                        | _ -> failwith result.Message.Items.[0].Type
                    else
                        Ok None
                else
                    Ok None
            else
                Error "Could not successfully query CrossRef, either due to no connection or a bad request."

        /// <summary>Attempts to match a reference to a record in CrossRef using the CrossRef API.</summary>
        /// <param name="query">A search query for the 'bibliographic' field of the CrossRef API.</param>
        /// <returns>A result indicating a successful match (as a BiodiversityCoder node) or mismatch.</returns>
        let tryMatch query =
            try
                query |> lookup |> bestMatch
            with e ->
                Error e.Message

    module ColandrParser =

        open FSharp.Data
        open System.IO

        type Colandr = CsvProvider<"colandr-output.csv">

        let private tryToInt (s: string) =
            match System.Int32.TryParse s with
            | true, v -> Some v
            | false, _ -> None

        let syncColandr file =
            if File.Exists(file) then
                let colandr = Colandr.Load file

                colandr.Rows
                |> Seq.where (fun row -> row.Citation_screening_status = "included")
                |> Seq.map (fun row ->
                    Bibliographic
                        { Author = Text.create row.Citation_authors |> Result.toOption
                          Title = Text.create row.Citation_title |> Result.toOption
                          Journal = Text.createShort row.Citation_journal_name |> Result.toOption
                          Year = tryToInt row.Citation_pub_year
                          Volume =
                            if row.Citation_journal_volume.HasValue then
                                Some(row.Citation_journal_volume.Value)
                            else
                                None
                          Number = None
                          Pages = None
                          Month = None
                          DataAvailability = RecordTypes.NotAttachedToSource })
                |> Ok
            else
                Error "Colandr file does not exist"
