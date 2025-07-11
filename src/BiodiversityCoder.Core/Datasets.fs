namespace BiodiversityCoder.Core

open FieldDataTypes

/// <summary>Nodes for handling datasets linked to the
/// metadata database.</summary>
module Datasets =

    module DataTable =

        open Newtonsoft.Json

        type IndexUnit =
            | Depths
            | Ages of AgeUnit

        and AgeUnit =
            | BP
            | CalYrBP
            | HistoryYearAD
            | HistoryYearBC

        type DataTable2D = {
            Index: float list
            IndexUnit: IndexUnit
            Morphotypes: string list
            Data: float[,]
        }

        [<JsonObject(MemberSerialization = MemberSerialization.Fields)>]
        type DataTable = private DataTable of DataTable2D

        let unwrap (DataTable table) = table

        let indexColumnType (str:string) =
            match str.ToLower() with
            | "depth" -> Depths |> Some
            | "year before present"
            | "bp" -> Ages BP |> Some
            | "calendar year before present"
            | "cal yr bp" -> Ages CalYrBP |> Some
            | "ad" -> Ages HistoryYearAD |> Some
            | "bc" -> Ages HistoryYearBC |> Some
            | _ -> None

        /// Parse a tab-delimited string to a data table
        let createDataTable (str:string) =
            let rows = str.Split('\n') |> Array.filter (System.String.IsNullOrEmpty >> not)
            if rows.Length = 0 then Error "The dataset was empty"
            else
                let rowLengths = rows |> Array.map(fun r -> r.Split("\t") |> Seq.length)
                if rowLengths |> Seq.distinct |> Seq.length > 1 || rowLengths |> Seq.contains 0 || rowLengths |> Seq.contains 1
                then Error "Each row must have at least two columns, and all rows must have the same number of columns"
                else
                    let headRow = (rows |> Seq.head).Split('\t')
                    let indexCol = headRow.[0]
                    let morphotypeCols = headRow |> Array.skip 1

                    let indexType = indexColumnType indexCol

                    let depths = rows |> Array.choose(fun r -> r.Split("\t").[0] |> Float.tryParse) |> Array.toList

                    let data =
                        rows
                        |> Array.skip 1
                        |> Array.map(fun x ->
                            x.Split('\t')
                            |> Array.skip 1
                            |> Array.choose Float.tryParse )

                    let dataRowLengths = data |> Array.map Array.length |> Array.distinct
                    if dataRowLengths.Length <> 1
                    then Error "Data in each row was of different lengths. Check data structure."
                    else
                        if
                            indexType.IsNone ||
                            morphotypeCols.Length <> dataRowLengths.[0]
                        then Error "Problem with data format. Check first column is named 'depth' or an age unit, and each taxon column is complete."
                        else
                            {
                                IndexUnit = indexType.Value
                                Index = depths
                                Morphotypes = morphotypeCols |> Array.toList
                                Data = array2D data
                            } |> DataTable |> Ok

        let isValid table =
            let table = unwrap table
            table.Data.GetLength(0) = table.Index.Length &&
                table.Data.GetLength(1) = table.Morphotypes.Length

        let depths table =
            (unwrap table).Index
            |> List.mapi(fun i d -> d, (unwrap table).Data.[i,*] )
            |> Map.ofList

        type DataTable with 
            static member TryCreate s = 
                match s with 
                | Text s -> createDataTable s
                | _ -> Error "Data not in a valid format to make a data table."
                |> Result.toOption

            member this.IsValid = isValid this
            member this.Depths () = (unwrap this).IndexUnit, depths this
            member this.Morphotypes () = (unwrap this).Morphotypes


    type SourceDataLocation =
        | FromFigure of figureNumber:int
        | FromTable of tableNumber: int
        | FromSupplementaryFigure of supplementaryFigureNumber: Text.ShortText
        | FromSupplementaryTable of supplementaryTableNumber: Text.ShortText

    with
        member this.Name =
            match this with
            | FromFigure n -> sprintf "Figure %i" n
            | FromTable n -> sprintf "Table %i" n
            | FromSupplementaryFigure n -> sprintf "Suppl Figure %s" n.Value
            | FromSupplementaryTable n -> sprintf "Suppl Table %s" n.Value

    type SoftwareUsed =
        | PlotDigitizer
        | OtherSoftware of Text.ShortText

    type Metric =
        | Abundance
        | Presence
        | Concentration
        | OtherMetric of Text.Text

    type MetricUnit =
        | Count
        | CountPerCmCubed of cm3:int
        | PercentAbundance
        | OtherUnit of Text.Text

    type DigitisedDataset = {
        [<Name("What was digitised?")>]
        [<Help("Enter a reference to a table or figure that was digitised.")>]
        WhatWasDigitised: SourceDataLocation
        [<Name("Which software was used?")>]
        SoftwareUsed: SoftwareUsed
        [<Name("Data metric / measure")>]
        [<Help("What is the metric / measure that has been digitised. e.g. percentage, count, presence.")>]
        Metric: Metric
        [<Name("Data unit")>]
        [<Help("What is the unit? e.g. number of grains, grains per 100cm^3.")>]
        Units: MetricUnit
        [<Name("Who digitised this?")>]
        [<Help("The name of the digitiser, in the format Smith, A.D.")>]
        DigitisedBy: Author.Author
        [<Name("Date of digitisation")>]
        [<Help("When was the source digitised? Use the format 2024-10-24")>]
        DigitisedOn: Time.SimpleDateOnly
        [<Name("Data table (raw data)")>]
        [<Help("Paste a tab-delimited dataset into this box. The data must be in wide format, with the first column named 'depth' (or an appropriate age unit if age-indexed) and subsequent columns named as the morphotypes in the dataset. If a sample band (e.g. Xcm - Ycm depth), depths should refer to the upper depth of the band. The first column should contain depth values in centimetres, and subsequent columns numerical values of the metric in the units as previously specified.")>]
        DataTable: DataTable.DataTable
        [<Name("Data table (additional dimensions)")>]
        [<Help("Additional indexed properties of the data that are not morphotypes (e.g. sample volume, sample width, pollen sum for % data). Paste a tab-delimited dataset into this box. The data must be in wide format, with the first column named 'depth' (or an appropriate age unit if age-indexed) and subsequent columns named as the morphotypes in the dataset. The first column should contain depth values in centimetres, and subsequent columns numerical values of the metric in the units as previously specified.")>]
        Dimensions: DataTable.DataTable option
    }

    type DatasetNode =
        | Digitised of DigitisedDataset

    type DatasetNodeRelation =
        | IsProxyGroup of DigitisedDataset * Population.BioticProxies.BioticProxyCategoryNode

    type DatasetRelation =
        | IsProxyGroup
