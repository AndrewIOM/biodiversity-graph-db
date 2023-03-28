namespace BiodiversityCoder.Core

open System
open Newtonsoft.Json

/// A result computation expression. 
/// Source: http://www.fssnip.net/7UJ/title/ResultBuilder-Computational-Expression
type ResultBuilder() =
    let ofOption error = function Some s -> Ok s | None -> Error error
    member __.Return(x) = Ok x
    member __.ReturnFrom(m: Result<_, _>) = m
    member __.Bind(m, f) = Result.bind f m
    member __.Bind((m, error): (Option<'T> * 'E), f) = m |> ofOption error |> Result.bind f
    member __.Zero() = None
    member __.Combine(m, f) = Result.bind f m
    member __.Delay(f: unit -> _) = f
    member __.Run(f) = f()
    member __.TryWith(m, h) =
        try __.ReturnFrom(m)
        with e -> h e
    member __.TryFinally(m, compensation) =
        try __.ReturnFrom(m)
        finally compensation()
    member __.Using(res:#IDisposable, body) =
        __.TryFinally(body res, fun () -> match res with null -> () | disp -> disp.Dispose())
    member __.While(guard, f) =
        if not (guard()) then Ok () else
        do f() |> ignore
        __.While(guard, f)
    member __.For(sequence:seq<_>, body) =
        __.Using(sequence.GetEnumerator(), fun enum -> __.While(enum.MoveNext, __.Delay(fun () -> body enum.Current)))

[<AutoOpen>]
module Result =

    let result = new ResultBuilder()

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

    let toOption result =
        match result with
        | Ok r -> Some r
        | Error _ -> None

    let ofList arg = 
        (arg, Ok[]) ||> List.foldBack (fun t s ->
        match t, s with
        | Ok x, Ok xs -> Ok(x::xs)
        | Error e, Ok _ -> Error e
        | Ok _, Error es -> Error es
        | Error e, Error es -> Error es )

    let ofOption onError result =
        match result with
        | Some r -> Ok r
        | None -> Error onError

    let lower fOk fErr r =
        match r with
        | Ok o -> fOk o
        | Error e -> fErr e
    
    let forceOk r =
        match r with
        | Ok o -> o
        | Error e -> failwith e

type SimpleValue =
    | Number of float
    | Text of string
    | Date of DateOnly
    | Time of TimeOnly
    | Boolean of bool

module Parse =

    let tryParseWith (tryParseFunc: string -> bool * _) = tryParseFunc >> function
        | true, v    -> Ok v
        | false, _   -> Error "Parse failed"

    let parseDouble = tryParseWith System.Double.TryParse


[<AutoOpen>]
module Attributes =

    // Allows setting properties on record fields.
    [<AttributeUsage(AttributeTargets.Property)>]
    type NameAttribute(x: string) =
        inherit Attribute()
        member _.value = x

    /// An International Resource Identifier
    [<AttributeUsage(AttributeTargets.Property)>]
    type IRIAttribute(x: Uri) =
        inherit Attribute()
        member _.Uri = x

    /// An attribute to display guidance text when specifying data for a 
    /// property or field.
    type HelpAttribute(text: string) =
        inherit Attribute()
        with member __.Text = text


// Services shared with MAUI
type IFolderPicker =
    abstract member PickFolder : unit -> System.Threading.Tasks.Task<string>

/// Contains types that represent data fields within
/// the graph and coding interface.
module FieldDataTypes =

    /// Text field that is between 1 and 100 characters long.
    [<RequireQualifiedAccess>]
    module Text =

        [<JsonObject(MemberSerialization = MemberSerialization.Fields)>]
        type ShortText = private ShortText of string
        [<JsonObject(MemberSerialization = MemberSerialization.Fields)>]
        type Text = private Text of string
        type ShortText with member this.Value =  this |> (fun (ShortText t) -> t)
        type Text with member this.Value =  this |> (fun (Text t) -> t)

        let createShort txt =
            if not (String.IsNullOrEmpty txt)
            then 
                if txt.Length > 100 then Error "Short text must be less than 100 characters"
                else Ok (ShortText txt)
            else Error "Short text must not be empty"

        let create txt =
            if not (String.IsNullOrEmpty txt)
            then Ok (Text txt)
            else Error "Short text must not be empty"

        type ShortText with static member TryCreate s = match s with | SimpleValue.Text s -> createShort s |> Result.toOption | _ -> None
        type Text with static member TryCreate s = match s with | SimpleValue.Text s -> create s |> Result.toOption | _ -> None

    /// Representation of cultures (e.g. for vernacular names)
    [<RequireQualifiedAccess>]
    module LanguageCode =

        open System.Globalization

        [<JsonObject(MemberSerialization = MemberSerialization.Fields)>]
        type LanguageCode = private LanguageCode of string

        let unwrap (LanguageCode c) = c

        type LanguageCode
            with member this.Value = unwrap this

        let create (culture:CultureInfo) =
            culture.TwoLetterISOLanguageName |> LanguageCode

    [<RequireQualifiedAccess>]
    module Geography =

        /// Decimal degrees
        [<Measure>]
        type DD

        [<JsonObject(MemberSerialization = MemberSerialization.Fields)>]
        type Latitude = private Latitude of float<DD>
        [<JsonObject(MemberSerialization = MemberSerialization.Fields)>]
        type Longitude = private Longitude of float<DD>
        [<JsonObject(MemberSerialization = MemberSerialization.Fields)>]
        type Polygon = private Polygon of (Latitude * Longitude) list

        let createLatitude lat =
            if lat >= -89.9 && lat <= 89.9 then lat * 1.<DD> |> Latitude |> Ok
            else Error "Latitude must be between -90 and 90 degrees"

        let createLongitude lon =
            if lon >= -180. && lon <= 180. then lon * 1.<DD> |> Longitude |> Ok
            else Error "Longitude must be between -180 and 180 degrees"

        let createPolygon points =
            match points |> Seq.length with
            | p when p < 2 -> Error "Polygons must have at least three points"
            | _ -> points |> Polygon |> Ok 

        type SamplingLocation =
            | Site      of latitude:Latitude * longitude:Longitude
            | Area      of polygon:Polygon
            | Locality  of locality:Text.ShortText * district:Text.ShortText * region:Text.ShortText * country:Text.ShortText
            | District  of district:Text.ShortText * region:Text.ShortText * country:Text.ShortText
            | Region    of region:Text.ShortText * country:Text.ShortText
            | Country   of country:Text.ShortText
            | Arctic

        let private unwrapLat (Latitude l) = l
        let private unwrapLon (Longitude l) = l
        let private unwrapPoly (Polygon l) = l

        type Latitude with 
            static member TryCreate s = 
                match s with 
                | Number s -> createLatitude s
                | Text s -> s |> Parse.parseDouble |> Result.bind createLatitude
                | _ -> Error "Not a valid latitude"
                |> Result.toOption
            member this.Value = unwrapLat this

        type Longitude with 
            static member TryCreate s = 
                match s with 
                | Number s -> createLongitude s
                | Text s -> s |> Parse.parseDouble |> Result.bind createLongitude
                | _ -> Error "Not a valid longitude"
                |> Result.toOption
            member this.Value = unwrapLon this

        type Polygon with
            static member TryCreate s =
                match s with
                | Text s -> 
                    let m = System.Text.RegularExpressions.Regex.Match(s, "^POLYGON[ ]?\(\((.*)\)\)")
                    if m.Success
                    then
                        let coords = m.Groups.[1].Value.Split(",") |> Array.map(fun s -> s.Split(" "))
                        if Array.TrueForAll(coords, fun f -> f.Length = 2)
                        then
                            coords
                            |> Array.map(fun c ->
                                match Parse.parseDouble c.[0] |> Result.bind(fun l -> createLongitude l) with
                                | Ok lon ->
                                    match Parse.parseDouble c.[1] |> Result.bind(fun l -> createLatitude l) with
                                    | Ok lat -> Ok (lat, lon)
                                    | Error e -> Error e
                                | Error e -> Error e
                                )
                            |> Array.toList
                            |> Result.ofList
                            |> Result.bind(fun l ->
                                if l.Length < 3
                                then Error "Cannot have less than three points for a polygon"
                                else Ok <| Polygon l )
                        else Error "Coordinates must be defined in format 'lon lat, lon lat, lon lat'"
                    else Error "You must enter a polygon as a WKT format string, starting with POLYGON."
                | _ -> Error "You must enter a polygon as a WKT format string, starting with POLYGON."
                |> Result.toOption
            member this.Value = unwrapPoly this

    [<RequireQualifiedAccess>]
    module StratigraphicSequence =

        [<Measure>] type cm

        [<JsonObject(MemberSerialization = MemberSerialization.Fields)>]
        type Depth = private Depth of depth:float<cm>

        let createDepth i =
            if i > 0. then Ok (Depth (i * 1.<cm>)) else Error "Depth cannot be negative"

        type Depth with 
            static member TryCreate s = 
                match s with 
                | Number s -> createDepth s
                | Text s -> s |> Parse.parseDouble |> Result.bind createDepth
                | _ -> Error "Not a valid depth"
                |> Result.toOption

        type DepthInCore = 
            | DepthBand of lower:Depth * upper:Depth
            | DepthPoint of depth:Depth
            | DepthNotStated


    [<RequireQualifiedAccess>]
    module OldDate =

        /// Calibrated dates where 1950 is year zero
        [<Measure>] type calYearBP

        /// Uncalibrated radiocarbon dates are reported in
        /// uncalibrated years before present (BP)
        [<Measure>] type uncalYearBP

        /// A calendar date
        [<Measure>] type AD
        [<Measure>] type BC

        type MeasurementError =
            | NoDatingErrorSpecified
            | DatingErrorPlusMinus of measurementError:float<calYearBP>

        type OldDatingMethod =
            | RadiocarbonUncalibrated of uncalibratedDate:float<uncalYearBP>
            | RadiocarbonCalibrated of calibratedDate:float<calYearBP> * calibrationCurve:Text.ShortText * uncalibratedDate:float<uncalYearBP> option
            | Tephra of tephraName:Text.ShortText * date:OldDate
            | HistoricEvent of eventName:Text.ShortText * date:OldDate
            | Lead210 of concentration:float * date:OldDate
            | Radiocaesium of concentration:float * date:OldDate
            | CollectionDate of yearCollected:float<AD>
            // | DepositionalZone of zoneName:Text.ShortText

        and OldDate =
            | BP of bpDate:float<uncalYearBP>
            | CalYrBP of calibratedDate:CalibratedRadiocarbonDate
            | HistoryYearAD of calendarYear:float<AD>
            | HistoryYearBC of calendarYear:float<BC>

        and CalibratedRadiocarbonDate = {
            [<Name("Calibrated date")>]
            [<Help("Enter the date in units 'cal yr BP'. If the source uses BCE or similar, convert the date manually to 'cal yr BP' for this field.")>]
            CalibratedDate: float<calYearBP>
            [<Name("Calibration curve or method")>]
            [<Help("If a calibration curve was used (e.g. IntCal98), enter its name here. Alternatively, older papers may reference specific sources for a calibration method (e.g. Clark 1975). Enter the author and year here in this format: A, B and X 1986.")>]
            CalibrationCurve: Text.ShortText
            [<Name("Uncalibrated date")>]
            [<Help("Does the source include the raw (uncalibrated) radiocarbon date? If so, enter it here as a BP date. If the uncalibrated date is given as a BCE date or similar, manually convert the date to 'BP'.")>]
            UncalibratedDate: float<uncalYearBP> option
            [<Name("Uncalibrated date: measurement error")>]
            [<Help("If there is an error bound (+/-) around the uncalibrated date, specify this here.")>]
            UncalibratedDateError: MeasurementError
        }

        /// This simple date representation is used on the relations between
        /// a date node and the cal yr BP time series. This helps us keep track
        /// of how each temporal extent relates to the harmonised (calibrated years)
        /// master time series.
        and OldDateSimple =
            | BP of bpDate:float<uncalYearBP>
            | CalYrBP of calibratedDate:float<calYearBP> * calibrationTechnique:Text.ShortText option
            | HistoryYearAD of calendarYear:float<AD>
            | HistoryYearBC of calendarYear:float<BC>


    type Month = Jan | Feb | Mar | Apr | May | Jun | Jul | Aug | Sep | Oct | Nov | Dec
    type Person = { FirstName: Text.ShortText; LastName: Text.ShortText }
    type License =
        | ``Public Domain Mark``
        | ``Creative Commons Public Domain Dedication``
        | ``Open Data Commons Public Domain Dedication and License``
        | ``Creative Commons Attribution 4 International``
        | ``Community Data License Agreement - Permissive``
        | ``Open Data Commons Attribution License``
        | ``Creative Commons Attribution-ShareAlike 4 International``
        | ``Community Data License Agreement – Sharing Version 1``
        | ``Open Data Commons Open Database License``
        | ``Creative Commons Attribution-NonCommercial 4 International``
        | ``Creative Commons Attribution-NoDerivatives 4 International``
        | ``Creative Commons Attribution-NonCommercial-ShareAlike 4 International``
        | ``Creative Commons Attribution-NonCommercial-NoDerivatives 4 International``
        | ``No license specified``
        | Other of Text.ShortText