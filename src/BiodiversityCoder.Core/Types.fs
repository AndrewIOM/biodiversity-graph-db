namespace BiodiversityCoder.Core

open System

/// Contains types that represent data fields within
/// the graph and coding interface.
module FieldDataTypes =

    /// Text field that is between 1 and 100 characters long.
    [<RequireQualifiedAccess>]
    module Text =

        type ShortText = private ShortText of string
        type Text = private Text of string

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

    /// Representation of cultures (e.g. for vernacular names)
    [<RequireQualifiedAccess>]
    module LanguageCode =

        open System.Globalization

        type LanguageCode = private LanguageCode of string

        let create (culture:CultureInfo) =
            culture.TwoLetterISOLanguageName |> LanguageCode

    [<RequireQualifiedAccess>]
    module Geography =

        /// Decimal degrees
        [<Measure>]
        type DD

        type Latitude = private Latitude of float<DD>
        type Longitude = private Longitude of float<DD>
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
            | Site      of Latitude * Longitude
            | Area      of Polygon
            | Locality  of locality:Text.ShortText * district:Text.ShortText * region:Text.ShortText * country:Text.ShortText
            | District  of district:Text.ShortText * region:Text.ShortText * country:Text.ShortText
            | Region    of region:Text.ShortText * country:Text.ShortText
            | Country   of country:Text.ShortText
            | Arctic

    [<RequireQualifiedAccess>]
    module StratigraphicSequence =

        [<Measure>] type cm

        type Depth = private Depth of float<cm>

        let createDepth i =
            if i > 0. then Ok (Depth (i * 1.<cm>)) else Error "Depth cannot be negative"


    [<RequireQualifiedAccess>]
    module OldDate =

        /// Calibrated dates where 1950 is year zero
        [<Measure>] type calYearBP

        /// Uncalibrated radiocarbon dates are reported in
        /// uncalibrated years before present 
        [<Measure>] type uncalYearBP

        /// A calendar date
        [<Measure>] type AD
        [<Measure>] type BC

        type OldDate =
        | CollectionDate of float<AD>
        | RadiocarbonCalibrated of float<calYearBP> * calibrationCurve:Text.ShortText
        | RadiocarbonUncalibrated of float<uncalYearBP>
        | Lead210 of float<calYearBP>

    type Month = Jan | Feb | Mar | Apr | May | Jun | Jul | Aug | Sep | Oct | Nov | Dec
    type Person = { FirstName: Text.ShortText; LastName: Text.ShortText }
    type License = License //?

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

module List =

    let retn = Ok
    let rec mapResult f list =
        let cons head tail = head :: tail
        match list with
        | [] -> 
            retn []
        | head::tail ->
            retn cons <*> (f head) <*> (mapResult f tail)
