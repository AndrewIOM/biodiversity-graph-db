namespace BiodiversityCoder.Core.Views

open BiodiversityCoder.Core
open Bolero
open Bolero.Html
open Bolero.Builders

type MessageType =
    | ErrorMessage
    | SuccessMessage
    | InfoMessage


/// Abstract representations of parts of a view, like alerts, cards etc.
module ViewParts =

    let _class = attr.``class``

    module Grid =

        let row (node: Node) =
            div {
                _class "row"
                node
            }

        let column n (node: Node) =
            div {
                _class (sprintf "col-md-%i" n)
                node
            }

    type AlertLevel =
        | Danger
        | Success
        | Info
        | Warning

    let alert level msg =
        div {
            match level with
            | Danger -> _class "alert alert-danger"
            | Success -> _class "alert alert-success"
            | Info -> _class "alert alert-info"
            | Warning -> _class "alert alert-warning"

            text msg
        }

    let alertMessage' level msg dismissAction =
        concat {
            alert level msg

            button {
                _class "btn"
                on.click (fun _ -> dismissAction)
                text "Dismiss"
            }
        }

    let alertMessage message dismissAction =
        cond message
        <| function
            | Some msg ->
                cond (fst msg)
                <| function
                    | ErrorMessage -> alertMessage' Danger (snd msg) dismissAction
                    | SuccessMessage -> alertMessage' Success (snd msg) dismissAction
                    | InfoMessage -> alertMessage' Info (snd msg) dismissAction
            | None -> empty ()

    let fieldRow labelText (field: Node) =
        div {
            _class "row g-3"

            div {
                _class "col-md-3"
                label { text labelText }
            }

            div {
                _class "col-md-9"
                field
            }
        }

    let fieldRowTriple labelText (fieldMiddle: Node) (fieldRight: Node) =
        div {
            _class "row g-3"

            div {
                _class "col-md-3"
                label { text labelText }
            }

            div {
                _class "col-md-6"
                fieldMiddle
            }

            div {
                _class "col-md-3"
                fieldRight
            }
        }

    let formLabel txt =
        label {
            _class "form-label"
            text txt
        }

    let formText txt =
        small {
            _class "form-text"
            text txt
        }

    type CardLevel =
        | Primary
        | Secondary

    let cardClass =
        function
        | Primary -> "text-bg-secondary"
        | Secondary -> ""

    // Card
    let card title level (body: Node) =
        div {
            _class (sprintf "card mb-4 %s" (cardClass level))

            cond title
            <| function
                | Some txt ->
                    div {
                        _class (sprintf "card-header %s" (cardClass level))
                        text txt
                    }
                | None -> empty ()

            div {
                _class "card-body"
                body
            }
        }

    let infoCard title (body: Node) =
        div {
            _class "card border-info mb-4 mt-4"

            div {
                _class "card-header text-dark bg-info"
                text title
            }

            div {
                _class "card-body"
                body
            }
        }


/// Views to display individual atoms by their type.
module AtomView =

    open ViewParts

    let timeline timelineAtom g =
        ViewParts.card None ViewParts.CardLevel.Secondary
        <| concat {
            cond (
                Storage.tryFindRelationFriendlyName<Exposure.ExposureRelation>
                    Exposure.ExposureRelation.ExtentEarliest
                    g
                    timelineAtom
            )
            <| function
                | Some early ->
                    cond (
                        Storage.tryFindRelationFriendlyName<Exposure.ExposureRelation>
                            Exposure.ExposureRelation.ExtentLatest
                            g
                            timelineAtom
                    )
                    <| function
                        | Some late ->
                            h5 {
                                _class "card-title"
                                textf "%s - %s timeline" early late
                            }
                        | None -> empty ()
                | None -> empty ()

            cond ((timelineAtom |> fst |> snd) |> GraphStructure.Nodes.asExposureNode)
            <| function
                | Some exposureNode ->
                    cond exposureNode
                    <| function
                        | Exposure.ExposureNode.TimelineNode timeline ->
                            cond timeline
                            <| function
                                | Exposure.StudyTimeline.Continuous res ->
                                    cond res
                                    <| function
                                        | Exposure.StudyTimeline.Regular(r, g) ->
                                            h6 {
                                                _class "card-subtitle mb-2 text-muted"

                                                textf
                                                    "A continuous timeline with %A year timesteps (formed using %A)"
                                                    r
                                                    g
                                            }
                                        | Exposure.StudyTimeline.Irregular ->
                                            h6 {
                                                _class "card-subtitle mb-2 text-muted"
                                                textf "A continuous timeline with irregular timesteps"
                                            }
                                | Exposure.StudyTimeline.Discontinuous(res, _) ->
                                    cond res
                                    <| function
                                        | Exposure.StudyTimeline.Regular(r, g) ->
                                            h6 {
                                                _class "card-subtitle mb-2 text-muted"

                                                textf
                                                    "A continuous timeline with %A year timesteps (formed using %A)"
                                                    r
                                                    g
                                            }
                                        | Exposure.StudyTimeline.Irregular ->
                                            h6 {
                                                _class "card-subtitle mb-2 text-muted"
                                                textf "A discontinuous timeline with irregular timesteps"
                                            }
                        | _ -> empty ()
                | _ -> empty ()

            cond (
                Storage.tryFindRelationNode<Exposure.ExposureRelation>
                    Exposure.ExposureRelation.IsLocatedAt
                    g
                    timelineAtom
            )
            <| function
                | Some context ->
                    p {
                        cond (context |> fst |> snd)
                        <| function
                            | GraphStructure.Node.PopulationNode p ->
                                cond p
                                <| function
                                    | GraphStructure.ContextNode c ->
                                        concat {
                                            textf "%s. " c.Name.Value

                                            cond c.SamplingLocation
                                            <| function
                                                | FieldDataTypes.Geography.Site(lat, lon) ->
                                                    textf "Occurs at the point %A DD, %A, DD" lat lon
                                                | FieldDataTypes.Geography.Area poly ->
                                                    textf "Occurs in an area: %s" <| poly.ToString()
                                                | FieldDataTypes.Geography.Locality(l, d, r, c) ->
                                                    textf
                                                        "Occurs at the locality %s (%s, %s, %s)"
                                                        l.Value
                                                        d.Value
                                                        r.Value
                                                        c.Value
                                                | FieldDataTypes.Geography.Region(r, c) ->
                                                    textf "Occurs in the region %s (%s)" r.Value c.Value
                                                | FieldDataTypes.Geography.Country(c) ->
                                                    textf "Occurs in the country %s" c.Value
                                                | _ -> textf "Occurs at %s." c.Name.Value
                                        }
                                    | _ -> empty ()
                            | _ -> empty ()
                    }
                | None -> empty ()

            small { text "The time-series is constructed from the following individual dates:" }

            ul {
                forEach (
                    Storage.tryFindRelationNodes<Exposure.ExposureRelation>
                        Exposure.ExposureRelation.ConstructedWithDate
                        g
                        timelineAtom
                )
                <| fun dateNode ->
                    cond (dateNode |> fst |> snd)
                    <| function
                        | GraphStructure.Node.ExposureNode p ->
                            cond p
                            <| function
                                | Exposure.ExposureNode.DateNode d ->
                                    li {
                                        textf
                                            "Esimated date from %A (depth %A). Method was %A."
                                            d.MaterialDated.Value
                                            d.SampleDepth
                                            d.Date
                                    }
                                | _ -> empty ()
                        | _ -> empty ()
            }
        }
