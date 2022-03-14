namespace BiodiversityCoder.Core

module Exposure =

    open FieldDataTypes

    /// Temporal dimensions of a study or dataset.
    module StudyTimeline =
    
        /// Represents 
        type IndividualDateNode = {
            TimeEstimate: float<OldDate.calYearBP>
        }
    
        type IndividualTimelineNode = float

    /// The temporal index handles relations between calendar-based / qualitative-based
    /// dates in the past and occurrence data.
    module TemporalIndex =
                
        type CalYearNode = private CalYear of int<OldDate.calYearBP>

        let private unwrap (CalYear c) = c
        type CalYearNode with
            member this.Year = unwrap this

        let createTimeNode yearBeforePresent =
            if yearBeforePresent >= 1950 - System.DateTime.Now.Year
                && yearBeforePresent <= 14000
            then yearBeforePresent * 1<OldDate.calYearBP> |> CalYear |> Ok
            else Error <| sprintf "The year %i is not a valid Holocene time" yearBeforePresent

        type QualitativeLabelNode = {
            Name: string
        }
    
    /// The master node type for 'exposure' peco element.
    type ExposureNode =
        | YearNode of TemporalIndex.CalYearNode
        | SliceLabelNode of TemporalIndex.QualitativeLabelNode

    open TemporalIndex
    open StudyTimeline

    /// Relations that go from the exposure elements as the source
    type ExposureNodeRelation =
        // from year nodes:
        | Next                      of CalYearNode * CalYearNode
        // from qualitative time period nodes:
        | Contains                  of QualitativeLabelNode * CalYearNode
        | EarliestTime              of QualitativeLabelNode * CalYearNode
        | LatestTime                of QualitativeLabelNode * CalYearNode
        // from individual dates (radiocarbon etc?)
        | TimeEstimate              of IndividualDateNode * CalYearNode
        | OccursWithin              of IndividualDateNode * QualitativeLabelNode
        | UncertaintyOldest         of IndividualDateNode * CalYearNode
        | UncertaintyYoungest       of IndividualDateNode * CalYearNode
        // from individual temporal extent:
        | ExtentEarliest            of IndividualTimelineNode * CalYearNode
        | ExtentEarliestUncertainty of IndividualTimelineNode * CalYearNode
        | ExtentLatest              of IndividualTimelineNode * CalYearNode
        | ExtentLatestUncertainty   of IndividualTimelineNode * CalYearNode
        | IntersectsTime            of IndividualTimelineNode * QualitativeLabelNode
        // from individual temporal extent:
        | HasProxyInfo              of IndividualTimelineNode * Population.ProxiedTaxon.ProxiedTaxonHyperEdge
        | HasOrphanProxy            of IndividualTimelineNode * Population.BioticProxies.BioticProxyNode
        // from individual temporal extent:
        | IsLocatedAt               of IndividualTimelineNode * Population.Context.ContextNode

    /// A relationship originating from an exposure node
    and ExposureRelation =
        | Next
        | Contains
        | EarliestTime
        | LatestTime
        | TimeEstimate                  of float
        | OccursWithin                  of float
        | UncertaintyOldest             of float
        | UncertaintyYoungest           of float
        | ExtentEarliest
        | ExtentEarliestUncertainty
        | ExtentLatest
        | ExtentLatestUncertainty
        | IntersectsTime
        | HasProxyInfo
        | HasOrphanProxy
        | IsLocatedAt