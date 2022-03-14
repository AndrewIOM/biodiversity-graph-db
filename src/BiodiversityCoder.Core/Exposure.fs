namespace BiodiversityCoder.Core

module Exposure =

    /// Temporal dimensions of a study or dataset.
    module StudyTimeline =
    
        /// Represents 
        type IndividualDateNode = float
    
        type IndividualTimelineNode = float

    /// The temporal index handles relations between calendar-based / qualitative-based
    /// dates in the past and occurrence data.
    module TemporalIndex =
            
        /// Based on 1950 as year zero
        [<Measure>]
        type calYearBP
        
        type CalYearNode = private CalYear of int<calYearBP>

        let private unwrap (CalYear c) = c
        type CalYearNode with
            member this.Year = unwrap this

        let createTimeNode yearBeforePresent =
            if yearBeforePresent >= 1950 - System.DateTime.Now.Year
                && yearBeforePresent <= 14000
            then yearBeforePresent * 1<calYearBP> |> CalYear |> Ok
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
        // from individual temporal extent?
        | ExtentEarliest            of IndividualTimelineNode * CalYearNode
        | ExtentEarliestUncertainty of IndividualTimelineNode * CalYearNode
        | ExtentLatest              of IndividualTimelineNode * CalYearNode
        | ExtentLatestUncertainty   of IndividualTimelineNode * CalYearNode
        | IntersectsTime            of IndividualTimelineNode * QualitativeLabelNode
        // from individual temporal extent
        | HasProxyInfo              of IndividualTimelineNode * Population.ProxiedTaxon.ProxiedTaxonHyperEdge
        | HasOrphanProxy            of IndividualTimelineNode * Population.BioticProxies.BioticProxyNode

    /// A relationship originating from an exposure node
    and ExposureRelation =
        | Next
        | Contains
        | EarliestTime
        | LatestTime                
        | TimeEstimate
        | OccursWithin
        | UncertaintyOldest
        | UncertaintyYoungest
        | ExtentEarliest
        | ExtentEarliestUncertainty
        | ExtentLatest
        | ExtentLatestUncertainty
        | IntersectsTime
