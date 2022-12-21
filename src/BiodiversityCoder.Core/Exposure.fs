namespace BiodiversityCoder.Core

open Newtonsoft.Json

module Exposure =

    open FieldDataTypes

    /// Temporal dimensions of a study or dataset.
    module StudyTimeline =
    
        /// An individual date made for a point in a discontinuous
        /// time-series, for example a stratigraphic sequence.
        type IndividualDateNode = {
            [<Name("Dating method")>]
            [<Help("Specify the dating method and associated raw date information. For radiocarbon, enter the raw uncalibrated date; if this is unavailable, enter the calibrated date and calibration curve used.")>]
            Date: OldDate.OldDatingMethod
            [<Name("Dating measurement error")>]
            [<Help("If there is an error bound (+/-) around the date, specify this here.")>]
            MeasurementError: OldDate.MeasurementError
            [<Name("Material that was dated")>]
            [<Help("Example: leaves; ostracod shells. Note: this is a free field, but be as succinct as possible.")>]
            MaterialDated: Text.ShortText
            [<Name("Sample depth")>]
            [<Help("If a sediment core, select 'Some' to enter the depth in centimetres (cm) at which this date was collected (either a depth range or single depth). Otherwise, select 'None'.")>]
            SampleDepth: StratigraphicSequence.DepthInCore option
            [<Name("Was this date discarded?")>]
            [<Help("Select 'No' unless the date was provided but not used in the formation of an age-depth model (for a sediment core).")>]
            Discarded: bool
        }
    
        /// Defines an individual temporal extent from a study and place,
        /// as defined within the research itself (not reinterpreted).
        type IndividualTimelineNode =
            | Continuous of resolution:TemporalResolution
            | Discontinuous of resolution:TemporalResolution * hiatuses:Hiatus list

        and TemporalResolution =
            /// time series that have even timesteps, for example tree ring data.
            | Regular of timestep:float<OldDate.calYearBP> * feature:RegularFeature
            /// time series that have uneven timesteps, for example sedimentary data.
            | Irregular
        
        and Hiatus = Hiatus of oldest:float<OldDate.calYearBP> * youngest:float<OldDate.calYearBP>

        and RegularFeature =
            | WoodAnatomicalFeatures
            | Varves
            | BoneOrToothGrowth
            | OtherRegularFeature of name:Text.ShortText

    /// The temporal index handles relations between calendar-based / qualitative-based
    /// dates in the past and occurrence data.
    module TemporalIndex =

        [<JsonObject(MemberSerialization = MemberSerialization.Fields)>]
        type CalYearNode = private CalYear of year:int<OldDate.calYearBP>

        let private unwrap (CalYear c) = c
        type CalYearNode with
            member this.Year = unwrap this

        let createTimeNode yearBeforePresent =
            if yearBeforePresent >= 1950 - System.DateTime.Now.Year
                && yearBeforePresent <= 14000
            then yearBeforePresent * 1<OldDate.calYearBP> |> CalYear |> Ok
            else Error <| sprintf "The year %i is not a valid time" yearBeforePresent

        type QualitativeLabelNode = {
            Name: Text.ShortText
            DesignatingAuthority: Text.ShortText
        }
    
    /// The master node type for 'exposure' peco element.
    type ExposureNode =
        | YearNode of TemporalIndex.CalYearNode
        | SliceLabelNode of TemporalIndex.QualitativeLabelNode
        | TimelineNode of StudyTimeline.IndividualTimelineNode
        | DateNode of StudyTimeline.IndividualDateNode

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
        | UncertaintyOldest         of IndividualDateNode * CalYearNode
        | UncertaintyYoungest       of IndividualDateNode * CalYearNode
        | OccursWithin              of IndividualDateNode * QualitativeLabelNode
        // from individual temporal extent:
        | ExtentEarliest            of IndividualTimelineNode * CalYearNode
        | ExtentEarliestUncertainty of IndividualTimelineNode * CalYearNode
        | ExtentLatest              of IndividualTimelineNode * CalYearNode
        | ExtentLatestUncertainty   of IndividualTimelineNode * CalYearNode
        | IntersectsTime            of IndividualTimelineNode * QualitativeLabelNode
        | ConstructedWithDate       of IndividualTimelineNode * IndividualDateNode
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
        | TimeEstimate                  of exact:OldDate.OldDateSimple
        | OccursWithin
        | UncertaintyOldest             of exact:OldDate.OldDateSimple
        | UncertaintyYoungest           of exact:OldDate.OldDateSimple
        | ExtentEarliest
        | ExtentEarliestUncertainty
        | ExtentLatest
        | ExtentLatestUncertainty
        | IntersectsTime
        | ConstructedWithDate
        | HasProxyInfo
        | HasOrphanProxy
        | IsLocatedAt