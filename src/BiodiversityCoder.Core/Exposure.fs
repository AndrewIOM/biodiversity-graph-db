﻿namespace BiodiversityCoder.Core

open Newtonsoft.Json

module Exposure =

    open FieldDataTypes

    /// Temporal dimensions of a study or dataset.
    module StudyTimeline =
    
        /// An individual date made for a point in a discontinuous
        /// time-series, for example a stratigraphic sequence.
        type IndividualDateNode = {
            [<Name("Dating method")>]
            [<Help("Specify the dating method and associated raw date information.")>]
            Date: OldDate.OldDatingMethod
            [<Name("Dating measurement error")>]
            [<Help("If there is an error bound (+/-) around the date, specify this here. For a calibrated radiocarbon date, enter the error bound for the calibrated date here.")>]
            MeasurementError: OldDate.MeasurementError
            [<Name("Material that was dated")>]
            [<Help("Example: leaves; ostracod shells. Note: this is a free field, but be as succinct as possible.")>]
            MaterialDated: Text.ShortText
            [<Name("Sample depth")>]
            [<Help("If a depositional record, select 'Some' to enter the depth, otherwise 'None'. Enter all depths in centimetres (cm). Typically, a band of depth (Xcm-Ycm) will be stated. However, the source may fail to state the depths of its dates, or may only present dates linked to qualitative time periods (e.g. Thule).")>]
            SampleDepth: StratigraphicSequence.DepthInCore option
            [<Name("Lab number (optional)")>]
            [<Help("Examples: Lu-3272; AAR-852.")>]
            LabNumber: Text.ShortText option
            [<Name("Was this date discarded?")>]
            [<Help("Select 'No' unless (a) an age-depth model was applied AND (b) the date was provided but not used in the formation of the age-depth model (for a sediment core).")>]
            Discarded: bool
        }

        type RegularFeature =
            | WoodAnatomicalFeatures
            | Varves
            | BoneOrToothGrowth
            | OtherRegularFeature of name:Text.ShortText

        type TemporalResolution =
            /// time series that have even timesteps, for example tree ring data.
            | Regular of timestep:float<OldDate.calYearBP> * feature:RegularFeature
            /// time series that have uneven timesteps, for example sedimentary data.
            | Irregular
        
        type Hiatus = Hiatus of oldest:float<OldDate.calYearBP> * youngest:float<OldDate.calYearBP>

        /// Defines an individual temporal extent from a study and place,
        /// as defined within the research itself (not reinterpreted).
        type IndividualTimelineNode =
            | Continuous of resolution:TemporalResolution
            | Discontinuous of resolution:TemporalResolution * hiatuses:Hiatus list

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

        type QualitativeLabelOutOfScopeNode = {
            Name: Text.ShortText
            DesignatingAuthority: Text.ShortText
        }
    
    module Reanalysis =

        /// Represents a process to calibrate dates for one or many
        /// dates. For sedimentary sequences, many dates may be used
        /// within the calibration and also produce an age-depth model.
        type DateCalibrationNode = {
            CalibrationCurve: Text.ShortText
            ModelApplied: CalibrationModel
            SoftwareName: Text.ShortText
            SoftwareVersion: Text.ShortText
            Origin: OldDate.Harmonised.DateCalibrationOrigin
            AgeDepthModel: AgeDepthModelDepth list option
        }

        and AgeDepthModelDepth = {
            Depth: float<StratigraphicSequence.cm>
            Date: float<OldDate.calYearBP>
            StandardDeviation: float<OldDate.calYearBP> option
        }

        and CalibrationModel =
            | Unmodelled
            | OxCalModel of oxCalScript: Text.Text
            | OtherModel of description:Text.Text


    /// The master node type for 'exposure' peco element.
    type ExposureNode =
        | YearNode of TemporalIndex.CalYearNode
        | SliceLabelNode of TemporalIndex.QualitativeLabelNode
        | OutOfScopeNode of TemporalIndex.QualitativeLabelOutOfScopeNode
        | TimelineNode of StudyTimeline.IndividualTimelineNode
        | DateNode of StudyTimeline.IndividualDateNode
        | DateCalibrationInstanceNode of Reanalysis.DateCalibrationNode

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
        | OccursOutOfScope          of IndividualDateNode * QualitativeLabelOutOfScopeNode
        | UsedInCalibration         of IndividualDateNode * Reanalysis.DateCalibrationNode
        // from calibration
        | Calibrated                of Reanalysis.DateCalibrationNode * IndividualDateNode
        // from individual temporal extent:
        | ExtentEarliest            of IndividualTimelineNode * CalYearNode
        | ExtentEarliestUncertainty of IndividualTimelineNode * CalYearNode
        | ExtentEarliestOutOfScope  of IndividualTimelineNode * QualitativeLabelOutOfScopeNode
        | ExtentLatest              of IndividualTimelineNode * CalYearNode
        | ExtentLatestUncertainty   of IndividualTimelineNode * CalYearNode
        | IntersectsTime            of IndividualTimelineNode * QualitativeLabelNode
        | ConstructedWithDate       of IndividualTimelineNode * IndividualDateNode
        // from individual temporal extent:
        | HasProxyInfo              of IndividualTimelineNode * Population.ProxiedTaxon.ProxiedTaxonHyperEdge
        | HasProxyCategory          of IndividualTimelineNode * Population.BioticProxies.BioticProxyCategoryNode
        | HasOrphanProxy            of IndividualTimelineNode * Population.BioticProxies.BioticProxyNode
        // from individual temporal extent:
        | IsLocatedAt               of IndividualTimelineNode * Population.Context.ContextNode
        // from individual temporal extent:
        | HasRawData                of IndividualTimelineNode * Datasets.DatasetNode
        // from individual temporal extent:
        | ExtentEarliestHarmonised  of IndividualTimelineNode * CalYearNode
        | ExtentLatestHarmonised    of IndividualTimelineNode * CalYearNode


    /// A relationship originating from an exposure node
    and ExposureRelation =
        | Next
        | Contains
        | EarliestTime
        | LatestTime
        | TimeEstimate                  of exact:OldDate.OldDateSimple
        | OccursWithin
        | OccursOutOfScope
        | UsedInCalibration
        | Calibrated                    of calibration:OldDate.Harmonised.DateCalibration
        | UncertaintyOldest             of exact:OldDate.OldDateSimple
        | UncertaintyYoungest           of exact:OldDate.OldDateSimple
        | ExtentEarliest
        | ExtentEarliestSpecified       of exact:OldDate.OldDateSimple option
        | ExtentEarliestUncertainty
        | ExtentEarliestOutOfScope      of exact:OldDate.OldDateSimple
        | ExtentLatestSpecified         of exact:OldDate.OldDateSimple option
        | ExtentLatest
        | ExtentLatestUncertainty
        | IntersectsTime
        | ConstructedWithDate
        | HasProxyInfo
        | HasProxyCategory
        | HasOrphanProxy
        | IsLocatedAt
        | HasRawData
        | ExtentEarliestHarmonised      of exact:OldDate.Harmonised.OldDateSimpleHarmonised
        | ExtentLatestHarmonised        of exact:OldDate.Harmonised.OldDateSimpleHarmonised