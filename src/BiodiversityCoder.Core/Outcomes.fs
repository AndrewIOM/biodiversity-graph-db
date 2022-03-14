namespace BiodiversityCoder.Core

module Outcomes =

    /// Types that represent the measured outcomes
    /// of the exposure.

    module BiodiversityMeasures =

        type MeasureNode =
            | Abundance
            | PresenceAbsence

    /// Relations that go from the exposure elements as the source
    //type OutcomeNodeRelation = 
    //    | ?