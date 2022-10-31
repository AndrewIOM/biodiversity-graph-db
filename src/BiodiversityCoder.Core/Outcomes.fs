namespace BiodiversityCoder.Core

module Outcomes =

    /// Types that represent measured outcomes.
    module Biodiversity =

        open FieldDataTypes

        type BiodiversityDimensionNode =
            | Richness
            | DiversityBeta
            | Evenness
            | Abundance
            | PresenceAbsence
            | PresenceOnly
            | OtherBiodiversityDimension of Text.ShortText
