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
            | OtherBiodiversityDimension of Text.ShortText
