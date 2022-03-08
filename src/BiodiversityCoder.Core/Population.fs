namespace BiodiversityCoder.Core

open System

module Population =

    // Hyper-edges are an instance of the product of n nodes.

    module SpokenLanguages =

        open System.Globalization

        type LanguageCode = private LanguageCode of string

        let languageCode (culture:CultureInfo) =
            culture.TwoLetterISOLanguageName |> LanguageCode


    module Taxonomy =

        open SpokenLanguages

        // TODO ranks vary between i.e. zoological and botanical classification
        // systems. For example, subfamilies and tribes in zoological ranks.
        // It may be that we need to avoid generic relations in the tree.
        type Taxon =
            | Life
            | Kingdom of name:string
            | Phylum of name:string
            | Class of name:string
            | Order of name:string
            | Family of name:string
            | Genus of name:string
            | Species of generic:string * specific:string
            | Subspecies of generic:string * specific:string * subspecific:string

        /// A common name given to a taxon, for example 'plants' for Plantae, or
        /// 'Manx Shearwater' for Puffinus puffinus.
        type VernacularTaxonLabel = {
            Label: string
            Language: LanguageCode
            Source: string // TODO more accurate representation required.
        }

        type TaxonRelation =
            | HasLabel of VernacularTaxonLabel * Taxon
            | DirectChildOf of Taxon * Taxon


    /// Biotic proxies occur where a biotic component is used to infer
    /// an actual taxon (for example, pollen
    module BioticProxies =

        /// For example, a pollen atlas
        type InferenceMethod = {
            Name: string
        }

        type BioticProxy =
            | Morphotype of string

    /// A hyper-edge representing the relation between a biotic proxy and
    /// taxon, which is only relevant within a particular study timeline context.
    module ProxiedTaxon =

        open BioticProxies
        open Taxonomy

        type ProxiedTaxon = {
            InferredFrom:BioticProxy
            InferredUsing:InferenceMethod
            InferredAs:Taxon
        }

        type ProxiedTaxonRelations =
            | InferredFrom of ProxiedTaxon * BioticProxy
            | InferredUsing of ProxiedTaxon * InferenceMethod
            | InferredAs of ProxiedTaxon * Taxon



    ///// Contains types to represent the population,
    ///// which in this case is Arctic biota.
    //module Population =

    //    // A study may identify

    //    type TaxonomicGroup =
    //        | Amphibians
    //        | Birds
    //        | Fungi
    //        | Lichens
    //        | Mammals
    //        | Plants
    //        | Reptiles

    //    type MorphotypeComponent =
    //        | Cool

    //    type TaxonomicLabel =
    //        | Morphotype of MorphotypeComponent
    //        | Genetic

    //    // Taxonomic unit.
    //    // - The certainty of attributing proxy data to a species etc. varies substantially between proxies.
    //    // - For pollen, this can be very uncertain (and capture one to many taxonomic umbrellas).
    //    type BioticUnit =
    //        | Taxonomic of string

    //    // Pollen and other proxy data may indicate the taxonomic group..
    //    // - Pollen ~> Taxon translation table (with uncertainty?)


    //    type DataAvailability =
    //        | Unavailable
    //        | FullOnline of Uri

    //    // Types should have a Gen method to make the possible values?
    //    // - This could work from an existing data source.
    //    // -

    //    // How was the species etc. identified as what it is?
    //    type BioticProxy =
    //        | Morphotype
    //        | DNA
    //        | DirectIdentification

    //    and Morphotype =
    //        | Pollen
    //        | Bone

    //    /// Governs relations between nodes in the temporal index
    //    type PopulationRelation =
    //        | IsA of BioticUnit * TaxonomicGroup
    //        | IdentifiedBy of BioticUnit * BioticProxy
    //        | KnownMeasureFor // NB includes abundance & presence/absence?


    //    // A study may contain one or more temporal extents (i.e. they may be discontinuous).
    //    // Within the temporal extent, certain biota (e.g. a bird species) may be measured
    //    // (e.g. present / absent, abundance). 