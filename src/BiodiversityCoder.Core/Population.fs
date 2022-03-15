namespace BiodiversityCoder.Core

module Population =

    module Taxonomy =

        open FieldDataTypes

        /// Broad taxonomic group of a taxon
        type TaxonomicGroup =
            | Amphibians
            | Birds
            | Fungi
            | Lichens
            | Mammals
            | Plants
            | Reptiles

        // Ranks vary between i.e. zoological and botanical classification
        // systems. For example, subfamilies and tribes in zoological ranks.
        // We are using generic relations for flexibility.
        type TaxonNode =
            | Life
            | Kingdom of name:Text.ShortText
            | Phylum of name:Text.ShortText
            | Class of name:Text.ShortText
            | Order of name:Text.ShortText
            | Family of name:Text.ShortText
            | Genus of name:Text.ShortText
            | Species of generic:Text.ShortText * specific:Text.ShortText
            | Subspecies of generic:Text.ShortText * specific:Text.ShortText * subspecific:Text.ShortText

        /// A common name given to a taxon, for example 'plants' for Plantae, or
        /// 'Manx Shearwater' for Puffinus puffinus.
        type VernacularTaxonLabelNode = {
            Label: Text.ShortText
            Language: LanguageCode.LanguageCode
            Source: Text.Text
        }


    /// Biotic proxies occur where a biotic component is used to infer
    /// an actual taxon (for example, pollen
    module BioticProxies =

        open FieldDataTypes

        /// The method through which a biotic proxy was inferred to
        /// be a particular taxon. For example, for microfossils it
        /// is common to use a particular regional atlas.
        type InferenceMethodNode =
            | IdentificationKeyOrAtlas of reference:Text.Text
            | Implicit

        type BioticProxyNode =
            | Morphotype of Morphotype
            | AncientDNA of taxon:Text.ShortText
            | DirectIdentification of taxon:Text.ShortText

        and Morphotype =
            | Microfossil of proxyGroup:MicrofossilGroup * morphotypeName:Text.ShortText
            | Fossil of morphotypeName: Text.ShortText

        and MicrofossilGroup =
            | Pollen
            | PlantMacrofossil
            | Diatom
            | Ostracod
            | OtherMicrofossil of Text.ShortText

    /// A hyper-edge representing the relation between a biotic proxy and
    /// taxon, which is only relevant within a particular study timeline context.
    module ProxiedTaxon =

        open BioticProxies
        open Taxonomy

        type ProxiedTaxonHyperEdge = {
            InferredFrom:BioticProxyNode
            InferredUsing:InferenceMethodNode
            InferredAs:TaxonNode
        }

    module Context =

        open FieldDataTypes

        type ContextNode = {
            Name: Text.ShortText
            SamplingLocation: Geography.SamplingLocation
            SampleOrigin: SampleOrigin
            SampleLocationDescription: Text.Text option
        }

        and SampleOrigin =
            | LakeSediment
            | PeatCore
            | Subfossil
            | LivingOrganism
            | OtherOrigin of Text.ShortText


    /// Relations that go from the exposure elements as the source
    type PopulationNodeRelation =
        // From taxonomy node:
        | IsA               of Taxonomy.TaxonNode * Taxonomy.TaxonNode
        | HasLabel          of Taxonomy.TaxonNode * Taxonomy.VernacularTaxonLabelNode
        // Links from hyper-edge:
        | InferredFrom      of ProxiedTaxon.ProxiedTaxonHyperEdge * BioticProxies.BioticProxyNode
        | InferredUsing     of ProxiedTaxon.ProxiedTaxonHyperEdge * BioticProxies.InferenceMethodNode
        | InferredAs        of ProxiedTaxon.ProxiedTaxonHyperEdge * Taxonomy.TaxonNode
        | MeasuredBy        of ProxiedTaxon.ProxiedTaxonHyperEdge * Outcomes.Biodiversity.BiodiversityDimensionNode
        
    /// A relationship originating from a population node
    and PopulationRelation =
        | IsA
        | HasLabel
        | InferredFrom
        | InferredUsing
        | InferredAs
