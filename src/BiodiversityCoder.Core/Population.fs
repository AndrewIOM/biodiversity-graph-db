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
            | Subfamily of name:Text.ShortText
            | Tribe of name:Text.ShortText
            | Subtribe of name:Text.ShortText
            | Genus of name:Text.ShortText
            | Subgenus of name:Text.ShortText
            | Species of generic:Text.ShortText * specific:Text.ShortText * authorship:Text.ShortText
            | Subspecies of generic:Text.ShortText * specific:Text.ShortText * subspecific:Text.ShortText * authorship:Text.ShortText
            | Variety of generic:Text.ShortText * specific:Text.ShortText * subspecific:Text.ShortText * authorship:Text.ShortText

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
            | ImplicitByExpert of lastName:Text.ShortText * initials:Text.ShortText

        /// Biotic proxy represents the physical nature of the material (e.g. the component)
        /// and the taxonomic label attached to it. Examples:
        /// - Microfossil. e.g. Pinus-type pollen - a **Pollen** of morphotype name **Pinus-type**
        /// - Macrofossil. e.g. Pine cone - a **Conifer Cone** of taxonomic group **Pinus**
        /// - Megafossil. e.g. subfossil trunk - a **Wood Stem** of taxonomic group **Pinus**
        /// - Megafossil. e.g. driftwood - a **Wood Stem** of taxonomic group **Pinus**
        /// - Megafossil. e.g. whole mammoth - a **Whole Organism** of taxonomic group **Mammuthus**
        /// - AncientDNA. e.g. Pinus sylvestris. **AncientDNA** of taxon **Pinus sylvestris**
        /// - ContemporaneousWholeOrganism. e.g. Wood disc collected at modern day. It was a **Pinus sylvestris**
        type BioticProxyNode =
            | Morphotype of morphotype:Morphotype
            | AncientDNA of taxon:Text.ShortText
            | ContemporaneousWholeOrganism of taxon:Text.ShortText

        /// Definitions of terms:
        /// organismPart = the name of the component used to conduct identification (e.g. trunk for driftwood)
        and Morphotype =
            | Microfossil of proxyGroup:MicrofossilGroup * morphotypeName:Text.ShortText
            | Macrofossil of organismPart:Text.ShortText * morphotypeName: Text.ShortText
            | Megafossil of organismPart:Text.ShortText * morphotypeName: Text.ShortText

        and MicrofossilGroup =
            | Pollen
            | PlantMacrofossil
            | Diatom
            | Ostracod
            | OtherMicrofossilGroup of Text.ShortText

    /// A hyper-edge representing the relation between a biotic proxy and
    /// taxon, which is only relevant within a particular study timeline context.
    module ProxiedTaxon =

        open BioticProxies
        open Taxonomy

        type ProxiedTaxonHyperEdge = {
            InferredFrom:BioticProxyNode
            InferredUsing:InferenceMethodNode
            InferredAs:TaxonNode
            InferredAsAdditional:TaxonNode list
        }

    module Context =

        open FieldDataTypes

        type ContextNode = {
            [<Help("A short description for the place.")>]
            Name: Text.ShortText
            [<Help("Enter the location for the specific timeline in the granularity specified in the text. Locations may be specified as a spatial point ('Site') or polygon ('Area'), or as political units. If using a point ('site'), you may enter coordinates in decimal degrees (DD) using 'Site' or in DMS (format: 40°26'46\"N,79°58'56\"W) using 'SiteDMS'. If using political units, attempt to ensure that the unit conforms to those in GADM https://www.gadm.org/maps.html. If entering a polygon, enter your polygon in WKT format (e.g. POLYGON((26.41 41.79,43.11 41.79,43.11 32.87,26.41 32.87,26.41 41.79)). You can make WKT format polygons on this web page: http://arthur-e.github.io/Wicket/sandbox-gmaps3.html")>]
            SamplingLocation: Geography.SamplingLocation
            [<Help("The parent material from which the 'Outcome' (biodiversity measure) has been measured. Example: pollen from a midden is subfossil.")>]
            SampleOrigin: SampleOrigin
            [<Help("An optional free-form description of the characteristics of the location of the time-series. Example: lake surrounded by Salix and Betula tall shrubs.")>]
            SampleLocationDescription: Text.Text option
        }

        and SampleOrigin =
            | LakeSediment of depths:StratigraphicSequence.DepthExtent
            | PeatCore of depths:StratigraphicSequence.DepthExtent
            | Excavation of depths:StratigraphicSequence.DepthExtent
            | Subfossil
            | LivingOrganism
            | OtherOrigin of origin:Text.ShortText * depths:StratigraphicSequence.DepthExtent option

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
        | MeasuredBy