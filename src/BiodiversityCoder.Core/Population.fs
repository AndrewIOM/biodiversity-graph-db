﻿namespace BiodiversityCoder.Core

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
            | Clade of name:Text.ShortText

        /// A common name given to a taxon, for example 'plants' for Plantae, or
        /// 'Manx Shearwater' for Puffinus puffinus.
        type VernacularTaxonLabelNode = {
            Label: Text.ShortText
            Language: LanguageCode.LanguageCode
            Source: Text.Text
        }

        /// Represents a database 
        type NomenclatureIndexNode = {
            Name: Text.ShortText
        }


    /// Biotic proxies occur where a biotic component is used to infer
    /// an actual taxon (for example, pollen
    module BioticProxies =

        open FieldDataTypes

        type IdentificationConfidence =
            | Reliability of ReliabilityConfidence
            | Unspecified

        and ReliabilityConfidence =
            | Unreliable
            | FairlyReliable
            | FullyReliable

        type TaxonLookup = {
            [<Help("The system that has been used for taxonomic names.")>]
            Nomleclature: Text.ShortText
            Entries: TaxonLookupItem list
            Reference: Text.Text
        }

        and TaxonLookupItem = {
            MorphotypeName: Text.ShortText
            Taxa: Taxonomy.TaxonNode list
            Confidence: IdentificationConfidence
        }

        /// The method through which a biotic proxy was inferred to
        /// be a particular taxon. For example, for microfossils it
        /// is common to use a particular regional atlas. Key components
        /// of the inference method are:
        /// (a) Taxonomic nomenclature (i.e. if species names etc. are used without authorships)
        /// (b) Morphotype terminology (i.e. the source used to define naming conventions)
        /// (c) Pollen key or atlas(es) (i.e. one or more pollen keys) 
        /// There may be multiple steps to the inference method, which may or
        /// may not be explicit in the text; in these cases, use multiple
        /// relations to these nodes.
        type InferenceMethodNode =
            | IdentificationKeyOrAtlas of reference:Text.Text
            | IdentificationKeyOrAtlasWithLookup of lookup:TaxonLookup
            | ReferenceCollection of name:Text.ShortText * location: Text.ShortText
            | Implicit
            | ImplicitByExpert of lastName:Text.ShortText * initials:Text.ShortText
            | TaxonomicNomenclature of reference: Text.ShortText
            | MorphotypeTerminology of reference: Text.ShortText

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

        type BioticProxyCategoryNode =
            | AncientDNA of Taxonomy.TaxonomicGroup
            | Microfossil of MicrofossilGroup
            | Fossil of Taxonomy.TaxonomicGroup
            | Contemporary of Taxonomy.TaxonomicGroup
            | OtherProxy of Text.ShortText

    /// A hyper-edge representing the relation between a biotic proxy and
    /// taxon, which is only relevant within a particular study timeline context.
    module ProxiedTaxon =

        open BioticProxies
        open Taxonomy

        type ProxiedTaxonHyperEdge = {
            InferredFrom:BioticProxyNode
            InferredUsing:InferenceMethodNode
            InferredUsingAdditional:InferenceMethodNode list
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
            [<Help("The parent material from which the 'Outcome' (biodiversity measure) has been measured. Example: pollen from a midden is subfossil. For sequences, you may be asked for a depth range. The 'top depth' means the closest to the surface; depths must be entered in centimetres.")>]
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
        | IsSynonymOf       of Taxonomy.TaxonNode * Taxonomy.TaxonNode
        | HasIdentifier     of Taxonomy.TaxonNode * Taxonomy.NomenclatureIndexNode
        | HasLabel          of Taxonomy.TaxonNode * Taxonomy.VernacularTaxonLabelNode
        // Links from hyper-edge:
        | InferredFrom      of ProxiedTaxon.ProxiedTaxonHyperEdge * BioticProxies.BioticProxyNode
        | InferredUsing     of ProxiedTaxon.ProxiedTaxonHyperEdge * BioticProxies.InferenceMethodNode
        | InferredAs        of ProxiedTaxon.ProxiedTaxonHyperEdge * Taxonomy.TaxonNode
        | MeasuredBy        of ProxiedTaxon.ProxiedTaxonHyperEdge * Outcomes.Biodiversity.BiodiversityDimensionNode
        
    /// A relationship originating from a population node
    and PopulationRelation =
        | IsA
        | IsSynonymOf of opinionMadeBySource:FieldDataTypes.Text.ShortText * dateMade:Time.SimpleDateOnly
        | HasIdentifier of checklistId:FieldDataTypes.Text.ShortText
        | HasLabel
        | InferredFrom
        | InferredUsing
        | InferredAs
        | MeasuredBy