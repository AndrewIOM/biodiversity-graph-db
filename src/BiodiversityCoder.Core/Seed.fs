namespace BiodiversityCoder.Core

module Seed =

    open GraphStructure
    open Exposure
    open Exposure.TemporalIndex
    open Population
    open FieldDataTypes

    /// Tests adding in a hyper-edge for the taxon identification.
    /// Consists of four nodes: the three core nodes, plus the
    /// 'occurrence' node ('ProxiedTaxonNode')
    let testHyperEdge (graph:Graph.Graph<Node,Relation>) =
        result {
            let! morphotype = Text.createShort "salix"
            let proxyNode = BioticProxies.Morphotype (BioticProxies.Microfossil (BioticProxies.Pollen, morphotype))

            let! inferSource = Text.create "Moore 1987. Pollen of the British Isles."
            let inferNode = BioticProxies.InferenceMethodNode.IdentificationKeyOrAtlas inferSource
            
            let! taxonName = Text.createShort "Salix"
            let taxonNode = Taxonomy.TaxonNode.Genus taxonName

            return!
                graph
                |> Graph.addNodeData [
                    PopulationNode (BioticProxyNode proxyNode)
                    PopulationNode (InferenceMethodNode inferNode)
                    PopulationNode (TaxonomyNode taxonNode) ] |> fst
                |> Relations.addProxiedTaxon {
                    InferredFrom = proxyNode
                    InferredUsing = inferNode
                    InferredAs = taxonNode }
        }

    /// Import all sources from a bibtext file.
    let importSources path graph =
        let bibText = System.IO.File.ReadAllText(path)
        BibtexParser.parse bibText
        |> Result.lift(fun sources ->
            Graph.addNodeData (sources |> List.map(fun source -> SourceNode source)) graph)

    /// Initalise a graph with the core nodes required for the research question.
    let initGraph () =

        // Population: life node
        let taxonRoot = Population.Taxonomy.Life

        // Exposure: Holocene time index
        let addTimeIndexNodes graph = 
            [ 0 .. 14000 ] 
            |> List.map createTimeNode 
            |> List.mapResult id
            |> Result.map(List.map(fun n -> ExposureNode(YearNode n)))
            |> Result.map (fun n -> graph |> Graph.addNodeData n)

        let holoceneLabel = SliceLabelNode { Name = "Holocene" }
        
        // Outcomes: basic measures
        let outcomes = [
            OutcomeNode(MeasureNode Outcomes.Biodiversity.Abundance)
            OutcomeNode(MeasureNode Outcomes.Biodiversity.PresenceAbsence)
        ]

        // Add basic nodes without relations.
        let graph : Result<Graph.Graph<Node,Relation>,string> =
            Graph.empty
            |> Graph.addNodeData [
                PopulationNode (TaxonomyNode taxonRoot)
                ExposureNode holoceneLabel
            ] |> fst
            |> Graph.addNodeData outcomes |> fst
            |> addTimeIndexNodes |> Result.map fst

        // Add core relations
        let addTimeRelation year relation graph = result {
            let! source = Nodes.tryFindTimePeriod "Holocene" graph, "Could not get Holocene node"
            let! sink = Nodes.tryFindYear year graph, "Could not get year node"
            return! Relations.addRelation source sink relation 1 graph
        }
        
        graph 
        |> Result.bind (addTimeRelation 11650<OldDate.calYearBP> (Exposure EarliestTime))
        |> Result.bind (addTimeRelation 0<OldDate.calYearBP> (Exposure LatestTime))
        |> Result.bind testHyperEdge
