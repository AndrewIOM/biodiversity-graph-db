namespace BiodiversityCoder.Core

module Seed =

    open GraphStructure
    open Exposure
    open Exposure.TemporalIndex
    open Population
    open FieldDataTypes
    open Sources

    /// Tests adding in a hyper-edge for the taxon identification.
    /// Consists of four nodes: the three core nodes, plus the
    /// 'occurrence' node ('ProxiedTaxonNode')
    let testHyperEdge (graph:Graph.Graph<Node,Relation>) =
        result {
            let! morphotype = Text.createShort "Salix"
            let proxyNode = BioticProxies.Morphotype (BioticProxies.Microfossil (BioticProxies.Pollen, morphotype))

            let! inferSource = Text.create "Moore 1987. Pollen of the British Isles."
            let inferNode = BioticProxies.InferenceMethodNode.IdentificationKeyOrAtlas inferSource
            
            let! taxonName = Text.createShort "Salix"
            let taxonNode = Taxonomy.TaxonNode.Genus taxonName

            return!
                graph
                |> Graph.addNodeData makeUniqueKey [
                    PopulationNode (BioticProxyNode proxyNode)
                    PopulationNode (InferenceMethodNode inferNode)
                    PopulationNode (TaxonomyNode taxonNode) ] |> Result.lift fst
                |> Result.bind (Relations.addProxiedTaxon {
                    InferredFrom = proxyNode
                    InferredUsing = inferNode
                    InferredAs = taxonNode })
        }

    /// Import all sources from a bibtext file.
    let importSources path graph =
        let bibText = System.IO.File.ReadAllText(path)
        BibtexParser.parse bibText
        |> Result.lift(fun sources ->
            Graph.addNodeData makeUniqueKey (sources |> List.map(fun source -> SourceNode (Unscreened source))) graph)

    /// Initalise a graph with the core nodes required for the research question.
    let initGraph () =

        // Population: life node
        let taxonRoot = Population.Taxonomy.Life

        // Exposure: Holocene time index
        let addTimeIndexNodes graph = 
            [ -72 .. 14000 ]
            |> List.map createTimeNode 
            |> Result.ofList
            |> fun x -> printfn "Made ID"; x
            |> Result.map(List.map(fun n -> ExposureNode(YearNode n)))
            |> Result.map (fun (n: Node list) -> 
                printfn "Adding time nodes..."
                graph |> Graph.addNodeData makeUniqueKey n)

        let holoceneLabel = SliceLabelNode { Name = forceOk (Text.createShort "Holocene"); DesignatingAuthority = forceOk (Text.createShort "Global Stratotype Section and Point") }
        
        // Outcomes: basic measures
        let outcomes = [
            OutcomeNode(MeasureNode Outcomes.Biodiversity.Abundance)
            OutcomeNode(MeasureNode Outcomes.Biodiversity.DiversityBeta)
            OutcomeNode(MeasureNode Outcomes.Biodiversity.Evenness)
            OutcomeNode(MeasureNode Outcomes.Biodiversity.PresenceAbsence)
            OutcomeNode(MeasureNode Outcomes.Biodiversity.PresenceOnly)
            OutcomeNode(MeasureNode Outcomes.Biodiversity.Richness)
        ]

        // Add basic nodes without relations.
        let graph : Result<Graph.Graph<Node,Relation>,string> =
            Graph.empty
            |> Graph.addNodeData makeUniqueKey [
                PopulationNode (TaxonomyNode taxonRoot)
                ExposureNode holoceneLabel
            ] 
            |> Result.lift fst
            |> Result.bind (Graph.addNodeData makeUniqueKey outcomes) 
            |> Result.lift fst
            |> Result.bind addTimeIndexNodes
            |> Result.bind (fun r -> r |> Result.map fst)

        // Add core relations
        let addTimeRelation year relation graph = result {
            let! source = Nodes.tryFindTimePeriod (forceOk (Text.createShort "Holocene")) graph, "Could not get Holocene node"
            let! sink = Nodes.tryFindYear year graph, "Could not get year node"
            return! Relations.addRelation source sink relation 1 graph
        }
        
        graph
        |> Result.bind (addTimeRelation 11650<OldDate.calYearBP> (Exposure EarliestTime))
        |> Result.bind (addTimeRelation 0<OldDate.calYearBP> (Exposure LatestTime))
        // |> Result.bind testHyperEdge
