namespace BiodiversityCoder.Core

/// Simple representation of a graph data structure
/// (adapted from https://orbifold.net/default/fsharp-graphs/).
[<RequireQualifiedAccess>]
module Graph =
    
    /// Each key is unique within its type.
    type UniqueKey =
        | FriendlyKey of nodeType: string * string
        | UUID of nodeType: string * System.Guid

        with 
            member this.AsString =
                match this with
                | FriendlyKey (t,f) -> sprintf "%s_%s" t f
                | UUID (t,u) -> sprintf "%s_%O" t u

    let inline (|Parse|_|) (str: string): ^a option =
        let mutable value = Unchecked.defaultof< ^a>
        let result = (^a: (static member TryParse: string * byref< ^a> -> bool) str, &value)
        if result then Some value
        else None

    let stringToKey (str:string) =
        match str.Split("_").[1] with
        | Parse(x: System.Guid) -> UUID (str.Split("_").[0], x)
        | _ -> FriendlyKey (str.Split("_").[0], str.Split("_") |> Seq.tail |> String.concat "_")

    type Node<'TData> = UniqueKey * 'TData
    
    type Connection<'TData> =
        UniqueKey * // source Id
        UniqueKey * // sink id
        int * // weight
        'TData // connection data
    
    /// Lists all connections
    type Adjacency<'TStructon> =
        Connection<'TStructon> list
        
    /// A single node with its outgoing connections
    type Atom<'TNodeDaton, 'TConnectionDaton> =
        Node<'TNodeDaton> * Adjacency<'TConnectionDaton>

    type Graph<'TNodeDaton, 'TConnectionDaton> =
        Atom<'TNodeDaton, 'TConnectionDaton> list

    let empty : Graph<_,_> = []

    let getAtomId atom =
        atom |> fst |> fst

    let getAtom id graph =
        graph
        |> List.tryFind(fun atom -> atom |> getAtomId = id)

    let getNode id graph =
        match getAtom id graph with
        | Some a -> a |> fst |> Some
        | None -> None

    let addNode node graph =
        let id = fst node
        match getNode id graph with
        | None ->
            let newAdjacency = []
            let newAtom = node, newAdjacency
            graph @ [ newAtom ] |> Ok
        | Some _ -> Error "node already exists"

    let addNodeOrSkip node graph =
        let id = fst node
        match getNode id graph with
        | None ->
            let newAdjacency = []
            let newAtom = node, newAdjacency
            graph @ [ newAtom ] |> Ok
        | Some _ -> graph |> Ok

    let replaceNodeData node graph : Result<Graph<'nodeData,_>,string> =
        let id = fst node
        match getAtom id graph with
        | None -> Error "Node doesn't already exist"
        | Some (oldNode, oldAdjacency) ->
            let newAtom = (id, (snd node)), oldAdjacency
            graph 
            |> List.except [(oldNode, oldAdjacency)]
            |> List.append [ newAtom ]
            |> Ok

    let addNodeData (makeKey:'nodeData -> UniqueKey) (items:'nodeData seq) (graph:Graph<'nodeData,_>) =
        Seq.fold(fun (acc) data ->
                    let newNode = makeKey data, data
                    (acc 
                     |> Result.bind(fun (acc,acc2) -> addNode newNode acc |> Result.map(fun r -> r,acc2))
                     |> Result.lift(fun (r,acc2) -> r, newNode :: acc2))
                     ) (Ok (graph, [])) items
    
    let addNodeDataOrSkip (makeKey:'nodeData -> UniqueKey) (items:'nodeData seq) (graph:Graph<'nodeData,_>) =
        Seq.fold(fun (acc) data ->
                    let newNode = makeKey data, data
                    (acc 
                     |> Result.bind(fun (acc,acc2) -> addNodeOrSkip newNode acc |> Result.map(fun r -> r,acc2))
                     |> Result.lift(fun (r,acc2) -> r, newNode :: acc2))
                     ) (Ok (graph, [])) items
    
    let pointsTo id atom =
        atom 
        |> snd
        |> List.exists(fun (_,y,_,_) -> y = id)

    let getConnectionSinkId ((_,sinkId,_,_):Connection<_>) =
        sinkId

    let private filterHasNotSinkId id =
        fun conn -> getConnectionSinkId conn <> id

    /// Remove a node and its connections from the graph.
    /// Identifies and removes all connections associated with the node.
    let removeNode id graph =
        List.fold(fun acc (n,a) ->
            if (fst n) = id 
            then acc
            else
                if pointsTo id (n,a) 
                then
                    let newAd = a |> List.filter (filterHasNotSinkId id)
                    let newAtom = n, newAd
                    acc @ [ newAtom ]
                else acc @ [ n, a ]
            ) [] graph

    /// Connect two nodes together given a relationship.
    /// If an identical relation already exists, it will not be duplicated.
    let addRelation (sourceId:UniqueKey) sinkId weight connData (graph:Graph<_,_>) : Result<Graph<'nodeData,'connData>,string> =
        let source = graph |> List.tryFind(fun n -> fst (fst n) = sourceId)
        let sink = graph |> List.tryFind(fun n -> fst (fst n) = sourceId)
        if source.IsNone || sink.IsNone
        then Error "The connection was not between valid nodes"
        else
            graph
            |> List.map(fun i ->
                if i = source.Value
                then
                    let conn : Connection<'connData> = (sourceId, sinkId, weight, connData)
                    let adjacency = 
                        if (snd i) |> Seq.contains conn
                        then snd source.Value
                        else conn :: snd source.Value
                    (fst source.Value, adjacency)
                else i)
            |> Ok

    let tryFind cond (graph:Graph<_,_>) =
        graph |> Seq.tryFind(fun g -> cond g)

/// The core structure of nodes and relations that governs
/// the ecology mapper database.
module GraphStructure =

    open Sources
    open Population
    open Exposure
    open Outcomes

    type PopulationNode =
        | TaxonomyNode of Taxonomy.TaxonNode
        | BioticProxyNode of BioticProxies.BioticProxyNode
        | BioticProxyCategoryNode of BioticProxies.BioticProxyCategoryNode
        | InferenceMethodNode of BioticProxies.InferenceMethodNode
        | ContextNode of Context.ContextNode
        | VernacularTaxonLabelNode of Taxonomy.VernacularTaxonLabelNode
        | ProxiedTaxonNode // Intermediate node that represents a hyperedge

    type OutcomeNode =
        | MeasureNode of Biodiversity.BiodiversityDimensionNode

    /// Routing type to represent all possible nodes within the
    /// evidence graph.
    type Node =
        | SourceNode of SourceNode
        | PopulationNode of PopulationNode
        | ExposureNode of ExposureNode
        | OutcomeNode of OutcomeNode

    /// Routing type to represent all possible relations within
    /// the evidence graph.
    type NodeRelation =
        | FromSource of Sources.SourceNodeRelation
        | FromPopulation of Population.PopulationNodeRelation
        | FromExposure of Exposure.ExposureNodeRelation

    /// Routing type to represent all possible relations within
    /// the evidence graph. Can only be created using `makeRelation`,
    /// which constrains relations to valid node types only.
    // TODO see below. [<Newtonsoft.Json.JsonObject(MemberSerialization = Newtonsoft.Json.MemberSerialization.Fields)>]
    type Relation =
        // private. TODO figure out how to get Json.net to deserialise this when private.
        | Source of SourceRelation
        | Population of PopulationRelation
        | Exposure of ExposureRelation

    type ProposedRelation =
        | Source of SourceRelation
        | Population of PopulationRelation
        | Exposure of ExposureRelation

    let tryAlphanum (c:char) =
        if System.Char.IsLetter c || System.Char.IsNumber c then Some c else None

    type Node with

        member this.NodeType () =
            match this with
            | PopulationNode p ->
                match p with
                | BioticProxyNode n -> "BioticProxyNode"
                | BioticProxyCategoryNode n -> "BioticProxyCategoryNode"
                | TaxonomyNode n -> "TaxonNode"
                | InferenceMethodNode n -> "InferenceMethodNode"
                | ProxiedTaxonNode -> "ProxiedTaxonNode"
                | ContextNode _ -> "ContextNode"
                | VernacularTaxonLabelNode _ -> "VernacularTaxonLabelNode"
            | SourceNode s ->
                match s with
                | Unscreened s2 -> "SourceNode"
                | Included _ -> "SourceNode"
                | Excluded (s2,_,_) -> "SourceNode"
            | ExposureNode e ->
                match e with
                | YearNode _ -> "CalYearNode"
                | SliceLabelNode _ -> "QualitativeLabelNode"
                | TimelineNode _ -> "IndividualTimelineNode"
                | DateNode _ -> "IndividualDateNode"
                | OutOfScopeNode _ -> "QualitativeLabelOutOfScopeNode"
            | OutcomeNode o ->
                match o with
                | MeasureNode n -> "BiodiversityDimensionNode"

        /// Provides a (non-unique) 'pretty' name for use in user interfaces to display
        /// the node, e.g. in a dropdown list or table.
        member this.DisplayName () =
            match this with
            | PopulationNode p ->
                match p with
                | BioticProxyNode n ->
                    match n with
                    | Population.BioticProxies.BioticProxyNode.AncientDNA a -> sprintf "aDNA: %s" a.Value
                    | Population.BioticProxies.BioticProxyNode.ContemporaneousWholeOrganism taxon -> sprintf "Contemporaneous Whole Organism: %s" taxon.Value
                    | Population.BioticProxies.BioticProxyNode.Morphotype m -> 
                        match m with
                        | Population.BioticProxies.Megafossil (part,f) -> sprintf "Megafossil: %s (%s)" f.Value part.Value
                        | Population.BioticProxies.Macrofossil (part,f) -> sprintf "Macrofossil: %s (%s)" f.Value part.Value
                        | Population.BioticProxies.Microfossil (group, name) ->
                            match group with
                            | Population.BioticProxies.MicrofossilGroup.Diatom -> sprintf "Morphotype: Diatom - %s" name.Value
                            | Population.BioticProxies.MicrofossilGroup.Ostracod -> sprintf "Morphotype: Ostracod - %s" name.Value
                            | Population.BioticProxies.MicrofossilGroup.PlantMacrofossil -> sprintf "Morphotype: Plant Macrofossil - %s" name.Value
                            | Population.BioticProxies.MicrofossilGroup.Pollen -> sprintf "Morphotype: Pollen - %s" name.Value
                            | Population.BioticProxies.MicrofossilGroup.OtherMicrofossilGroup group -> sprintf "Morphotype: %s - %s" group.Value name.Value
                | BioticProxyCategoryNode n -> n.ToString()
                | TaxonomyNode n ->
                    match n with
                    | Taxonomy.TaxonNode.Life -> "Life"
                    | Taxonomy.TaxonNode.Kingdom l -> sprintf "%s [Kingdom]" l.Value
                    | Taxonomy.TaxonNode.Phylum l -> sprintf "%s [Phylum]" l.Value
                    | Taxonomy.TaxonNode.Class l -> sprintf "%s [Class]" l.Value
                    | Taxonomy.TaxonNode.Clade l -> sprintf "%s [Clade]" l.Value
                    | Taxonomy.TaxonNode.Order l -> sprintf "%s [Order]" l.Value
                    | Taxonomy.TaxonNode.Family l -> sprintf "%s [Family]" l.Value
                    | Taxonomy.TaxonNode.Subfamily l -> sprintf "%s [Sub-Family]" l.Value
                    | Taxonomy.TaxonNode.Tribe l -> sprintf "%s [Tribe]" l.Value
                    | Taxonomy.TaxonNode.Subtribe l -> sprintf "%s [Sub-Tribe]" l.Value
                    | Taxonomy.TaxonNode.Genus l -> sprintf "%s [Genus]" l.Value
                    | Taxonomy.TaxonNode.Subgenus l -> sprintf "%s [Sub-Genus]" l.Value
                    | Taxonomy.TaxonNode.Species (l,l2,l3) -> sprintf "%s %s %s [Species]" l.Value l2.Value l3.Value
                    | Taxonomy.TaxonNode.Subspecies (l,l2,l3, l4) -> sprintf "%s %s subsp. %s %s [Subspecies]" l.Value l2.Value l3.Value l4.Value
                    | Taxonomy.TaxonNode.Variety (l,l2,l3, l4) -> sprintf "%s %s var. %s %s [Variety]" l.Value l2.Value l3.Value l4.Value
                | InferenceMethodNode n ->
                    match n with
                    | BioticProxies.InferenceMethodNode.Implicit -> "Implicit"
                    | BioticProxies.InferenceMethodNode.IdentificationKeyOrAtlas r -> sprintf "Explicit: Atlas or Key - %s" r.Value
                    | BioticProxies.InferenceMethodNode.ImplicitByExpert (lastName, initials) -> sprintf "Implicit: Expert ID - %s, %s" lastName.Value initials.Value
                | ProxiedTaxonNode -> "[Proxied taxon hyper-edge]"
                | ContextNode n -> sprintf "%s: %s" (n.SamplingLocation.GetType().Name) n.Name.Value
                | VernacularTaxonLabelNode(_) -> failwith "Not Implemented"
            | SourceNode s ->
                match s with
                | Unscreened s
                | Included (s,_)
                | Excluded (s,_,_) ->
                    match s with
                    | Bibliographic n -> 
                        sprintf "%s (%s). %s" 
                            (if n.Author.IsSome then n.Author.Value.Value else "?")
                            (if n.Year.IsSome then n.Year.Value.ToString() else "?")
                            (if n.Title.IsSome then n.Title.Value.Value else "?")
                    | GreyLiterature n -> sprintf "Grey literature source: %s" n.Title.Value
                    | DarkData n -> sprintf "'Dark data' from %s" n.Contact.LastName.Value
                    | Database n -> sprintf "Database: %s" n.FullName.Value
                    | DatabaseEntry n -> sprintf "Database: %s - entry %s" n.DatabaseAbbreviation.Value n.UniqueIdentifierInDatabase.Value
                    | PublishedSource p ->
                        match p with
                        | Book b ->
                            sprintf "%s (%i). [book] %s. %s"
                                (FieldDataTypes.Author.authorList (List.concat [ [ b.BookFirstAuthor]; b.BookAdditionalAuthors ] ))
                                b.BookCopyrightYear b.BookTitle.Value
                                    (if b.ISBN.IsSome then b.ISBN.Value.Value + " (ISBN)"
                                        else if b.ISSNDOI.IsSome then b.ISSNDOI.Value.Value + " (ISSN/DOI)" else "")
                        | BookChapter n ->
                            sprintf "%s. [book chapter] %s"
                                (FieldDataTypes.Author.authorList (List.concat [ [ n.ChapterFirstAuthor]; n.ChapterAdditionalAuthors ] ))
                                n.ChapterTitle.Value
                        | Dissertation n ->
                            sprintf "%s (%i). [dissertation] %s" n.Author.Display n.CompletionYear n.Title.Value
                        | IndividualDataset d ->
                            sprintf "%s (%s). [dataset] %s." (FieldDataTypes.Author.authorList d.Contributors)
                                (if d.YearPublished.IsSome then d.YearPublished.Value.ToString() else "Unknown year")
                                d.Title.Value
                        | JournalArticle n ->
                            sprintf "%s (%i). %s. %s" 
                                (FieldDataTypes.Author.authorListTruncated (n.FirstAuthor :: n.AdditionalAuthors) 5)
                                n.Year
                                n.Title.Value
                                n.Journal.Value
                    | GreyLiteratureSource n ->
                        sprintf "%s (%s). [grey|%s] %s%s"
                            (FieldDataTypes.Author.authorList n.Contributors)
                            (if n.PostedYear.IsSome then n.PostedYear.Value.ToString() else "Unknown year")
                            (n.Format.ToString())
                            n.Title.Value
                            (if n.Institution.IsSome then ". " + n.Institution.Value.Value else "")
                    | DarkDataSource n ->
                        sprintf "%s unpublished. [dataset] %s%s"
                            (FieldDataTypes.Author.authorList (List.concat [ [ n.Investigator]; n.AdditionalInvestigators ]))
                            (if n.Title.IsSome then n.Title.Value.Value + ". " else "")
                            (n.Details.Value |> Seq.truncate 50 |> Seq.map string |> String.concat "")
            | ExposureNode e ->
                match e with
                | YearNode y -> sprintf "%i cal yr BP" y.Year
                | SliceLabelNode n -> sprintf "%s (designated by: %s)" n.Name.Value n.DesignatingAuthority.Value
                | TimelineNode n -> "A study timeline"
                | DateNode n -> "An individual date"
                | OutOfScopeNode n -> sprintf "%s (designated by: %s)" n.Name.Value n.DesignatingAuthority.Value
            | OutcomeNode o ->
                match o with
                | MeasureNode n -> n.ToString()

    let private safeString s = System.Text.RegularExpressions.Regex.Replace(System.Net.WebUtility.HtmlEncode s, @"[^aA-zZ_ \-+0-9]", replacement = "")
    let private toLower (s:string) = s.ToLower()
    let private takeAlphaNumFirstChars seq = seq |> Seq.map (Seq.head >> tryAlphanum) |> Seq.choose id |> Seq.map string |> String.concat ""

    /// Makes a unique key based on the node type. Some nodes have a unique identifier, while
    /// others do not.
    let makeUniqueKey (nodeData:Node) : Graph.UniqueKey =
        let friendlyKey t = Graph.FriendlyKey(nodeData.NodeType() |> toLower, t)
        let guidKey g = Graph.UUID(nodeData.NodeType() |> toLower, g)
        match nodeData with
        | PopulationNode p ->
            match p with
            | BioticProxyNode n ->
                match n with
                | Population.BioticProxies.BioticProxyNode.AncientDNA a -> sprintf "aDNA_%s" (safeString a.Value) |> toLower |> friendlyKey
                | Population.BioticProxies.BioticProxyNode.ContemporaneousWholeOrganism taxon -> sprintf "direct_%s" (safeString taxon.Value) |> toLower |> friendlyKey
                | Population.BioticProxies.BioticProxyNode.Morphotype m -> 
                    match m with
                    | Population.BioticProxies.Megafossil (part,f) -> sprintf "morphotype_megafossil_%s_%s" (safeString f.Value) (safeString part.Value) |> toLower |> friendlyKey
                    | Population.BioticProxies.Macrofossil (part,f) -> sprintf "morphotype_macrofossil_%s_%s" (safeString f.Value) (safeString part.Value) |> toLower |> friendlyKey
                    | Population.BioticProxies.Microfossil (group, name) ->
                        match group with
                        | Population.BioticProxies.MicrofossilGroup.Diatom -> sprintf "morphotype_diatom_%s" (safeString name.Value) |> toLower |> friendlyKey
                        | Population.BioticProxies.MicrofossilGroup.Ostracod -> sprintf "morphotype_ostracod_%s" (safeString name.Value) |> toLower |> friendlyKey
                        | Population.BioticProxies.MicrofossilGroup.PlantMacrofossil -> sprintf "morphotype_plantmacrofossil_%s" (safeString name.Value) |> toLower |> friendlyKey
                        | Population.BioticProxies.MicrofossilGroup.Pollen -> sprintf "morphotype_pollen_%s" (safeString name.Value) |> toLower |> friendlyKey
                        | Population.BioticProxies.MicrofossilGroup.OtherMicrofossilGroup group -> sprintf "morphotype_customgroup_%s_%s" (safeString group.Value) (safeString name.Value) |> toLower |> friendlyKey
            | BioticProxyCategoryNode n ->
                match n with
                | BioticProxies.BioticProxyCategoryNode.AncientDNA g -> sprintf "aDNA_%s" (safeString <| g.ToString()) |> toLower |> friendlyKey
                | BioticProxies.BioticProxyCategoryNode.Contemporary g -> sprintf "contemporary_%s" (safeString <| g.ToString()) |> toLower |> friendlyKey
                | BioticProxies.BioticProxyCategoryNode.Fossil g -> sprintf "fossil_%s" (safeString <| g.ToString()) |> toLower |> friendlyKey
                | BioticProxies.BioticProxyCategoryNode.OtherProxy p -> sprintf "fossil_%s" (safeString <| p.Value) |> toLower |> friendlyKey 
                | BioticProxies.BioticProxyCategoryNode.Microfossil m ->
                    match m with
                    | BioticProxies.MicrofossilGroup.Diatom -> "fossilmicro_diatom" |> toLower |> friendlyKey
                    | BioticProxies.MicrofossilGroup.Ostracod -> "fossilmicro_ostracod" |> toLower |> friendlyKey
                    | BioticProxies.MicrofossilGroup.PlantMacrofossil -> "fossilmicro_plantmacrofossil" |> toLower |> friendlyKey
                    | BioticProxies.MicrofossilGroup.Pollen -> "fossilmicro_pollen" |> toLower |> friendlyKey
                    | BioticProxies.MicrofossilGroup.OtherMicrofossilGroup group -> sprintf "fossilmicro_other_%s" (safeString group.Value) |> toLower |> friendlyKey
            | TaxonomyNode n ->
                match n with
                | Taxonomy.TaxonNode.Life -> "life" |> toLower |> friendlyKey
                | Taxonomy.TaxonNode.Kingdom l -> sprintf "kingdom_%s" (safeString l.Value) |> toLower |> friendlyKey
                | Taxonomy.TaxonNode.Phylum l -> sprintf "phylum_%s" (safeString l.Value) |> toLower |> friendlyKey
                | Taxonomy.TaxonNode.Class l -> sprintf "class_%s" (safeString l.Value) |> toLower |> friendlyKey
                | Taxonomy.TaxonNode.Clade l -> sprintf "clade_%s" (safeString l.Value) |> toLower |> friendlyKey
                | Taxonomy.TaxonNode.Order l -> sprintf "order_%s" (safeString l.Value) |> toLower |> friendlyKey
                | Taxonomy.TaxonNode.Family l -> sprintf "family_%s" (safeString l.Value) |> toLower |> friendlyKey
                | Taxonomy.TaxonNode.Subfamily l -> sprintf "subfamily_%s" (safeString l.Value) |> toLower |> friendlyKey
                | Taxonomy.TaxonNode.Tribe l -> sprintf "tribe_%s" (safeString l.Value) |> toLower |> friendlyKey
                | Taxonomy.TaxonNode.Subtribe l -> sprintf "subtribe_%s" (safeString l.Value) |> toLower |> friendlyKey
                | Taxonomy.TaxonNode.Genus l -> sprintf "genus_%s" (safeString l.Value) |> toLower |> friendlyKey
                | Taxonomy.TaxonNode.Subgenus l -> sprintf "subgenus_%s" (safeString l.Value) |> toLower |> friendlyKey
                | Taxonomy.TaxonNode.Species (l,l2,l3) -> sprintf "species_%s_%s_%s" (safeString l.Value) (safeString l2.Value) (safeString(l3.Value)) |> toLower |> friendlyKey
                | Taxonomy.TaxonNode.Subspecies (l,l2,l3, l4) -> sprintf "subspecies_%s_%s_%s_%s" (safeString l.Value) (safeString l2.Value) (safeString l3.Value) (System.Net.WebUtility.HtmlEncode(l4.Value)) |> toLower |> friendlyKey
                | Taxonomy.TaxonNode.Variety (l,l2,l3, l4) -> sprintf "variety_%s_%s_%s_%s" (safeString l.Value) (safeString l2.Value) (safeString l3.Value) (System.Net.WebUtility.HtmlEncode(l4.Value)) |> toLower |> friendlyKey
            | InferenceMethodNode n ->
                match n with
                | BioticProxies.InferenceMethodNode.Implicit -> "Implicit" |> toLower |> friendlyKey
                | BioticProxies.InferenceMethodNode.IdentificationKeyOrAtlas r -> sprintf "atlas_%s" ((r.Value.Split(" ") |> Seq.map (Seq.head >> tryAlphanum) |> Seq.choose id |> Seq.map string |> String.concat "") |> safeString) |> toLower |> friendlyKey
                | BioticProxies.InferenceMethodNode.ImplicitByExpert (l,i) -> sprintf "expert_%s_%s" (safeString l.Value) (safeString i.Value) |> toLower |> friendlyKey
            | ProxiedTaxonNode -> guidKey (System.Guid.NewGuid())
            | ContextNode _ -> guidKey (System.Guid.NewGuid())
            | VernacularTaxonLabelNode v -> sprintf "%s_%s" (safeString v.Language.Value) (safeString v.Label.Value) |> toLower |> friendlyKey
        | SourceNode s ->
            match s with
            | Unscreened s
            | Included (s,_)
            | Excluded (s,_,_) ->
                match s with
                | Bibliographic n -> 
                    String.concat "_" [
                        "pub"
                        (if n.Author.IsSome then n.Author.Value.Value.Split(",").[0] else "unknown")
                        (if n.Title.IsSome then 
                            (n.Title.Value.Value.Split(" ") |> Seq.map (Seq.head >> tryAlphanum) |> Seq.choose id |> Seq.map string |> String.concat "")
                            else "notitle")
                        (if n.Year.IsSome then string n.Year.Value else "noyear") ] |> toLower |> friendlyKey
                | GreyLiterature n -> 
                    sprintf "grey_%s_%s_%s"
                        n.Contact.LastName.Value
                        (n.Contact.FirstName.Value.Split(" ") |> Seq.map (Seq.head >> string) |> String.concat "")
                        (n.Title.Value.Split(" ") |> Seq.map (Seq.head >> string) |> String.concat "") |> toLower |> friendlyKey
                | DarkData n -> sprintf "darkdata_%s_%s_%s" (safeString n.Contact.LastName.Value) (safeString n.Contact.FirstName.Value) ((n.Details.Value.Split(" ") |> Seq.map (Seq.head >> tryAlphanum) |> Seq.choose id |> Seq.map string |> Seq.truncate 40 |> String.concat "")) |> toLower |> friendlyKey
                | Database n -> sprintf "database_%s" (safeString n.Abbreviation.Value) |> toLower |> friendlyKey
                | DatabaseEntry n -> sprintf "database_%s_entry_%s" (safeString n.DatabaseAbbreviation.Value) (safeString n.UniqueIdentifierInDatabase.Value) |> toLower |> friendlyKey
                | PublishedSource n ->
                    match n with
                    | PublishedSource.Book n ->
                        String.concat "_" [
                            "book"
                            n.BookFirstAuthor.Value.LastName
                            (n.BookAdditionalAuthors |> List.map(fun c -> c.Value.LastName) |> takeAlphaNumFirstChars)
                            (n.BookTitle.Value.Split(" ") |> takeAlphaNumFirstChars)
                            string n.BookCopyrightYear
                        ] |> toLower |> friendlyKey
                    | PublishedSource.BookChapter n ->
                        String.concat "_" [
                            "book-chapter"
                            n.ChapterFirstAuthor.Value.LastName
                            (n.ChapterAdditionalAuthors |> List.map(fun c -> c.Value.LastName) |> takeAlphaNumFirstChars)
                            (n.ChapterTitle.Value.Split(" ") |> takeAlphaNumFirstChars)
                            string n.FirstPage
                        ] |> toLower |> friendlyKey
                    | PublishedSource.IndividualDataset n ->
                        String.concat "_" [
                            "published-dataset"
                            (if n.Contributors.IsEmpty then "unk" else n.Contributors.Head.Value.LastName)
                            (if n.Contributors.IsEmpty then "unk" else n.Contributors.Head.Value.Initials |> List.map string |> String.concat "")
                            (if n.Contributors.Length > 1 then n.Contributors.Tail |> List.map(fun c -> c.Value.LastName) |> takeAlphaNumFirstChars else "single")
                            (if n.YearPublished.IsNone then "unk" else string n.YearPublished.Value)
                            (n.Title.Value.Split(" ") |> takeAlphaNumFirstChars)
                        ] |> toLower |> friendlyKey
                    | PublishedSource.Dissertation n ->
                        String.concat "_" [
                            "dissertation"
                            n.Author.Value.LastName
                            n.Author.Value.Initials |> List.map string |> String.concat ""
                            n.CompletionYear |> string
                            (n.Title.Value.Split(" ") |> takeAlphaNumFirstChars)
                        ] |> toLower |> friendlyKey
                    | PublishedSource.JournalArticle n ->
                        String.concat "_" [
                            "pub"
                            n.FirstAuthor.Value.LastName
                            (n.Title.Value.Split(" ") |> takeAlphaNumFirstChars)
                            string n.Year ] |> toLower |> friendlyKey
                | GreyLiteratureSource n ->
                    sprintf "greylit_%s_%s_%s_%s_%s_%s"
                        (n.Format.ToString() |> toLower |> safeString)
                        (if n.Contributors.IsEmpty then "unk" else n.Contributors.Head.Value.LastName)
                        (if n.Contributors.IsEmpty then "unk" else n.Contributors.Head.Value.Initials |> List.map string |> String.concat "")
                        (if n.Contributors.Length > 1 then n.Contributors.Tail |> List.map(fun c -> c.Value.LastName) |> takeAlphaNumFirstChars else "single")
                        (if n.PostedYear.IsNone then "unk" else string n.PostedYear.Value)
                        (n.Title.Value.Split(" ") |> takeAlphaNumFirstChars)
                    |> toLower |> friendlyKey
                | DarkDataSource n ->
                    sprintf "darkdata_%s_%s_%s_%s_%s"
                        n.Investigator.Value.LastName
                        (n.Investigator.Value.Initials |> List.map string |> String.concat "")
                        (n.AdditionalInvestigators |> List.map(fun c -> c.Value.LastName) |> takeAlphaNumFirstChars)
                        (if n.CompilationYear.IsNone then "unk" else string n.CompilationYear.Value)
                        (n.Details.Value.Split(" ") |> takeAlphaNumFirstChars)
                    |> toLower |> friendlyKey
        | ExposureNode e ->
            match e with
            | YearNode y -> sprintf "%iybp" y.Year |> toLower |> friendlyKey
            | SliceLabelNode n -> sprintf "%s_by_%s" (safeString n.Name.Value) (safeString n.DesignatingAuthority.Value) |> toLower |> friendlyKey
            | TimelineNode _ -> guidKey (System.Guid.NewGuid())
            | DateNode _ -> guidKey (System.Guid.NewGuid())
            | OutOfScopeNode n -> sprintf "%s_by_%s" (safeString n.Name.Value) (safeString n.DesignatingAuthority.Value) |> toLower |> friendlyKey
        | OutcomeNode o ->
            match o with
            | MeasureNode n ->
                match n with
                | Biodiversity.BiodiversityDimensionNode.Abundance -> "abundance" |> toLower |> friendlyKey
                | Biodiversity.BiodiversityDimensionNode.DiversityBeta -> "beta_diversity" |> toLower |> friendlyKey
                | Biodiversity.BiodiversityDimensionNode.Evenness -> "evenness" |> toLower |> friendlyKey
                | Biodiversity.BiodiversityDimensionNode.PresenceOnly -> "presence" |> toLower |> friendlyKey
                | Biodiversity.BiodiversityDimensionNode.PresenceAbsence -> "presence_absence" |> toLower |> friendlyKey
                | Biodiversity.BiodiversityDimensionNode.Richness -> "richness" |> toLower |> friendlyKey
                | Biodiversity.BiodiversityDimensionNode.OtherBiodiversityDimension o -> sprintf "custom_%s" (safeString o.Value) |> toLower |> friendlyKey


    /// Functions to tryFind specific node types based on their
    /// inherent indexes or other conditions.
    module Nodes =

        let asPopnNode = function | PopulationNode n -> Some n | _ -> None
        let asExposureNode = function | ExposureNode n -> Some n | _ -> None
        let asSourceNode = function | SourceNode n -> Some n | _ -> None
        let asOutcomeNode = function | OutcomeNode n -> Some n | _ -> None

        let internal whereTaxon' cond = function
            | TaxonomyNode t -> if cond t then Some t else None
            | _ -> None

        let internal whereBioticProxy' cond = function
            | BioticProxyNode t -> if cond t then Some t else None
            | _ -> None

        let internal whereInferenceMethod' cond = function
            | InferenceMethodNode t -> if cond t then Some t else None
            | _ -> None

        let internal whereTime' label = function
            | SliceLabelNode y -> if y.Name = label then Some y else None
            | _ -> None

        let internal whereYear' year = function
            | YearNode y -> if y.Year = year then Some y else None
            | _ -> None

        let internal tryFind nodeType cond2 cond graph = 
            graph
            |> Graph.tryFind (fun (d,_) -> snd d |> nodeType |> Option.bind(fun x -> cond2 cond x) |> Option.isSome)
            |> Option.map(fun ((i,d),adj) -> ((i, (d |> nodeType |> Option.get |> cond2 cond |> Option.get)), adj))
        
        let tryFindTaxon cond graph = tryFind asPopnNode whereTaxon' cond graph
        let tryFindProxy cond graph = tryFind asPopnNode whereBioticProxy' cond graph
        let tryFindInferenceMethod cond graph = tryFind asPopnNode whereInferenceMethod' cond graph
        let tryFindTimePeriod label graph = tryFind asExposureNode whereTime' label graph
        let tryFindYear year graph = tryFind asExposureNode whereYear' year graph

        let tryMakeNode t (n:obj) =
            try
                match t with
                // Source nodes:
                | (t: System.Type) when t = typeof<Sources.SourceNode> -> n :?> Sources.SourceNode |> SourceNode |> Ok
                | (t: System.Type) when t = typeof<Sources.Source> -> n :?> Sources.Source |> Unscreened |> SourceNode |> Ok
                // Population nodes:
                | (t: System.Type) when t = typeof<BioticProxies.BioticProxyNode> -> n :?> BioticProxies.BioticProxyNode |> BioticProxyNode |> PopulationNode |> Ok
                | (t: System.Type) when t = typeof<BioticProxies.BioticProxyCategoryNode> -> n :?> BioticProxies.BioticProxyCategoryNode |> BioticProxyCategoryNode |> PopulationNode |> Ok
                | (t: System.Type) when t = typeof<Taxonomy.TaxonNode> -> n :?> Taxonomy.TaxonNode |> TaxonomyNode |> PopulationNode |> Ok
                | (t: System.Type) when t = typeof<BioticProxies.InferenceMethodNode> -> n :?> BioticProxies.InferenceMethodNode |> InferenceMethodNode |> PopulationNode |> Ok
                | (t: System.Type) when t = typeof<Population.Taxonomy.VernacularTaxonLabelNode> -> n :?> Population.Taxonomy.VernacularTaxonLabelNode |> VernacularTaxonLabelNode |> PopulationNode |> Ok
                | t when t = typeof<Population.Context.ContextNode> -> n :?> Population.Context.ContextNode |> ContextNode |> PopulationNode |> Ok
                // Exposure node:
                | (t: System.Type) when t = typeof<TemporalIndex.CalYearNode> -> n :?> TemporalIndex.CalYearNode |> YearNode |> ExposureNode |> Ok
                | (t: System.Type) when t = typeof<TemporalIndex.QualitativeLabelNode> -> n :?> TemporalIndex.QualitativeLabelNode |> SliceLabelNode |> ExposureNode |> Ok
                | (t: System.Type) when t = typeof<StudyTimeline.IndividualTimelineNode> -> n :?> StudyTimeline.IndividualTimelineNode |> TimelineNode |> ExposureNode |> Ok
                | (t: System.Type) when t = typeof<StudyTimeline.IndividualDateNode> -> n :?> StudyTimeline.IndividualDateNode |> DateNode |> ExposureNode |> Ok
                // Outcome node:
                | (t: System.Type) when t = typeof<Outcomes.Biodiversity.BiodiversityDimensionNode> -> n :?> Biodiversity.BiodiversityDimensionNode |> MeasureNode |> OutcomeNode |> Ok
                | _ -> Error <| sprintf "Not a known node type (%s)" t.Name
            with | e -> Error <| sprintf "Failed to make a node: %s" e.Message

    module Relations =

        type ValidRelation<'TData> = private ValidRelation of Graph.UniqueKey * Graph.UniqueKey * 'TData
        let private unwrap (ValidRelation (a,b,c)) = a,b,c

        /// Helpers to read F# cases
        module Cases =

            open FSharp.Reflection

            let getUnionCaseName (x:'a) = 
                match FSharpValue.GetUnionFields(x, typeof<'a>) with
                | case, _ -> case.Name  

            /// Use an equivalent 'NodeRelation' discriminated union to lookup
            /// the source and sink type constraints for a given relation.
            /// Returns true if the case parameter at the specified index is
            /// of the same type as 'node.
            let compareTypes (node:System.Guid * 'node) rel paramIndex = 
                printfn "type is %A" (rel.GetType().FullName)
                let lookupType = 
                    rel.GetType().FullName.Replace("Relation", "NodeRelation")
                    |> System.Type.GetType
                printfn "type2 is %A" lookupType
                let caseName = getUnionCaseName rel
                printfn "case is %A" caseName
                let constraintCase =
                    Reflection.FSharpType.GetUnionCases(lookupType)
                    |> Seq.tryFind (fun uc -> uc.Name = caseName)
                printfn "constraint is %A" constraintCase
                match constraintCase with
                | Some (c: UnionCaseInfo) -> 
                    let field: System.Reflection.PropertyInfo = c.GetFields().[paramIndex]
                    let nodeType =
                        let baseType = (snd node).GetType().BaseType
                        if baseType = typeof<System.Object> then (snd node).GetType() else baseType
                    field.PropertyType = nodeType
                | None -> false
                
        let compare source sink rel ifTrue =
            // TODO re-enable constraints when figured out how to unwrap to node type from Node.
            // if Cases.compareTypes source rel 0 && Cases.compareTypes sink rel 1
            // then Ok <| ValidRelation(fst source, fst sink, ifTrue)
            // else Error "Node didn't match"
            Ok <| ValidRelation(fst source, fst sink, ifTrue)

        /// Makes a master relation from defined relation DU types.
        /// This effectively constrains relations to a specific
        /// node type combination. May also implement additional graph
        /// validation constraints (beyond type signature) here.
        let makeRelation source sink (rel:ProposedRelation) : Result<ValidRelation<Relation>,string> =
            match rel with
            | Exposure rel ->
                match rel with
                | EarliestTime -> compare source sink rel (Relation.Exposure EarliestTime)
                | Next -> compare source sink rel (Relation.Exposure Next)
                | Contains -> compare source sink rel (Relation.Exposure Contains)
                | LatestTime -> compare source sink rel (Relation.Exposure LatestTime)
                | TimeEstimate t -> compare source sink rel (Relation.Exposure <| TimeEstimate t)
                | OccursWithin -> compare source sink rel (Relation.Exposure <| OccursWithin)
                | UncertaintyOldest t -> compare source sink rel (Relation.Exposure <| UncertaintyOldest t)
                | UncertaintyYoungest t -> compare source sink rel (Relation.Exposure <| UncertaintyYoungest t)
                | ExtentEarliest -> compare source sink rel (Relation.Exposure ExtentEarliest)
                | ExtentEarliestUncertainty -> compare source sink rel (Relation.Exposure ExtentEarliestUncertainty)
                | ExtentLatest -> compare source sink rel (Relation.Exposure ExtentLatest)
                | ExtentLatestUncertainty -> compare source sink rel (Relation.Exposure ExtentLatestUncertainty)
                | ExtentEarliestOutOfScope t -> compare source sink rel (Relation.Exposure <| ExtentEarliestOutOfScope t)
                | IntersectsTime -> compare source sink rel (Relation.Exposure IntersectsTime)
                | HasProxyInfo -> compare source sink rel (Relation.Exposure HasProxyInfo)
                | HasProxyCategory -> compare source sink rel (Relation.Exposure HasProxyCategory)
                | HasOrphanProxy -> compare source sink rel (Relation.Exposure HasOrphanProxy)
                | IsLocatedAt -> compare source sink rel (Relation.Exposure IsLocatedAt)
                | ConstructedWithDate -> compare source sink rel (Relation.Exposure ConstructedWithDate)
            | Population rel ->
                match rel with
                | InferredFrom -> compare source sink rel (Relation.Population InferredFrom)
                | IsA -> compare source sink rel (Relation.Population IsA)
                | HasLabel -> compare source sink rel (Relation.Population HasLabel)
                | InferredUsing -> compare source sink rel (Relation.Population InferredUsing)
                | InferredAs -> compare source sink rel (Relation.Population InferredAs)
                | MeasuredBy -> compare source sink rel (Relation.Population MeasuredBy)
              | Source rel ->
                match rel with
                | HasTemporalExtent -> compare source sink rel (Relation.Source HasTemporalExtent)
                | UsesPrimarySource -> compare source sink rel (Relation.Source UsesPrimarySource)
                | UsedDatabase(accessDate, subset) -> compare source sink rel (Relation.Source <| UsedDatabase(accessDate,subset))
                | HasDataset -> compare source sink rel (Relation.Source HasDataset)
                | IsChapterIn -> compare source sink rel (Relation.Source IsChapterIn)

        /// Add a node relation to the graph, validating that the relation
        /// can only occur on valid node sources / sinks in the process.
        /// Will not add relation when same source * sink * relation is specified.
        let addRelation (atom1:Graph.Atom<'data,'conn>) (atom2:Graph.Atom<'data2,'conn2>) rel weight (graph:Graph.Graph<'a,Relation>) =
            result {
                let! validated = makeRelation (fst atom1) (fst atom2) rel
                let sourceId,sinkId,data = unwrap validated
                return! Graph.addRelation sourceId sinkId weight data graph
            }

        /// Adds a 'proxied taxon' intermediary node to the graph. This node represents
        /// an 'occurrence' of a combination of inference method, biotic proxy, and taxon
        /// that are only valid for one time and study.
        let addProxiedTaxon (edge:ProxiedTaxon.ProxiedTaxonHyperEdge) graph = result {
            // Get the three included nodes:
            let! existingProxy = graph |> Nodes.tryFindProxy (fun n -> n = edge.InferredFrom), "proxy doesn't exist"
            let! existingTaxon = graph |> Nodes.tryFindTaxon (fun n -> n = edge.InferredAs), "taxon doesn't exist"
            let! existingInfer = graph |> Nodes.tryFindInferenceMethod (fun n -> n = edge.InferredUsing), "infer doesn't exist"
            let! proxiedGraph = Graph.addNodeData makeUniqueKey [PopulationNode ProxiedTaxonNode] graph
            let! proxiedTaxon = Graph.getAtom (snd proxiedGraph |> List.last |> fst) (fst proxiedGraph), "no intermediate node"
            // Add relations that make the intermediate node encode the hyper-edge:
            return!
                proxiedGraph |> fst
                |> addRelation proxiedTaxon existingProxy (Population InferredFrom) 1
                |> Result.bind (addRelation proxiedTaxon existingProxy (Population InferredFrom) 1)
                |> Result.bind (addRelation proxiedTaxon existingInfer (Population InferredUsing) 1)
                |> Result.bind (addRelation proxiedTaxon existingTaxon (Population InferredAs) 1)
                |> Result.lift(fun g -> g, proxiedTaxon |> fst |> fst)
        }

        /// Get the ID of sink nodes for a specific relation.
        let nodeIdsByRelation<'a> (relationCase:obj) (atom:Graph.Atom<Node,Relation>) =
            atom |> snd |> Seq.choose(fun (source,sink,_,r) ->
                match r with
                | Relation.Exposure e -> 
                    if typeof<'a> = typeof<ExposureRelation> then
                        if e = (relationCase :?> ExposureRelation) then Some sink else None
                    else None
                | Relation.Source s -> 
                    if typeof<'a> = typeof<SourceRelation> then
                        if s = (relationCase :?> SourceRelation) then Some sink else None
                    else None
                | Relation.Population p ->
                    if typeof<'a> = typeof<PopulationRelation> then
                        if p = (relationCase :?> PopulationRelation) then Some sink else None
                    else None
            )