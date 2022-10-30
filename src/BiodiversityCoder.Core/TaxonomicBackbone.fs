namespace BiodiversityCoder.Core

open System

module TaxonomicBackbone =

    module GlobalPollenProject =
        
        open System.Web
        open System.Net.Http
        open FieldDataTypes
        open Population

        /// A taxon result from the global pollen project's lookup APIs.
        type BackboneSearchRequest = {
            Id: string
            Group: string
            Family: string
            Genus: string
            Species: string
            FamilyId: string
            GenusId: string
            SpeciesId: string
            NamedBy: string
            LatinName: string
            Rank: string
            ReferenceName: string
            ReferenceUrl: string
            TaxonomicStatus: string
            TaxonomicAlias: string
        }

        let lookup rank family genus species (authorship:string) =

            let query =
                match rank with
                | "Family" -> sprintf "rank=Family&family=%s&latinname=%s" family family |> Ok
                | "Genus" -> sprintf "rank=Genus&family=%s&genus=%s&latinname=%s" family genus genus |> Ok
                | "Species" -> sprintf "rank=Species&family=%s&genus=%s&species=%s&latinname=%s %s&authorship=%s" family genus species genus species (HttpUtility.UrlEncode(authorship)) |> Ok
                | _ -> Error "An invalid rank was specified"

            let client = new HttpClient()

            query
            |> Result.bind(fun q -> 
                try
                    let result = 
                        client.GetStringAsync(sprintf "https://globalpollenproject.org/api/v1/backbone/trace?%s" q)
                        |> Async.AwaitTask 
                        |> Async.RunSynchronously
                    match Microsoft.FSharpLu.Json.BackwardCompatible.tryDeserialize<BackboneSearchRequest[]> result with
                    | Choice.Choice1Of2 s -> Ok s
                    | Choice.Choice2Of2 exn -> Error <| sprintf "Could not decode the response from the Global Pollen Project's taxonomic backbone. %s" exn
                with | e -> Error e.Message
            )

        let plantae = 
            Text.createShort "Plantae"
            |> Result.lift Taxonomy.TaxonNode.Kingdom

        let lookupAsNodesAndRelations rank family genus species authorship =
            lookup rank family genus species authorship
            |> Result.bind(fun r -> 
                if r.Length = 1 && r.[0].TaxonomicStatus = "accepted" then Ok r.[0]
                else if r.Length = 0 then Error "Could not understand the response from the taxonomic backbone"
                else r |> Seq.tryFind(fun r -> r.TaxonomicStatus = "accepted") |> Result.ofOption "No accepted names"
            )
            |> Result.bind(fun vm ->
                match vm.Rank with
                | "Family" -> 
                    let family = Taxonomy.TaxonNode.Family <!> (Text.createShort vm.LatinName)
                    (fun f plantae -> f, [ PopulationNodeRelation.IsA(f,plantae); PopulationNodeRelation.IsA(plantae, Taxonomy.TaxonNode.Life)]) <!> family <*> plantae
                | "Genus" -> 
                    let genus = (Taxonomy.TaxonNode.Genus <!> (Text.createShort vm.LatinName))
                    let family = 
                        Taxonomy.TaxonNode.Family
                        <!> (if not (String.IsNullOrEmpty(vm.Family)) then Text.createShort vm.Family else Error "No family returned by taxonomic backbone")
                    let relations =
                        (fun plantae f g -> [PopulationNodeRelation.IsA(g,f); PopulationNodeRelation.IsA(f,plantae); PopulationNodeRelation.IsA(plantae, Taxonomy.TaxonNode.Life)])
                        <!> plantae
                        <*> family
                        <*> genus
                    (fun g relations -> g, relations) <!> genus <*> relations
                | "Species" ->
                    let generic,specific = Text.createShort (vm.LatinName.Split(" ")[0]), Text.createShort (vm.LatinName.Split(" ")[1])
                    let auth = if (not (String.IsNullOrEmpty(vm.NamedBy))) then Text.createShort vm.NamedBy else Error "No authorship returned from taxonomic backbone"
                    let species =
                        (fun g s a -> Taxonomy.TaxonNode.Species(g,s,a))
                        <!> generic
                        <*> specific
                        <*> auth
                    let genus = 
                        Taxonomy.TaxonNode.Genus 
                        <!> (if (not (String.IsNullOrEmpty(vm.Genus))) then Text.createShort vm.Genus else Error "No genus returned by taxonomic backbone")
                    let family = 
                        Taxonomy.TaxonNode.Family 
                        <!> (if (not(String.IsNullOrEmpty(vm.Family))) then Text.createShort vm.Family else Error "No family returned by taxonomic backbone")

                    let relations = 
                        (fun plantae f g s -> [ PopulationNodeRelation.IsA(s, g); PopulationNodeRelation.IsA(g, f); PopulationNodeRelation.IsA(f,plantae); PopulationNodeRelation.IsA(plantae, Taxonomy.TaxonNode.Life) ])
                        <!> plantae
                        <*> family
                        <*> genus
                        <*> species
                    (fun s relations -> s, relations) <!> species <*> relations
                | _ -> Error "A valid rank was not returned by GPP."
            )
