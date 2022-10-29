namespace BiodiversityCoder.Core

    module TaxonomicBackbone =

        module GlobalPollenProject =
            
            open System.Web
            open System.Net.Http
            open FieldDataTypes
            open Population

            /// A taxon result from the global pollen project's lookup APIs.
            [<CLIMutable>]
            type BackboneSearchRequest = {
                LatinName: string
                Rank: string
                Family: string option
                Genus: string option
                Species: string option
                Authorship: string option
            }

            let lookup rank family genus species (authorship:string) =

                let query =
                    match rank with
                    | "family" -> sprintf "rank=Family&family=%s&latinname=%s" family family |> Ok
                    | "genus" -> sprintf "rank=Genus&family=%s&genus=%s&latinname=%s" family genus genus |> Ok
                    | "species" -> sprintf "rank=Species&family=%s&genus=%s&species=%s&latinname=%s %s&authorship=%s" family genus species genus species (HttpUtility.UrlEncode(authorship)) |> Ok
                    | _ -> Error "An invalid rank was specified"

                let client = new HttpClient()

                query
                |> Result.bind(fun q -> 
                    try
                        let result = 
                            client.GetStringAsync(sprintf "https://globalpollenproject.org/api/v1/backbone/trace?%s" q)
                            |> Async.AwaitTask 
                            |> Async.RunSynchronously
                        match Microsoft.FSharpLu.Json.Compact.tryDeserialize<BackboneSearchRequest> result with
                        | Choice.Choice1Of2 s -> Ok s
                        | Choice.Choice2Of2 exn -> Error exn
                    with | e -> Error e.Message
                )

            let lookupAsNodesAndRelations rank family genus species authorship =
                lookup rank family genus species authorship
                |> Result.bind(fun vm ->
                    match vm.Rank with
                    | "Family" -> 
                        let family = Taxonomy.TaxonNode.Family <!> (Text.createShort vm.LatinName)
                        family |> Result.lift(fun f -> f, [])
                    | "Genus" -> 
                        let genus = (Taxonomy.TaxonNode.Genus <!> (Text.createShort vm.LatinName))
                        let family = 
                            Taxonomy.TaxonNode.Family 
                            <!> (if vm.Family.IsSome then Text.createShort vm.Family.Value else Error "No family returned by taxonomic backbone")
                        let relations =
                            (fun f g -> [PopulationNodeRelation.IsA(g,f)])
                            <!> family
                            <*> genus
                        (fun g relations -> g, relations) <!> genus <*> relations
                    | "Species" ->
                        let generic,specific = Text.createShort (vm.LatinName.Split(" ")[0]), Text.createShort (vm.LatinName.Split(" ")[0])
                        let auth = if vm.Authorship.IsSome then Text.createShort vm.Authorship.Value else Error "No authorship returned from taxonomic backbone"
                        let species =
                            (fun g s a -> Taxonomy.TaxonNode.Species(g,s,a))
                            <!> generic
                            <*> specific
                            <*> auth
                        let genus = 
                            Taxonomy.TaxonNode.Genus 
                            <!> (if vm.Genus.IsSome then Text.createShort vm.Genus.Value else Error "No genus returned by taxonomic backbone")
                        let family = 
                            Taxonomy.TaxonNode.Family 
                            <!> (if vm.Family.IsSome then Text.createShort vm.Family.Value else Error "No family returned by taxonomic backbone")

                        let relations = 
                            (fun f g s -> [ PopulationNodeRelation.IsA(s, g); PopulationNodeRelation.IsA(g, f) ])
                            <!> family
                            <*> genus
                            <*> species
                        (fun s relations -> s, relations) <!> species <*> relations
                    | _ -> Error "A valid rank was not returned by GPP."
                )
