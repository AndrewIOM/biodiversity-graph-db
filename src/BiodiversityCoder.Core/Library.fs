namespace BiodiversityCoder.Core

open Elmish
open Bolero
open Bolero.Html

module GraphVisualisation =

    open Cyjs.NET
    open Cyjs.NET.Elements

    let nodes (graph:Graph.Graph<GraphStructure.Node,GraphStructure.Relation>) =
        graph |> Seq.map(fun atom ->
            let label = (atom |> fst |> snd).ToString()
            node (atom |> Graph.getAtomId |> string) [ CyParam.label label ] )

    let relations (graph:Graph.Graph<GraphStructure.Node,GraphStructure.Relation>) =
        graph |> Seq.collect(fun atom ->
            atom |> snd |> Seq.map(fun (source,sink,_,_) -> source, sink))
        |> Seq.mapi(fun i (source,sink) -> edge (string i) (string source) (string sink) [])

    let view graph =
        CyGraph.initEmpty ()
        |> CyGraph.withElements (nodes graph)
        |> CyGraph.withElements (relations graph)
        |> CyGraph.withStyle "node"     
                [
                    CyParam.content =. CyParam.label
                    CyParam.color "#A00975"
                ]
        |> CyGraph.withSize(800, 400)
        |> HTML.toEmbeddedHTML


module App =

    type Model =
        {
            Graph: Graph.Graph<GraphStructure.Node,GraphStructure.Relation>
        }

    let initModel =
        match Seed.initGraph() with
        | Ok g ->  { Graph = g }
        | Error e -> failwithf "Error making graph: %s" e

    type Message =
        | Ping

    let update message model =
        match message with
        | Ping -> model

    let view model dispatch =
        div [] [
            RawHtml (GraphVisualisation.view model.Graph)
        ]

type MainApp() =
    inherit ProgramComponent<App.Model, App.Message>()

    override this.Program =
        Program.mkSimple (fun _ -> App.initModel) App.update App.view