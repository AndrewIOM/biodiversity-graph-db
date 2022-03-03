namespace BiodiversityCoder.Core

open Elmish
open Bolero
open Bolero.Html

module Test =

    type Model =
        {
            x: string
        }

    let initModel =
        {
            x = ""
        }

    type Message =
        | Ping

    let update message model =
        match message with
        | Ping -> model

    let view model dispatch =
        text "Hello, world!"

type MainApp() =
    inherit ProgramComponent<Test.Model, Test.Message>()

    override this.Program =
        Program.mkSimple (fun _ -> Test.initModel) Test.update Test.view