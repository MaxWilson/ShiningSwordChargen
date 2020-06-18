module UI.App

open UI
open Elmish
open Fable.SimpleHttp
open Thoth.Json
open Feliz
open Feliz.Bulma

let init _ = { stats = None }, Cmd.Empty
let update msg model =
    match msg with
    | Update t ->
        (t model), Cmd.Empty

let view model dispatch =
    Bulma.section [
        prop.className "content"
        prop.children [
            Bulma.content [
                UI.Chargen.view {
                        stats = UI.stats_
                        updateCmd = UI.Update >> dispatch
                    } model
                ]
            ]
        ]