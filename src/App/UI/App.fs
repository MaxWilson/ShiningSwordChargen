module UI.App

open UI
open Elmish
open Fable.SimpleHttp
open Thoth.Json
open Feliz
open Feliz.Bulma

let init _ = { chargen = Chargen.State.fresh }, Cmd.Empty
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
                        state_ = UI.chargen_
                        updateCmd = UI.Update >> dispatch
                    } model
                ]
            ]
        ]