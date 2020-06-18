module App

open Elmish
open Fable.SimpleHttp
open Thoth.Json
open Feliz

type UI = {
    txt: string
    }
type Msg =
    | Update of transform: (UI -> UI)

let init _ = { txt = "" }, Cmd.Empty
let update msg model =
    match msg with
    | Update t ->
        (t model), Cmd.Empty

let view model dispatch =
    Html.div [
        prop.className "content"
        prop.children [
            Html.h1 "Hello"
            Html.div "world"
            ]
        ]