module UI.App

open UI.Types
open Elmish
open Fable.SimpleHttp
open Thoth.Json
open Feliz
open Feliz.Bulma

let init _ = { chargen = Chargen.State.fresh; roster = []; error = None; currentCreatureIndex = None }, Cmd.Empty
let update msg model =
    match msg with
    | Update t ->
        try
            (t model), Cmd.Empty
        with | exn ->
            { model with error = "Error during update: " + exn.ToString() |> Some }, Cmd.Empty

let view model dispatch =
    match model.error with
    | Some err ->
        Bulma.section [
            Bulma.title.h1 "Something went wrong"
            Bulma.title.h3 "Catastrophic error"
            Html.div err
            ]
    | None ->
        try
            Bulma.section [
                prop.className "content"
                prop.children [
                    Bulma.content [
                        UI.Chargen.view {
                                chargen_ = Model.chargen_
                                roster_ = Model.roster_
                                currentIndex_ = Model.currentCreatureIndex_
                                updateCmd = Update >> dispatch
                            } model
                        ]
                    ]
                ]
        with | exn ->
            Bulma.section [
                Bulma.title.h1 "Something went wrong"
                Bulma.title.h3 "Catastrophic error during rendering"
                Html.div (exn.ToString())
                Html.button [
                    prop.text "Refresh"
                    prop.onClick (fun _ -> Update (Optics.Operations.over Model.currentCreatureIndex_ (Option.map (fun ix -> ix + 1 % (model.roster.Length)))) |> dispatch)
                    ]
                ]