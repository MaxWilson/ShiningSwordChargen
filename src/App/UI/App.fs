module UI.App

open UI.Components
open UI.AppModel
open Elmish
open Fable.SimpleHttp
open Thoth.Json
open Feliz
open Optics.Operations

let init _ = { chargen = Chargen.State.fresh; roster = []; error = None; modalDialog = None }, Cmd.Empty
let update msg model =
    match msg with
    | Update t ->
        try
            (t model), Cmd.Empty
        with | exn ->
            { model with error = "Error during update: " + exn.ToString() |> Some }, Cmd.Empty


type AppDialog(dispatch) =
    interface Launcher<Model, ReactElement> with
        member this.Launch(initial: 'state, factory) =
            let update = dispatch << Update
            update(writeSome Model.modalDialog_ (
                    React.functionComponent(fun () ->
                        let state, updateState = React.useState(thunk initial)
                        factory(state, updateState, fun f -> update (f >> write Model.modalDialog_ None)))))


let view model dispatch =
    match model.error with
    | Some err ->
        Html.section [
            Html.h1 "Something went wrong"
            Html.h3 "Catastrophic error"
            Html.div err
            ]
    | None ->
        try
            match model.modalDialog with
            | Some f ->
                Html.div [
                    prop.className "modalDialog"
                    prop.children [f()]
                    ]
            | None ->
                Html.div [
                    prop.className "main"
                    prop.children [
                        UI.Chargen.view {
                            chargen_ = Model.chargen_
                            roster_ = Model.roster_
                            updateCmd = Update >> dispatch
                            modalDialog = new AppDialog(dispatch)
                        } model
                    ]
                ]
        with | exn ->
            Html.div [
                Html.h1 "Something went wrong"
                Html.h3 "Catastrophic error during rendering"
                Html.div (exn.ToString())
                Html.button [
                    prop.text "Refresh"
                    prop.onClick (fun _ -> Update id |> dispatch)
                    ]
                ]