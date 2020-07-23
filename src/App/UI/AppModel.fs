module UI.AppModel
open Myriadic
open Domain.Model
open Fable.React

type DialogFactory = unit -> ReactElement
[<Generator.Lenses>]
type Model = {
    chargen: UI.Chargen.State
    roster: Domain.Model.Creature list
    error: string option
    currentCreatureIndex: int option
    modalDialog: DialogFactory option
    }

and Msg =
    | Update of transform: (Model -> Model)


