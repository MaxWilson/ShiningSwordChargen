module UI.Types
open Myriadic
open Domain.Model

[<Generator.Lenses>]
type Model = {
    chargen: UI.Chargen.State
    roster: Domain.Model.Creature list
    error: string option
    currentCreatureIndex: int option
    }

type Msg =
    | Update of transform: (Model -> Model)


