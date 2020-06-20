module UI
open Myriadic
open Domain.Model

[<Generator.Lenses>]
type UI = {
    chargen: UI.Chargen.State
    roster: Domain.Model.Creature list
    error: string option
    currentCreatureIndex: int option
    }

type Msg =
    | Update of transform: (UI -> UI)


