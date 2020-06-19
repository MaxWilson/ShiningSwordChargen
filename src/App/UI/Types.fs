module UI
open Myriadic
open Domain.Model

[<Generator.Lenses>]
type UI = {
    chargen: UI.Chargen.State
    }

type Msg =
    | Update of transform: (UI -> UI)


