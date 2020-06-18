module UI
open Myriadic
open Domain.Model

[<Generator.Lenses>]
type UI = {
    stats: Stats option
    }

type Msg =
    | Update of transform: (UI -> UI)


