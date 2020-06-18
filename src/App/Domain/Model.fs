module Model
open Myriadic
open Myriad.Plugins
[<Generator.Lenses>]
type Stats = {
    str: int
    dex: int
    con: int
    int: int
    wis: int
    cha: int
    }

[<Generator.DuCases>]
type Race = Human | Elf | Dwarf