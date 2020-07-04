﻿module Domain.Model
#if INTERACTIVE
module Generator =
    type LensesAttribute() =
        inherit System.Attribute()
    type DuCasesAttribute() =
        inherit System.Attribute()
#else
open Myriadic
open Myriad.Plugins
#endif

type Stat = Str | Dex | Con | Int | Wis | Cha

[<Generator.Lenses>]
type Stats = {
    str: int
    dex: int
    con: int
    int: int
    wis: int
    cha: int
    }

[<Generator.Lenses>]
type StatBlock = {
    stats: Stats
    hp: int
    ac: int
    }

[<Generator.DuCases>]
type Sex = Male | Female | Neither

[<Generator.Lenses>]
type CharSheet = {
    statBlock: StatBlock
    xp: int
    yearOfBirth: int
    sex: Sex
    }

type StatSource = StatBlock of StatBlock | CharSheet of CharSheet

[<Generator.Lenses>]
type Creature = {
    name: string
    stats: StatSource
    }

[<Generator.DuCases>]
type Race = Human | Elf | Dwarf