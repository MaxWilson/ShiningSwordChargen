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
open AutoWizard

module Character =
    [<Generator.DuCases>]
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
    [<Generator.DuCases>]
    type Sex = Male | Female | Neither
    [<Generator.DuCases>]
    type Feat = Sharpshooter | CrossbowExpert | HeavyArmorMaster | GreatWeaponMaster
    [<Generator.DuCases>]
    type Skill = Athletics | Stealth | Perception | Insight
    [<Generator.DuCases>]
    type ElfRace = High | Wood | Drow
    [<Generator.DuCases>]
    type DwarfRace = Mountain | Hill
    [<Generator.DuCases>]
    type HumanType = Standard | Variant
    [<Generator.DuCases>]
    type Race = Human of HumanType * Skill * Feat * (Stat * Stat) | Elf of ElfRace | Dwarf of DwarfRace | Halforc | Goblin

    [<Generator.DuCases>]
    type Class = Barbarian | Fighter | Monk | Rogue
    [<Generator.DuCases>]
    type FightingStyle = Dueling | Archery | Defense | GreatWeaponFighting
    [<Generator.DuCases>]
    type ClassAbility = ASI of (Stat * Stat) | Feat of Feat | FightingStyle of FightingStyle
open Character

[<Generator.Lenses>]
type StatBlock = {
    stats: Stats
    hp: int
    ac: int
    }


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

module Draft =
    [<Generator.Lenses>]
    type CharacterSheet = {
        unmodifiedStats: Stats
        name: string
        sex: Setting<Sex>
        race: Setting<Race>
        xp: int
        allocatedLevels: Class list // advancement priorities, e.g. [Fighter; Fighter; Fighter; Fighter; Rogue; Fighter; Rogue]
        classAbilities: Setting<ClassAbility> list
        }
