//------------------------------------------------------------------------------
//        This code was generated by myriad.
//        Changes to this file will be lost when the code is regenerated.
//------------------------------------------------------------------------------
namespace rec Domain


namespace rec Domain

module Stat =
    open Domain.Model

    let toString (x: Stat) =
        match x with
        | Str -> "Str"
        | Dex -> "Dex"
        | Con -> "Con"
        | Int -> "Int"
        | Wis -> "Wis"
        | Cha -> "Cha"

    let fromString (x: string) =
        match x with
        | "Str" -> Some Str
        | "Dex" -> Some Dex
        | "Con" -> Some Con
        | "Int" -> Some Int
        | "Wis" -> Some Wis
        | "Cha" -> Some Cha
        | _ -> None

    let toTag (x: Stat) =
        match x with
        | Str -> 0
        | Dex -> 1
        | Con -> 2
        | Int -> 3
        | Wis -> 4
        | Cha -> 5

    let isStr (x: Stat) =
        match x with
        | Str -> true
        | _ -> false

    let isDex (x: Stat) =
        match x with
        | Dex -> true
        | _ -> false

    let isCon (x: Stat) =
        match x with
        | Con -> true
        | _ -> false

    let isInt (x: Stat) =
        match x with
        | Int -> true
        | _ -> false

    let isWis (x: Stat) =
        match x with
        | Wis -> true
        | _ -> false

    let isCha (x: Stat) =
        match x with
        | Cha -> true
        | _ -> false

module Sex =
    open Domain.Model

    let toString (x: Sex) =
        match x with
        | Male -> "Male"
        | Female -> "Female"
        | Neither -> "Neither"

    let fromString (x: string) =
        match x with
        | "Male" -> Some Male
        | "Female" -> Some Female
        | "Neither" -> Some Neither
        | _ -> None

    let toTag (x: Sex) =
        match x with
        | Male -> 0
        | Female -> 1
        | Neither -> 2

    let isMale (x: Sex) =
        match x with
        | Male -> true
        | _ -> false

    let isFemale (x: Sex) =
        match x with
        | Female -> true
        | _ -> false

    let isNeither (x: Sex) =
        match x with
        | Neither -> true
        | _ -> false

module Race =
    open Domain.Model

    let toString (x: Race) =
        match x with
        | Human -> "Human"
        | Elf -> "Elf"
        | Dwarf -> "Dwarf"

    let fromString (x: string) =
        match x with
        | "Human" -> Some Human
        | "Elf" -> Some Elf
        | "Dwarf" -> Some Dwarf
        | _ -> None

    let toTag (x: Race) =
        match x with
        | Human -> 0
        | Elf -> 1
        | Dwarf -> 2

    let isHuman (x: Race) =
        match x with
        | Human -> true
        | _ -> false

    let isElf (x: Race) =
        match x with
        | Elf -> true
        | _ -> false

    let isDwarf (x: Race) =
        match x with
        | Dwarf -> true
        | _ -> false
namespace rec Domain

module Stats =
    open Domain.Model

    let str_ =
        Optics.lens (fun (data: Stats) -> data.str) (fun (value: int) (data: Stats) -> { data with str = value })

    let dex_ =
        Optics.lens (fun (data: Stats) -> data.dex) (fun (value: int) (data: Stats) -> { data with dex = value })

    let con_ =
        Optics.lens (fun (data: Stats) -> data.con) (fun (value: int) (data: Stats) -> { data with con = value })

    let int_ =
        Optics.lens (fun (data: Stats) -> data.int) (fun (value: int) (data: Stats) -> { data with int = value })

    let wis_ =
        Optics.lens (fun (data: Stats) -> data.wis) (fun (value: int) (data: Stats) -> { data with wis = value })

    let cha_ =
        Optics.lens (fun (data: Stats) -> data.cha) (fun (value: int) (data: Stats) -> { data with cha = value })

module StatBlock =
    open Domain.Model

    let stats_ =
        Optics.lens (fun (data: StatBlock) -> data.stats) (fun (value: Stats) (data: StatBlock) ->
            { data with stats = value })

    let hp_ =
        Optics.lens (fun (data: StatBlock) -> data.hp) (fun (value: int) (data: StatBlock) -> { data with hp = value })

    let ac_ =
        Optics.lens (fun (data: StatBlock) -> data.ac) (fun (value: int) (data: StatBlock) -> { data with ac = value })

module CharSheet =
    open Domain.Model

    let statBlock_ =
        Optics.lens (fun (data: CharSheet) -> data.statBlock) (fun (value: StatBlock) (data: CharSheet) ->
            { data with statBlock = value })

    let xp_ =
        Optics.lens (fun (data: CharSheet) -> data.xp) (fun (value: int) (data: CharSheet) -> { data with xp = value })

    let yearOfBirth_ =
        Optics.lens (fun (data: CharSheet) -> data.yearOfBirth) (fun (value: int) (data: CharSheet) ->
            { data with yearOfBirth = value })

    let sex_ =
        Optics.lens (fun (data: CharSheet) -> data.sex) (fun (value: Sex) (data: CharSheet) -> { data with sex = value })

module Creature =
    open Domain.Model

    let name_ =
        Optics.lens (fun (data: Creature) -> data.name) (fun (value: string) (data: Creature) ->
            { data with name = value })

    let stats_ =
        Optics.lens (fun (data: Creature) -> data.stats) (fun (value: StatSource) (data: Creature) ->
            { data with stats = value })
