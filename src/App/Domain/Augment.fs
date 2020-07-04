// augment auto-generated operations

namespace Domain
open Optics
open Optics.Operations
open Domain.Model
open Domain.Model.Character

module Stat =
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

    let lenses = [Stats.str_; Stats.dex_; Stats.con_; Stats.int_; Stats.wis_; Stats.cha_]
    let lens (s:Stat) = lenses.[toTag s]
    let values = [Str; Dex; Con; Int; Wis; Cha]

module StatSource =
    let charSheet_ = Optics.prism (function CharSheet v -> Some v | _ -> None) (fun v d -> CharSheet v)
    let age_ = charSheet_ => CharSheet.xp_
    let yearOrBirth_ = charSheet_ => CharSheet.yearOfBirth_
    let statBlock_ = Optics.lens (function CharSheet v -> v.statBlock | StatBlock v -> v) (fun v -> function CharSheet d -> CharSheet { d with statBlock = v } | StatBlock d -> StatBlock v)

