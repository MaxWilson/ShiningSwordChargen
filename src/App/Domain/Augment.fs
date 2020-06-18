// augment auto-generated operations

namespace Domain
open Optics
open Optics.Operations
open Domain.Model

module Stat =
    let lens (s:Stat) = [Stats.str_; Stats.dex_; Stats.con_; Stats.int_; Stats.wis_; Stats.cha_].[Stat.toTag s]
    let values = [Str; Dex; Con; Int; Wis; Cha]

module StatSource =
    let charSheet = Optics.prism (function CharSheet v -> Some v | _ -> None) (fun v d -> CharSheet v)
    let age = charSheet => CharSheet.xp_
    let yearOrBirth = charSheet => CharSheet.yearOfBirth_
    let statBlock = Optics.lens (function CharSheet v -> v.statBlock | StatBlock v -> v) (fun v -> function CharSheet d -> CharSheet { d with statBlock = v } | StatBlock d -> StatBlock v)

