namespace Domain
open Optics
open Optics.Operations
open Domain.Model
module StatSource =
    let charSheet = Optics.prism (function CharSheet v -> Some v | _ -> None) (fun v d -> CharSheet v)
    let age = charSheet => CharSheet.xp_
    let yearOrBirth = charSheet => CharSheet.yearOfBirth_
    let statBlock = Optics.lens (function CharSheet v -> v.statBlock | StatBlock v -> v) (fun v -> function CharSheet d -> CharSheet { d with statBlock = v } | StatBlock d -> StatBlock v)
