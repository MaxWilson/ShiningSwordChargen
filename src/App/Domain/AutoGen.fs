//------------------------------------------------------------------------------
//        This code was generated by myriad.
//        Changes to this file will be lost when the code is regenerated.
//------------------------------------------------------------------------------
namespace rec Domain


namespace rec Domain

module Sex =
    open Domain.Model.Character

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

module Feat =
    open Domain.Model.Character

    let toString (x: Feat) =
        match x with
        | Sharpshooter -> "Sharpshooter"
        | CrossbowExpert -> "CrossbowExpert"
        | HeavyArmorMaster -> "HeavyArmorMaster"
        | GreatWeaponMaster -> "GreatWeaponMaster"

    let fromString (x: string) =
        match x with
        | "Sharpshooter" -> Some Sharpshooter
        | "CrossbowExpert" -> Some CrossbowExpert
        | "HeavyArmorMaster" -> Some HeavyArmorMaster
        | "GreatWeaponMaster" -> Some GreatWeaponMaster
        | _ -> None

    let toTag (x: Feat) =
        match x with
        | Sharpshooter -> 0
        | CrossbowExpert -> 1
        | HeavyArmorMaster -> 2
        | GreatWeaponMaster -> 3

    let isSharpshooter (x: Feat) =
        match x with
        | Sharpshooter -> true
        | _ -> false

    let isCrossbowExpert (x: Feat) =
        match x with
        | CrossbowExpert -> true
        | _ -> false

    let isHeavyArmorMaster (x: Feat) =
        match x with
        | HeavyArmorMaster -> true
        | _ -> false

    let isGreatWeaponMaster (x: Feat) =
        match x with
        | GreatWeaponMaster -> true
        | _ -> false

module Skill =
    open Domain.Model.Character

    let toString (x: Skill) =
        match x with
        | Athletics -> "Athletics"
        | Stealth -> "Stealth"
        | Perception -> "Perception"
        | Insight -> "Insight"

    let fromString (x: string) =
        match x with
        | "Athletics" -> Some Athletics
        | "Stealth" -> Some Stealth
        | "Perception" -> Some Perception
        | "Insight" -> Some Insight
        | _ -> None

    let toTag (x: Skill) =
        match x with
        | Athletics -> 0
        | Stealth -> 1
        | Perception -> 2
        | Insight -> 3

    let isAthletics (x: Skill) =
        match x with
        | Athletics -> true
        | _ -> false

    let isStealth (x: Skill) =
        match x with
        | Stealth -> true
        | _ -> false

    let isPerception (x: Skill) =
        match x with
        | Perception -> true
        | _ -> false

    let isInsight (x: Skill) =
        match x with
        | Insight -> true
        | _ -> false

module ElfRace =
    open Domain.Model.Character

    let toString (x: ElfRace) =
        match x with
        | High -> "High"
        | Wood -> "Wood"
        | Drow -> "Drow"

    let fromString (x: string) =
        match x with
        | "High" -> Some High
        | "Wood" -> Some Wood
        | "Drow" -> Some Drow
        | _ -> None

    let toTag (x: ElfRace) =
        match x with
        | High -> 0
        | Wood -> 1
        | Drow -> 2

    let isHigh (x: ElfRace) =
        match x with
        | High -> true
        | _ -> false

    let isWood (x: ElfRace) =
        match x with
        | Wood -> true
        | _ -> false

    let isDrow (x: ElfRace) =
        match x with
        | Drow -> true
        | _ -> false

module DwarfRace =
    open Domain.Model.Character

    let toString (x: DwarfRace) =
        match x with
        | Mountain -> "Mountain"
        | Hill -> "Hill"

    let fromString (x: string) =
        match x with
        | "Mountain" -> Some Mountain
        | "Hill" -> Some Hill
        | _ -> None

    let toTag (x: DwarfRace) =
        match x with
        | Mountain -> 0
        | Hill -> 1

    let isMountain (x: DwarfRace) =
        match x with
        | Mountain -> true
        | _ -> false

    let isHill (x: DwarfRace) =
        match x with
        | Hill -> true
        | _ -> false

module HumanType =
    open Domain.Model.Character

    let toString (x: HumanType) =
        match x with
        | Standard -> "Standard"
        | Variant _ -> "Variant"

    let fromString (x: string) =
        match x with
        | "Standard" -> Some Standard
        | _ -> None

    let toTag (x: HumanType) =
        match x with
        | Standard -> 0
        | Variant _ -> 1

    let isStandard (x: HumanType) =
        match x with
        | Standard -> true
        | _ -> false

    let isVariant (x: HumanType) =
        match x with
        | Variant _ -> true
        | _ -> false

module Race =
    open Domain.Model.Character

    let toString (x: Race) =
        match x with
        | Human _ -> "Human"
        | Elf _ -> "Elf"
        | Dwarf _ -> "Dwarf"
        | Halforc -> "Halforc"
        | Goblin -> "Goblin"

    let fromString (x: string) =
        match x with
        | "Halforc" -> Some Halforc
        | "Goblin" -> Some Goblin
        | _ -> None

    let toTag (x: Race) =
        match x with
        | Human _ -> 0
        | Elf _ -> 1
        | Dwarf _ -> 2
        | Halforc -> 3
        | Goblin -> 4

    let isHuman (x: Race) =
        match x with
        | Human _ -> true
        | _ -> false

    let isElf (x: Race) =
        match x with
        | Elf _ -> true
        | _ -> false

    let isDwarf (x: Race) =
        match x with
        | Dwarf _ -> true
        | _ -> false

    let isHalforc (x: Race) =
        match x with
        | Halforc -> true
        | _ -> false

    let isGoblin (x: Race) =
        match x with
        | Goblin -> true
        | _ -> false

module Class =
    open Domain.Model.Character

    let toString (x: Class) =
        match x with
        | Barbarian -> "Barbarian"
        | Fighter -> "Fighter"
        | Monk -> "Monk"
        | Rogue -> "Rogue"

    let fromString (x: string) =
        match x with
        | "Barbarian" -> Some Barbarian
        | "Fighter" -> Some Fighter
        | "Monk" -> Some Monk
        | "Rogue" -> Some Rogue
        | _ -> None

    let toTag (x: Class) =
        match x with
        | Barbarian -> 0
        | Fighter -> 1
        | Monk -> 2
        | Rogue -> 3

    let isBarbarian (x: Class) =
        match x with
        | Barbarian -> true
        | _ -> false

    let isFighter (x: Class) =
        match x with
        | Fighter -> true
        | _ -> false

    let isMonk (x: Class) =
        match x with
        | Monk -> true
        | _ -> false

    let isRogue (x: Class) =
        match x with
        | Rogue -> true
        | _ -> false

module FightingStyle =
    open Domain.Model.Character

    let toString (x: FightingStyle) =
        match x with
        | Dueling -> "Dueling"
        | Archery -> "Archery"
        | Defense -> "Defense"
        | GreatWeaponFighting -> "GreatWeaponFighting"

    let fromString (x: string) =
        match x with
        | "Dueling" -> Some Dueling
        | "Archery" -> Some Archery
        | "Defense" -> Some Defense
        | "GreatWeaponFighting" -> Some GreatWeaponFighting
        | _ -> None

    let toTag (x: FightingStyle) =
        match x with
        | Dueling -> 0
        | Archery -> 1
        | Defense -> 2
        | GreatWeaponFighting -> 3

    let isDueling (x: FightingStyle) =
        match x with
        | Dueling -> true
        | _ -> false

    let isArchery (x: FightingStyle) =
        match x with
        | Archery -> true
        | _ -> false

    let isDefense (x: FightingStyle) =
        match x with
        | Defense -> true
        | _ -> false

    let isGreatWeaponFighting (x: FightingStyle) =
        match x with
        | GreatWeaponFighting -> true
        | _ -> false

module Subclass =
    open Domain.Model.Character

    let toString (x: Subclass) =
        match x with
        | Champion -> "Champion"
        | EldritchKnight -> "EldritchKnight"
        | Samurai -> "Samurai"
        | Zealot -> "Zealot"
        | Swashbuckler -> "Swashbuckler"
        | FourElements -> "FourElements"

    let fromString (x: string) =
        match x with
        | "Champion" -> Some Champion
        | "EldritchKnight" -> Some EldritchKnight
        | "Samurai" -> Some Samurai
        | "Zealot" -> Some Zealot
        | "Swashbuckler" -> Some Swashbuckler
        | "FourElements" -> Some FourElements
        | _ -> None

    let toTag (x: Subclass) =
        match x with
        | Champion -> 0
        | EldritchKnight -> 1
        | Samurai -> 2
        | Zealot -> 3
        | Swashbuckler -> 4
        | FourElements -> 5

    let isChampion (x: Subclass) =
        match x with
        | Champion -> true
        | _ -> false

    let isEldritchKnight (x: Subclass) =
        match x with
        | EldritchKnight -> true
        | _ -> false

    let isSamurai (x: Subclass) =
        match x with
        | Samurai -> true
        | _ -> false

    let isZealot (x: Subclass) =
        match x with
        | Zealot -> true
        | _ -> false

    let isSwashbuckler (x: Subclass) =
        match x with
        | Swashbuckler -> true
        | _ -> false

    let isFourElements (x: Subclass) =
        match x with
        | FourElements -> true
        | _ -> false

module ClassAbility =
    open Domain.Model.Character

    let toString (x: ClassAbility) =
        match x with
        | ASIChoice _ -> "ASIChoice"
        | FightingStyle _ -> "FightingStyle"
        | ExtraAttack _ -> "ExtraAttack"
        | SecondWind _ -> "SecondWind"
        | Indomitable _ -> "Indomitable"
        | Subclass _ -> "Subclass"

    let fromString (x: string) =
        match x with
        | _ -> None

    let toTag (x: ClassAbility) =
        match x with
        | ASIChoice _ -> 0
        | FightingStyle _ -> 1
        | ExtraAttack _ -> 2
        | SecondWind _ -> 3
        | Indomitable _ -> 4
        | Subclass _ -> 5

    let isASIChoice (x: ClassAbility) =
        match x with
        | ASIChoice _ -> true
        | _ -> false

    let isFightingStyle (x: ClassAbility) =
        match x with
        | FightingStyle _ -> true
        | _ -> false

    let isExtraAttack (x: ClassAbility) =
        match x with
        | ExtraAttack _ -> true
        | _ -> false

    let isSecondWind (x: ClassAbility) =
        match x with
        | SecondWind _ -> true
        | _ -> false

    let isIndomitable (x: ClassAbility) =
        match x with
        | Indomitable _ -> true
        | _ -> false

    let isSubclass (x: ClassAbility) =
        match x with
        | Subclass _ -> true
        | _ -> false
namespace rec Domain

open AutoWizard
open Domain.Model
open Domain.Model.Character

module Stats =
    open Domain.Model.Character

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

module CharacterSheet =
    open Domain.Model.Draft

    let unmodifiedStats_ =
        Optics.lens (fun (data: CharacterSheet) -> data.unmodifiedStats) (fun (value: Stats) (data: CharacterSheet) ->
            { data with unmodifiedStats = value })

    let explicitName_ =
        Optics.lens (fun (data: CharacterSheet) -> data.explicitName) (fun (value: string option) (data: CharacterSheet) ->
            { data with explicitName = value })

    let autoName_ =
        Optics.lens (fun (data: CharacterSheet) -> data.autoName) (fun (value: string) (data: CharacterSheet) ->
            { data with autoName = value })

    let sex_ =
        Optics.lens (fun (data: CharacterSheet) -> data.sex) (fun (value: Setting<Sex>) (data: CharacterSheet) ->
            { data with sex = value })

    let race_ =
        Optics.lens (fun (data: CharacterSheet) -> data.race) (fun (value: Setting<Race>) (data: CharacterSheet) ->
            { data with race = value })

    let xp_ =
        Optics.lens (fun (data: CharacterSheet) -> data.xp) (fun (value: int) (data: CharacterSheet) ->
            { data with xp = value })

    let allocatedLevels_ =
        Optics.lens (fun (data: CharacterSheet) -> data.allocatedLevels) (fun (value: Class list) (data: CharacterSheet) ->
            { data with allocatedLevels = value })

    let classAbilities_ =
        Optics.lens (fun (data: CharacterSheet) -> data.classAbilities) (fun (value: Setting<ClassAbility> list) (data: CharacterSheet) ->
            { data with classAbilities = value })
