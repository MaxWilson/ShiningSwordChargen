module Domain.Chargen
open Common
open Domain
open Model
open Stats
open Optics.Operations
open Character

let roll3d6InOrder() =
    let r _ = Array.init 3 (thunk1 d 6) |> Array.sum
    { str = r(); dex = r(); con = r(); int = r(); wis = r(); cha = r() }

let roll4d6k3() =
    let r _ = Array.init 4 (thunk1 d 6) |> Array.sortDescending |> Array.take 3 |> Array.sum
    { str = r(); dex = r(); con = r(); int = r(); wis = r(); cha = r() }

let reassign ((p1, p2, p3, p4, p5, p6) as priorityRankings) ({ str = v1; dex = v2; con = v3; int = v4; wis = v5; cha = v6 } as stats) =
    let lenses = [|str_,p1; dex_,p2; con_,p3; int_,p4; wis_,p5; cha_,p6|] |> shuffleCopy |> Array.sortBy snd |> Array.map fst
    let statsInOrder = [| v1; v2; v3; v4; v5; v6 |] |> Array.sortDescending |> Array.mapi (fun i v -> write lenses.[i] v)
    statsInOrder |> Array.fold (fun s t -> t s) stats

let standardArray() =
  let lenses = [|str_;dex_; con_; int_; wis_; cha_|] |> shuffleCopy
  let statsInOrder = [| 15; 14; 13; 12; 10; 8 |] |> Array.sortDescending |> Array.mapi (fun i v -> write lenses.[i] v)
  statsInOrder |> Array.fold (fun s t -> t s) { str = 0; dex = 0; con = 0; int = 0; wis = 0; cha = 0 }

module Character =
    let subclasses = function
        | Fighter, n when n >= 3 -> [Champion; EldritchKnight; Samurai]
        | Monk, n when n >= 3 -> [FourElements]
        | Rogue, n when n >= 3 -> [Swashbuckler]
        | _ -> []

    let expandClasses (classLevels: (Class * int) list) : Class list =
        let rec add alreadySeen accumulator = function
            | [] -> accumulator
            | (charClass, level)::rest ->
                let newLevels =
                    match alreadySeen |> Map.tryFind charClass with
                    | Some n -> List.replicate (max 0 (level - n)) charClass
                    | None -> List.replicate (max 0 level) charClass
                add (alreadySeen |> Map.add charClass level) (accumulator@newLevels) rest
        add Map.empty [] classLevels

    let compactClasses (classes: Class list) : (Class * int) list =
        let rec add classes = function
            | [] ->
                classes |> List.rev
            | cl::rest ->
                match classes with
                | (h,lvl)::lrest when h = cl ->
                    rest |> add ((h, lvl+1)::lrest)
                | _ ->
                    let lvl = classes |> List.tryPick (function (h,lvl) when h = cl -> Some lvl | _ -> None)
                    rest |> add ((cl, match lvl with Some lvl -> lvl + 1 | None -> 1)::classes)
        add [] classes

let classFeatures (classes: Class list) =
    [for (charClass, lvl) in classes |> List.groupBy id |> List.map(fun (c,g) -> c, g.Length) do
        let c = AutoWizard.c
        let choose = AutoWizard.choose
        let ctor = AutoWizard.ctor
        let ctor2 = AutoWizard.ctor2
        let feats =
            [for x in [4;8;12;16;19] do
                if lvl >= x then
                    AutoWizard.ctor("ASI or feat", c ASIChoice,
                        choose [
                            ctor("Feat", (c Feat), choose [c Sharpshooter; c CrossbowExpert])
                            ctor2("ASI", c (fun (s1, s2) -> ASI(s1, s2)), (Stat.values |> List.map c |> choose), (Stat.values |> List.map c |> choose))]
                        )]
        match charClass with
        | Fighter ->
            c (SecondWind lvl)
            if lvl >= 20 then c (ExtraAttack 3)
            elif lvl >= 11 then c (ExtraAttack 2)
            elif lvl >= 5 then c (ExtraAttack 1)
        | Barbarian ->
            if lvl >= 5 then c (ExtraAttack 1)
        | _ -> ()
        ]

module Draft =
    open AutoWizard
    open Draft
    type SettingEvaluator<'t> = Setting<'t> -> 't option

    let chooseFrom lst = lst |> List.map c |> choose
    let featChoice = [c Sharpshooter; alias "Crossbow Expert" CrossbowExpert; alias "Heavy Armor Master" HeavyArmorMaster; alias "Great Weapon Master" GreatWeaponMaster] |> choose
    let fightingStyleChoice = chooseFrom [Dueling; Archery; Defense; GreatWeaponFighting]
    let skillChoice = chooseFrom [Athletics; Stealth; Perception; Insight]
    let raceChoice =
        let humanAsi = ctor("Ability score bonuses", c (function [a;b] -> a,b | _ -> shouldntHappen()), chooseDistinct 2 (Stat.values |> List.map c))
        choose [
            ctor("Human", c Human,
                choose [
                    c Standard
                    ctor3("Variant", c Variant, skillChoice, featChoice, humanAsi)
                ])
            ctor("Elf", c Elf,
                choose [
                    alias "High elf" High
                    alias "Wood elf" Wood
                    c Drow
                ])
            ctor("Dwarf", c Dwarf,
                choose [
                    c Mountain
                    c Hill
                ])
            c Goblin
            alias "Half-orc" Halforc
        ]

    let autoName (eval:SettingEvaluator<_>) (draft : Draft.DraftSheet) =
        let name =
            match eval(draft.sex) with
            | Some Female -> chooseRandom ["Diana"; "Kiera"; "Kelsey"; "Samantha"; "Alexandra"; "Cleo"; "Berlin"; "Jenny"; "Katherine"]
            | Some Male | _ -> chooseRandom ["Ryan"; "Theodore"; "Sam"; "Alex"; "Max"; "Dante"; "Zorro"; "Vlad"]
        name

    let createBlank eval stats =
        {
        Draft.DraftSheet.unmodifiedStats = stats
        explicitName = None
        autoName = "Unnamed"
        sex = choose [c Male; c Female; c Neither]
        race = raceChoice
        xp = 0
        allocatedLevels = []
        subclasses = Map.empty
        classAbilities = []
        } |> fun draft -> { draft with autoName = autoName eval draft }

    let statBonuses traits =
        // locally stateful seems clearer in this case than using Map.fold
        let mutable bonuses = Map.empty
        let add stat bonus =
            match bonuses |> Map.tryFind stat with
            | Some v -> bonuses <- bonuses |> Map.add stat (v + bonus)
            | None -> bonuses <- bonuses |> Map.add stat bonus
        let rec analyze = function
            | Trait.Race race ->
                match race with
                | Human(Standard) ->
                    Stat.values |> List.iter (flip add 1)
                | Human(Variant(skill, feat, (stat1, stat2))) ->
                    add stat1 1
                    add stat2 1
                    analyze (Feat feat)
                | Elf(sub) ->
                    add Dex 2
                    match sub with
                    | High -> add Int 1
                    | Wood -> add Wis 1
                    | Drow -> add Cha 1
                | Dwarf(sub) ->
                    add Con 2
                    match sub with
                    | Hill -> add Wis 1
                    | Mountain -> add Str 2
                | Halforc ->
                    add Str 2
                    add Con 1
                | Goblin ->
                    add Dex 2
                    add Con 1
            | Feat feat ->
                match feat with
                | HeavyArmorMaster -> add Str 1
                | _ -> ()
            | _ -> ()
        for tr in traits do
            analyze tr
        [for stat in Stat.values do
            match bonuses |> Map.tryFind stat with
            | Some n -> (stat, n)
            | None -> ()
            ]

    let currentStats statBonuses stats =
        statBonuses |> List.fold (fun stats (stat, n) -> stats |> over (Stat.lens stat) ((+)n)) stats

    let subclassOptions classLevels =
        classLevels
        |> Character.compactClasses
        |> List.map (fun (cl, lvl) ->
            match Character.subclasses (cl, lvl) with
            | [] -> c (cl, None)
            | subs -> subs |> List.map (fun sub -> alias (sub.ToString()) (cl, Some sub)) |> choose
            )