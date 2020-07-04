module Domain.Unit
open Expecto
open Expecto.Flip
open FsCheck
open Swensen.Unquote

open Domain
open Domain.Model
open Domain.Model.Character
open Domain.Chargen

// map lvl onto 1-20 inclusive, and ignore as invalid any entries that are not increasing
let normalizeLevels (spec: (Class * int) list) =
    spec |> List.map (fun (c, lvl) -> c, (abs lvl) % 20 + 1)
    |> List.fold(fun (accum,alreadySeen) (c, lvl) ->
        match alreadySeen |> Map.tryFind c, accum with
        | Some prev, (p, plvl)::paccum when lvl > prev && p = c -> (c, lvl)::paccum, alreadySeen |> Map.add c lvl
        | Some prev, _ when lvl > prev -> (c, lvl)::accum, alreadySeen |> Map.add c lvl
        | Some prev, _ -> accum, alreadySeen
        | None, _ -> (c,lvl)::accum, alreadySeen |> Map.add c lvl) ([], Map.empty)
    |> fst
    |> List.rev

module Settings =
    open AutoWizard
    // fill any any build choices with whatever so we can test the things that are actually fixed
    // like ExtraAttack
    let arbitrarilyFulfill (features: Setting<ClassAbility> list) =
        let getLens hashCode =
            // if there's any choices left to take, just take the first option
            Optics.lens (fun () -> ChoiceIndex 0 |> Some) (fun _ d -> d)
        let render =
            { new Render<unit, unit> with
                  member this.Render(options: 't1 list) (lens: Optics.Lens<unit,ChoiceState option>): unit list =
                      []
                }
        features
        |> List.map (fun s -> eval(s, getLens, render, ()))
        |> List.map (function Complete(v), _ -> v | v, _ -> failwithf "Unexpected output! '%A' should be completed" v)
        // now filter out duplicates, e.g. Second Wind 10 + Second Wind 11 = just Second Wind 11
        |> List.groupBy ClassAbility.toTag
        |> List.map (function _, [v] -> v | _, vs -> vs |> List.sortDescending |> List.head)

[<Tests>]
let tests = testList "Domain.Unit.Spot checks" [
    testProperty "Transform expandClasses keeps the same base class and same total number of levels" <|
        fun (spec: (Class * int) list) ->
            (spec.Length > 0) ==> lazy
                let spec = spec |> normalizeLevels
                let spec' = spec |> expandClasses |> compactClasses
                test <@ spec' = spec && (fst spec'.Head) = (fst spec.Head) @>
    testProperty "Transform compactClasses keeps the same base class and same total number of levels" <|
        fun (spec: Class list) ->
            spec.Length > 0 ==> lazy
                let spec' = spec |> compactClasses |> expandClasses
                test <@ spec' = spec && spec'.Head = spec.Head @>
    testCase "Check that Extra Attack doesn't stack" <| fun _ ->
        let eval classes =
            let cl = expandClasses classes
            classFeatures cl |> Settings.arbitrarilyFulfill
            |> List.pick (function ExtraAttack n as e -> Some e | _ -> None)
        eval [Barbarian, 5]
        |> Expect.equal "Barbarian 5 should grant extra attack" (ExtraAttack 1)
        eval [Fighter, 8]
        |> Expect.equal "Fighter 8 should grant extra attack" (ExtraAttack 1)
        eval [Fighter, 11]
        |> Expect.equal "Fighter 11 should grant extra attack 2" (ExtraAttack 2)
        eval [Fighter, 20]
        |> Expect.equal "Fighter 20 should grant extra attack 3" (ExtraAttack 3)
        eval [Barbarian, 5; Fighter, 11]
        |> Expect.equal "Barbarian 5 extra attack does not stack with Fighter 11" (ExtraAttack 2)
    ]
