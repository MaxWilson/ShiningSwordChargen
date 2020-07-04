module Domain.Unit
open Expecto

open Domain.Model
open Domain.Model.Character

let expand (spec:(Class * int) list) =
    spec |> List.collect (fun (cl, n) ->
        List.replicate n cl)

[<Tests>]
let tests = testList "Domain.Unit.Spot checks" [
    testCase "Check that Extra Attack doesn't stack" <| fun _ ->
        let cl = expand [Barbarian, 5; Fighter, 11]
        notImpl()
    ]