module Domain.Chargen
open Common
open Domain
open Model
open Stats
open Optics.Operations

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

