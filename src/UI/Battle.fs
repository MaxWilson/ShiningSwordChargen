module UI.Battle
open Common
open Model.Types
open Model.Functions
open UI.Global
open UI.Types

open Fable.Helpers.React
open Fable.Helpers.React.Props
open Elmish.React
open Fulma

let battleSummary fullInfo (combatants:Combatant seq) =
    // fullInfo: controls whether full info is displayed in the UI for this creature e.g. exact HP totals, current orders
    let giveOrders pc _ = ()
    table [ClassName "table"] [
        thead [] [
            tr [] [
                yield th [] [str "ID"]
                yield th [] [str "Name"]
                yield th [] [str "Type"]
                yield th [] [str "Status"]
                yield th [] [str "AC"]
                yield th [] [str "HP"]
                yield th [] [str "Last Round"]
                if fullInfo then yield th [] [str "This Round"]
                ]
            ]
        tbody [] (combatants |> Seq.map (fun npc ->
                let maxHP = npc.stats.hp
                let hp =
                    match npc.usages.TryGetValue("HP") with
                    | true, v -> v
                    | _ -> maxHP
                tr [OnClick (giveOrders npc); Style[Color (sprintf "%A" npc.team)]] ([
                    yield str <| npc.id.ToString()
                    yield str npc.stats.name
                    yield str <| defaultArg npc.stats.typeName ""
                    yield str <| describeStatus hp maxHP
                    yield str (describeAC fullInfo npc.stats.ac)
                    if fullInfo then yield str <| (sprintf "%d/%d" hp maxHP) else yield str <| sprintf "About %d" (5. * (System.Math.Round (float hp / 5.)) |> int)
                    yield str "Attack!"
                    if fullInfo then yield str "Attack!"
                    ] |> List.map (fun v -> td [] [v]))
            ))
        ]

let view1 dispatch modalOperation buttonsWithHotkeys logOutput (game:GameState) (battle: Battle1.State1) =
    let winBattle _ =
        game |> Model.Gameplay.finishTower
            |> modalOperation dispatch (Battle1.Finish >> Battle1Update >> dispatch)
    let fightOneRound _ =
        let state = game |> Model.Gameplay.fight
        state |> Model.Gameplay.finishTower
            |> modalOperation dispatch (Battle1.Finish >> Battle1Update >> dispatch)
    let teams = battle.combatants |> Seq.map (function KeyValue(_,(c:Combatant)) -> c) |> Seq.groupBy (fun c -> c.team) |> Map.ofSeq
    [   div [ClassName "battleSummary"][
            for team in teams do
                yield div [ClassName "heading"] [str (match team.Key with TeamId.Blue -> "Friendlies" | TeamId.White as teamId -> "Neutrals" | _ -> "Hostiles")]
                yield battleSummary (match team.Key with Blue | White -> true | _ -> false) team.Value
            yield! buttonsWithHotkeys ["Fight", fightOneRound; "Win", winBattle]
            ]
        logOutput
        ]

type CloudStorage() =
    interface DataEngine.IDataStorage with
        member this.Save (label:DataEngine.Label) data callback = notImpl()
        member this.Load label callback = notImpl()

let update (model: Model) = function
    | Battle.Msg.Update state ->
        { model with game = { model.game with battle = Some state } }


let respond state continuation txt =
    DataEngine.execute (CloudStorage()) state txt continuation

let display detailLevel (logEntries: Log.Entry<_> list) =
    [for (cmd, e) in logEntries do
        let rec help currentDepth logEntry =
            // visually distinguish log entries from commands by italicizing them, among other things so that a user can recognize commands that failed to parse
            let txt = match cmd with Model.Types.Battle2.Log(_) when currentDepth = 0 -> i [] [str (Log.getText logEntry)] | _ -> str (Log.getText logEntry)
            match logEntry with
            | Hierarchy.Nested(_, children) when detailLevel > currentDepth -> // if detailLevel = 2, last recursion will happen when currentDepth = 1.
                let children' = children |> List.map (help <| currentDepth + 1)
                li [] [txt; ul [] children']
            | _ ->
                match cmd with
                | Model.Types.Battle2.Log(_) when currentDepth = 0 ->
                    li [] [txt]
                | _ ->
                    li [] [txt]
        yield help 0 e]

let logOutput =
    lazyView <| fun (detailLevel, log: Log.Data<_>) ->
        if log = Log.empty then div[ClassName "logDisplay"][]
        else
            let log = Log.extractEntries log |> List.collect id
            ul [ClassName "logDisplay"] (display detailLevel log)

let view respond (battle: Battle2.State) =
    [
        match battle.view.lastOutput with
        | [] -> ()
        | outputs ->
            yield ul[ClassName "battleSummary content"](outputs |> (display battle.view.logDetailLevel))
        yield div[ClassName "interaction"] [
            statefulInput respond [Placeholder "Enter a command"; AutoFocus true; ClassName "prompt"]
            ]
        yield div[ClassName "logDisplay content"] [
            (battle.view.logDetailLevel, battle.data.log) |> logOutput
            ]
        ]