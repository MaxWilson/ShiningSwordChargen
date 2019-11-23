module Ribbit

open System
open Elmish
open Common

// make sure errors are not silent: show them as Alerts (ugly but better than nothing for now)
open Fable.React
open Fable.React.Props
open Data
open Data.Functor
open Domain
open Domain.Properties
open Domain.Commands

module Domain =
    type EventStatus = Blocked | Resolved of output: string option
    type Event = { status: EventStatus; cmd: Command; cmdText: string }
    type Model = {
            properties: Property list
            roster: SymmetricMap.Data<Id, string>
            data: Map<Key, int>
            blocking: SymmetricRelation.Data<Key, Reference> // data which needs to be in data to proceed
            blockedThreads: {| eventId: Id; stack: Command |} list
            eventLog: FastList<Event>
        }
    let fresh = { properties = []; roster = SymmetricMap.empty(); data = Map.empty; blocking = SymmetricRelation.empty; blockedThreads = []; eventLog = FastList.fresh() }
    module Lens =
        let data = Lens.lens (fun d -> d.data) (fun v d -> { d with data = v})
        let creatureIds = Lens.lens (fun d -> d.roster) (fun v d -> { d with roster = v})
        let blocking = Lens.lens (fun d -> d.blocking) (fun v d -> { d with blocking = v})
        let blockedThreads = Lens.lens (fun d -> d.blockedThreads) (fun v d -> { d with blockedThreads = v})
        let eventLog = Lens.lens (fun d -> d.eventLog) (fun v d -> { d with eventLog = v})

    let rec tryParseExpression model (cmd: string) =
        match Int32.TryParse cmd with
        | true, v -> Number v |> Literal |> Some
        | _ when String.IsNullOrWhiteSpace cmd -> None
        | _ ->
            // REALLY crude parsing because that's not the point right now
            if cmd.Contains "+" || cmd.Contains "-" then
                let op = if (cmd.Contains "+") then Plus else Minus
                let cmds = cmd.Split('+', '-') |> Array.map (tryParseExpression model >> FSharp.Core.Option.get)
                match cmds with
                | [|lhs; rhs|] -> BinaryOperation(lhs, op, rhs) |> Some
                | _ -> None
            else
                match cmd.Split('.') with
                | [|name;prop|] ->
                    match model.roster |> SymmetricMap.tryFindValue name with
                    | Some id ->
                        Ref(PropertyRef (id, prop.Trim())) |> Some
                    | _ -> None
                | _ -> Ref(PropertyRef(0, cmd)) |> Some
    let rec tryParseCommand model (cmd: string) =
        if cmd.StartsWith ("add ") then
            Some (AddRow <| cmd.Replace("add ", "").Trim())
        elif cmd.Contains("=") then
            match cmd.Split('=') with
            | [|lhs; value|] ->
                match tryParseExpression model value with
                | Some expr ->
                    match lhs.Split('.') with
                    | [|name;prop|] ->
                        match model.roster |> SymmetricMap.tryFindValue name with
                        | Some id ->
                            SetData((id, prop.Trim()), expr) |> Some
                        | _ -> None
                    | _ -> SetData((0, cmd), expr) |> Some
                | _ -> None
            | _ -> None
        else
            match tryParseExpression model cmd with
            | Some e -> Some (Evaluate e)
            | _ -> None
    let rec eval model = function
        | Literal (Number v) -> Ready v
        | Ref (PropertyRef key) -> match model.data.TryFind key with Some v -> Ready v | None -> Awaiting key
        | BinaryOperation(lhs, op, rhs) ->
            match eval model lhs, eval model rhs with
            | Awaiting key, _ | _, Awaiting key -> Awaiting key
            | Ready l, Ready r ->
                Ready(if op = Plus then l + r else l - r)
        | BestN(n, exprs) ->
            match exprs |> List.tryMapFold(fun vs e -> match eval model e with Ready v -> Ok(v::vs) | Awaiting k -> Error k) [] with
            | Ok vs -> Ready (vs |> List.sortDescending |> List.take (min vs.Length n) |> List.sum)
            | Error k -> Awaiting k

    let addName name model =
        if (model: Model).roster |> SymmetricMap.tryFindValue name |> Option.isSome then // idempotence
            model
        else
            let id = 1 + (if model.roster |> SymmetricMap.isEmpty then 0 else model.roster |> SymmetricMap.toSeq |> Seq.map fst |> Seq.max)
            model |> Lens.over Lens.creatureIds (SymmetricMap.add id name)
    let addProperty propertyName model =
        if (model:Model).properties |> List.exists (fun p -> p.name = propertyName) then
            model
        else
            { model with properties = model.properties @ [{name = propertyName}] }
    let execute model cmdText cmd =
        let model = model |> Lens.over Lens.eventLog (add { status = Blocked; cmd = cmd; cmdText = cmdText })
        let eventId = model.eventLog.lastId.Value
        let resolve eventId msg model =
            model |> Lens.over Lens.eventLog (transform eventId (fun e -> { e with status = Resolved msg }))
        let rec help model (eventId, cmd) =
            match cmd with
            | Evaluate expr as cmd ->
                match eval model expr with
                | Ready v -> resolve eventId (v.ToString() |> Some) model
                | Awaiting key ->
                    { model with blockedThreads = {| eventId = eventId; stack = cmd; |} :: model.blockedThreads }
                        |> Lens.over Lens.blocking (SymmetricRelation.add key (EventRef eventId))
            | AddRow name -> addName name model |> resolve eventId None
            | SetData (key, expr) ->
                // execute any unblocked threads
                let unblock key (model: Model) =
                    match model.blocking.forward |> Map.tryFind key with
                    | None | Some [] ->
                        model
                    | Some unblocked ->
                        // re-process the unblocked threads from the beginning (TODO: store continuations instead of whole cmds)
                        unblocked
                            |> List.map (function
                                            | EventRef eventId -> eventId, (model.blockedThreads |> List.find (fun t -> t.eventId = eventId)).stack
                                            | v -> matchfail v) // don't yet have an implementation for unblocking data references
                            |> List.fold help { model with blocking = SymmetricRelation.removeAllForward key model.blocking }
                match eval model expr with
                | Ready v ->
                    model |> addProperty (snd key) |> Lens.over Lens.data (Map.add key v) |> unblock key |> resolve eventId None
                | _ -> model
        eventId, help model (eventId, cmd)

module View =
    type Id = int
    type ConsoleLog = Resolved of string | Blocked of Id
    type Model = { currentInput: string; console: ConsoleLog list; domainModel: Domain.Model }
    module Lens =
        let domainModel = Lens.lens (fun d -> d.domainModel) (fun v d -> { d with domainModel = v })
    type Cmd = NewValue of string | ENTER | RESET | Error of string
    let summaryOf (m:Domain.Model) =
        let data = m.data
        [
            yield tr[][
                yield th[][str "Name"]
                for p in m.properties do yield th[][str p.name]
            ]
            for (id, name) in m.roster |> SymmetricMap.toSeq do
                yield tr[][
                    yield td[][str name]
                    for p in m.properties ->
                        match data |> Map.tryFind (id, p.name) with
                        | Some v -> td[][str (v.ToString())]
                        | None -> td[][str "???"]
                ]
            ]
    let view m dispatch =
        let log = [
            for e in m.console ->
                match e with
                | Resolved s -> li[ClassName "resolved"][str s]
                | Blocked id ->
                    let d = m.domainModel
                    let event = d.eventLog.rows.[id]
                    let dependencies = d.blocking.backward.[EventRef id] |> List.map (fun (id, prop) -> if id > 0 then (d.roster |> SymmetricMap.find id) + "." + prop else prop)
                                        |> String.join ", "
                    li[ClassName "blocked"][str <| sprintf "%s (needs %s)" event.cmdText dependencies]
            ]

        div [ClassName ("frame" + if log = [] then "" else " withSidebar")] [
            yield p[ClassName "summaryPane"][
                    table [] (summaryOf m.domainModel)
                ]
            yield p[ClassName "queryPane"] [
                h2[][str "Enter a command:"]
                br[]
                form [OnSubmit (fun e -> dispatch ENTER; e.preventDefault())] [
                        input [OnChange (fun e -> e.Value |> NewValue |> dispatch); HTMLAttr.Value m.currentInput; HTMLAttr.AutoFocus true]
                        button[Type "submit"][str "OK"]
                    ]
                br[]
                ]
            if log <> [] then
                yield div[ClassName "sidebar"][
                    ul[] log
                    button[OnClick (fun _ -> dispatch RESET)][str "Clear"]
                ]
        ]

    let init _ = { currentInput = ""; console = []; domainModel = Domain.fresh }, Cmd.Empty
    let update msg model =
        try
            match msg with
            | NewValue s -> { model with currentInput = s; }, Cmd.Empty
            | ENTER ->
                match Domain.tryParseCommand model.domainModel model.currentInput with
                | Some expr ->
                    let eventId, domain' = Domain.execute model.domainModel model.currentInput expr
                    let logEntry eventId =
                        let event = domain'.eventLog.rows.[eventId]
                        match event.status with
                        | Domain.Blocked -> Blocked eventId
                        | Domain.Resolved None -> Resolved (event.cmdText)
                        | Domain.Resolved (Some output) -> sprintf "%s: %s" event.cmdText output |> Resolved
                    let resolve = function Blocked id -> logEntry id | v -> v
                    { model with currentInput = ""; domainModel = domain'; console = (model.console@[logEntry eventId] |> List.map resolve) }, Cmd.Empty
                | _ -> model, Cmd.Empty
            | RESET -> init()
            | Error err ->
                { model with console = model.console@[Resolved err] }, Cmd.Empty
        with err ->
            { model with console = model.console@[Resolved err.Message] }, Cmd.Empty
