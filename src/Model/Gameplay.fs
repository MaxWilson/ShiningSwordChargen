module Model.Gameplay
open Interaction
open Model.Types
open Model.Operations
open Model.Tables
open Common
open System
open System

let calculate mtable (monsters: Name seq) =
    let costs = monsters |> Seq.map (fun m -> Math.Pow((mtable m |> snd |> float) / 100., (2./3.)))
    (Math.Pow(Seq.sum costs, 1.5) * 100. |> Math.Round |> int)

let normalize template =
    [|for (name, i) in template do
        for i in 1..i do
            yield name
        |]

let makeEncounter (mtable: Name -> float * int) templates (maxCR: int) (xpBudget: int) =
    let rec generate() =
        let template : (Name * int) list = templates maxCR
        let template = normalize template
        let rec addMonster accum =
            let precost = calculate mtable accum
            if precost >= xpBudget then
                accum
            else
                let monster = template.[random.Next(template.Length)]
                let monsters' = monster::accum
                let postcost = calculate mtable monsters'
                if postcost <= xpBudget then
                    addMonster monsters'
                else // probabilistically add the final monster, or not
                    let overage = postcost - xpBudget
                    let overageRatio = (float overage) / (float (postcost - precost))
                    if random.NextDouble() < overageRatio then
                        accum
                    else
                        monsters'
        match addMonster [] with
        | [] ->
            generate() // this template was too tough to allow even one monster--choose a different template
        | candidate ->
            candidate
    let lst = generate()
    lst |> List.groupBy id |> List.map (fun (k, vs) -> k, List.length vs) |> List.sortByDescending snd

let monsters = [
    "Hobgoblin", 0.5
    "Orc", 0.5
    "Orog", 2.
    "Orc War Chief", 4.
    "Beholder", 13.
    "Frost Giant", 8.
    "Fire Giant", 9.
    "Skeleton", 0.25
    "Zombie", 0.5
    "Goblin", 0.25
    "Flameskull", 4.
    "Githyanki Warrior", 3.
    "Yeti", 3.
    "Young White Dragon", 6.
    "Young Red Dragon", 10.
    "Adult Red Dragon", 17.
    "Ancient White Dragon", 20.
    "Purple Worm", 15.
    "Nightwalker", 20.
    "Bodak", 6.
    "Tarrasque", 30.
    ]
let lookup monsters name = monsters |> List.find (fst >> (=) name) |> fun (_, cr) -> Model.Tables.monsterCR |> Array.pick (function { CR = cr'; XPReward = xp } when cr' = cr -> Some(cr, xp) | _ -> None)
let templates = [|
    ["Orc", 10; "Orc War Chief", 1]
    ["Beholder", 1; "Hobgoblin", 20]
    ["Fire Giant", 1; "Hobgoblin", 8; "Skeleton", 4]
    ["Orc", 10; "Orog", 1]
    ["Skeleton", 3; "Zombie", 2]
    ["Orc", 6; "Skeleton", 4]
    ["Githyanki Warrior", 6; "Yeti", 3]
    ["Young White Dragon", 1]
    ["Young Red Dragon", 1]
    ["Adult Red Dragon", 1]
    ["Frost Giant", 1; "Yeti", 2]
    ["Beholder", 1; "Purple Worm", 1; "Adult Red Dragon", 1; "Frost Giant", 3]
    ["Nightwalker",1;"Ancient White Dragon", 1; "Beholder", 1; "Purple Worm", 1; "Adult Red Dragon", 1; "Frost Giant", 3]
    ["Ancient White Dragon", 1; "Beholder", 1; "Purple Worm", 1; "Adult Red Dragon", 1; "Frost Giant", 3]
    ["Nightwalker", 1; "Bodak", 6]
    ["Tarrasque", 1]
    |]
let rec getTemplate monsters (templates: (string * int) list[]) maxCR =
    let t = templates.[random.Next(templates.Length)]
    if t |> List.exists (fun (name, _) -> (lookup monsters name |> fst) |> int > maxCR) then
        getTemplate monsters templates maxCR
    else
        t
let mixTemplate mixProbability getTemplate arg =
    if (random.NextDouble() < mixProbability) then
        (getTemplate arg) @ (getTemplate arg)
    else getTemplate arg

let queryInteraction = Interaction.InteractionBuilder<Query, string>()

let rec getPCs() : Eventual<_,_,_> = queryInteraction {
    let! name = Query.text "Enter a name:"
    let pc = { name = name; xp = 0; hp = 10 }
    let! more = Query.confirm "Are there any more?"
    if more then
        let! rest = getPCs()
        return pc::rest
    else
        return [pc]
    }

let makeTower pcs parXpEarned nTower =
    let N = pcs |> Seq.length // number of ideal PCs
    let avg a b = (a + b)/2
    let isEpic = parXpEarned >= 400000
    let computeLevel xp =
        (levelAdvancement |> Array.findBack (fun x -> xp >= x.XPReq)).level
    let level = (computeLevel parXpEarned)
    let budget =
        match nTower with
        // once you've been 20th level for a while, we take off the difficulty caps and scale to unlimited difficulty
        | 1 | 2 when isEpic ->
            N * (parXpEarned / 40)
        | 3 when isEpic ->
            N * (parXpEarned / 27)
        | _ when isEpic ->
            N * (parXpEarned / 20)
        // otherwise use the DMG tables. Note that encounters 1-4 should sum to somewhat less than a full day's XP budget,
        // because you will have random encounters while resting.
        | 1 ->
            N * (avg xpBudgets.[level-1].easy xpBudgets.[level-1].medium)
        | 2 ->
            N * (avg xpBudgets.[level-1].medium xpBudgets.[level-1].hard)
        | 3 ->
            N * (avg xpBudgets.[level-1].hard xpBudgets.[level-1].deadly)
        | _ ->
            N * ((float xpBudgets.[level-1].deadly) * 1.2 |> int)
    let e = makeEncounter (lookup monsters) (getTemplate monsters templates |> mixTemplate 0.30) (if isEpic then 30 else level) budget
    let cost = (calculate (lookup monsters) (normalize e))
    let xpEarned = e |> Seq.sumBy (fun (name, i) -> i * (lookup monsters name |> snd))
    let earned = xpEarned/N
    let gpEarned = rand (Math.Log (float budget) |> int)
    e, cost, earned, gpEarned

let makeRandom pcs parXpEarned nRandom =
    let N = pcs |> Seq.length // number of ideal PCs
    let avg a b = (a + b)/2
    let isEpic = parXpEarned >= 400000
    let computeLevel xp =
        (levelAdvancement |> Array.findBack (fun x -> xp >= x.XPReq)).level
    let level = (computeLevel parXpEarned)
    let minRandomEncounterBudget = if isEpic then N * (parXpEarned / 80) else N * (avg xpBudgets.[level-1].easy xpBudgets.[level-1].medium) / 2
    let budget = minRandomEncounterBudget * nRandom
    let e = makeEncounter (lookup monsters) (getTemplate monsters templates) (if isEpic then 30 else level) budget
    let c = (calculate (lookup monsters) (normalize e))
    let xpEarned = e |> Seq.sumBy (fun (name, i) -> i * (lookup monsters name |> snd))
    let earned = xpEarned / N
    let gpEarned = rand (Math.Log10 (float budget) |> int) // make less money from random encounters
    e, c, earned, gpEarned

type State = {
    pcs: StatBlock list
    parEarned: int
    gateNumber: int
    towerNumber: int
    randomNumber: int
    timeElapsed: int // seconds
    gp: int
    }

let oxfordJoin = function
    | a::b::c::rest -> sprintf "%s, and %s" (System.String.Join(", ", b::c::rest)) a
    | [a;b] -> sprintf "%s and %s" a b
    | [a] -> a
    | [] -> "Nothing at all!" // shouldn't happen

let battlecry (pcs: StatBlock list) monsters =
    let plural = match monsters with [_, 1] -> false | _ -> true
    let cries = [|
        sprintf (if plural then """"Give me blood!" you scream as %s attack.""" else """"Give me blood!" you scream as %s attacks.""")
        sprintf (if plural then """"Not again!" you groan, as %s attack.""" else """"Not again!" you groan, as %s attacks.""")
        sprintf """"Blood or death!" shout your companions at %s, as they draw their weapons."""
        sprintf """%s grins crazily and gestures behind you. You turn and see %s!""" (pcs.[random.Next(pcs.Length)].name)
        sprintf "Glumly you prepare yourselves to meet %s in battle."
        |]
    let cry = cries.[random.Next(cries.Length)]
    let rec monsterDescription monsters =
        match monsters with
        | (name:string, qty)::rest when qty = 1 ->
            match Char.ToLowerInvariant(name.[0]) with
            | 'a' | 'e' | 'i' | 'o' | 'u' -> (sprintf "an %s" name)::(monsterDescription rest)
            | _ -> (sprintf "a %s" name)::(monsterDescription rest)
        | (name, qty)::rest ->
            (sprintf "%d %ss" qty name)::(monsterDescription rest)
        | [] -> []
    cry (monsterDescription monsters |> oxfordJoin)

let advance = function
    | { towerNumber = 4 } as state -> { state with gateNumber = state.gateNumber + 1; towerNumber = 1; timeElapsed = state.timeElapsed + 600 }
    | { towerNumber = n } as state -> { state with towerNumber = n + 1; timeElapsed = state.timeElapsed + 600 }

let timeSummary = function
    | n when n < 60 -> sprintf "%d seconds" n
    | n when n < 3600 -> sprintf "%d minutes" (n/60)
    | n when n < 3600*24 -> sprintf "%d hour(s)" (n/(3600))
    | n ->
        let hours = (n/(3600))
        sprintf "%d day(s) and %d hour(s)" (hours / 24) (hours % 24)

let rec doRest state : Eventual<_,_,_> = queryInteraction {
    match! Query.choose (sprintf "You have earned %d XP and %d gold pieces, and you've been adventuring for %s. What do you wish to do next?" state.pcs.[0].xp state.gp (timeSummary state.timeElapsed)) ["Advance"; "Rest"; "Return to town"] with
        | "Advance" -> return! doTower (advance state)
        | "Rest" -> return! doRest (advance state)
        | "Return to town" ->
            do! Query.alert (sprintf "You happily retire from adventuring and spend the rest of your life living off %d gold pieces that you found." state.gp)
            return state
        | _ -> return state
        }
and doTower state : Eventual<_,_,_> = queryInteraction {
    let e, c, xp, gp = makeTower state.pcs state.parEarned state.towerNumber
    do! Query.alert (sprintf "You have reached the %s tower of Gate #%d" (match state.towerNumber with | 1 -> "first" | 2 -> "second" | 3 -> "inner" | _ -> "final") state.gateNumber)
    do! Query.alert (battlecry state.pcs e)
    if rand 10 = 1 then
        // super simple battle resolver: die 10% of the time
        do! Query.alert "You have died!"
        return state
    else
        do! Query.alert (sprintf "You have found %d gold pieces and earned %d experience points." gp xp)
        let state = { state with gp = state.gp + gp; pcs = state.pcs |> List.map (fun pc -> { pc with xp = pc.xp + xp }); parEarned = state.parEarned + xp }
        match! Query.choose (sprintf "You have earned %d XP and %d gold pieces, and you've been adventuring for %s. What do you wish to do next?" state.pcs.[0].xp state.gp (timeSummary state.timeElapsed)) ["Advance"; "Rest"; "Return to town"] with
        | "Advance" -> return! doTower (advance state)
        | "Rest" -> return! doRest (advance state)
        | "Return to town" ->
            do! Query.alert (sprintf "%s happily retire from adventuring and spend the rest of your life living off %d gold pieces that you found." (state.pcs |> List.map (fun pc -> pc.name) |> oxfordJoin) state.gp)
            return state
        | _ -> return state
    }
let doGate state : Eventual<_,_,_> = queryInteraction {
    return! (doTower state)
    }

let game() : Eventual<_,_,_> = queryInteraction {
    let! party = getPCs()
    do! Query.alert "Before you lies the Wild Country, the Gate of Doom. Prepare yourselves for death and glory!"
    let state = { pcs = party; parEarned = 0; gateNumber = 1; towerNumber = 1; randomNumber = 1; timeElapsed = 0; gp = 0 }
    let! state = doGate state
    return ()
    }