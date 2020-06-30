/// A module for dynamically building interactive wizards that minimize the number of unnecessary questions they ask.
/// Key concepts: 
///   Setting<T>:will eventually yield a T when user finishes answering all the questions.
///   Render<output>: typically Render<ReactElement>, used to format output for user to look at/interact with to update wizardState.
///   WizardState: the current choices that have been made by the user, in the form of Choice hashcodes -> index mapping.
module AutoWizard

type 't LifecycleStage = Unset | Set | Complete of 't
    with static member map f = function Complete v -> Complete (f v) | v -> v
    
// In general, a Setting<T> is something that may or may not yet yield a T, and if it
//    isn't currenting yielding a value then it's asking you questions that will eventually
//    let it yield a value. These questions may be about itself, or about arguments to
//    T's constructor, supplied in the form of Settings themselves.

//type Setting<'t> =
//    Const<'t>: 't -> Setting<'t>
//    Choice : Setting<'t List> -> Setting<'t>
//    App1 :    Setting<'S -> 't> -> Setting<'S> -> Setting<'t>
//    App2 :    Setting<'S1*'S2 -> 't> -> Setting<'S1> -> Setting<'S2> -> Setting<'t>
// 't is a "free" variable to make GDT eval work, will be constrained to be equal to 't but
//    should not be referenced directly in 
type Setting<'t> =
    abstract member Match: IPatternMatch<'t> -> 't LifecycleStage * 'output list
and IPatternMatch<'t> =
    abstract member Const: 't -> 't LifecycleStage * 'output list
    abstract member Choice: Setting<'t> list -> 't LifecycleStage * 'output list
    abstract member App1 : Setting<'s -> 't> -> Setting<'s> -> 't LifecycleStage * 'output list
    abstract member App2 : Setting<'s1*'s2 -> 't> -> Setting<'s1> -> Setting<'s2> -> 't LifecycleStage * 'output list
type Render<'output> = 
    abstract member Render: 't1 -> isSelected:bool -> 'output
and HashCode = int
and WizardState = Map<HashCode, int>

let compose render children (input: 't LifecycleStage) =
    input, [render input]@children
type SettingConst<'t>(v: 't) =
    interface Setting<'t> with
        member this.Match m = m.Const v
    override this.ToString() = sprintf "%A" v
type SettingChoice<'t>(values: Setting<'t> list) =
    interface Setting<'t> with
        member this.Match m = m.Choice values
    override this.ToString() = values |> List.map (fun v -> v.ToString()) |> fun vs -> System.String.Join(", ", vs) |> sprintf "[%s]"
// returns a value only once the user has picked a value
type SettingCtor<'t,'s>(label: string, ctor: Setting<'s -> 't>, arg: Setting<'s>) =
    interface Setting<'t> with
        member this.Match m = m.App1 ctor arg
    override this.ToString() = label
// returns a value only once the user has picked a value
type SettingCtor2<'t,'s1,'s2>(label: string, ctor: Setting<'s1*'s2 -> 't>, arg1: Setting<'s1>, arg2: Setting<'s2>) =
    interface Setting<'t> with
        member this.Match m = m.App2 ctor arg1 arg2
    override this.ToString() = label
let c v = SettingConst(v) :> Setting<_>
let choose options = SettingChoice(options) :> Setting<_>
let ctor(label, f, arg)= SettingCtor(label,f,arg) :> Setting<_>
let ctor2(label, f, arg1, arg2)= SettingCtor2(label, f, arg1, arg2) :> Setting<_>
let both(arg1, arg2) = ctor2("both", c id, arg1, arg2)
type XanatharDifficulty = Easy | Medium | Hard
type XanatharType = Solo | Group | Mixed
type Difficulty = Easy | Medium | Hard | Deadly | Ludicrous
type EncounterGenerator = Xanathar of XanatharDifficulty | DMG of Difficulty | ShiningSword of Difficulty
type Analysis = PureCR | Encounter of EncounterGenerator
let wizard = 
    choose [
        c PureCR
        ctor("Encounter", c Encounter,
            choose [
                ctor("Xanathar", c Xanathar,
                    choose [c XanatharDifficulty.Easy; c XanatharDifficulty.Medium; c XanatharDifficulty.Hard]
                    )
                ctor("DMG/SS", choose [c DMG; c ShiningSword],
                    choose [c Easy; c Medium; c Hard; c Deadly; c Ludicrous]
                    )
                ])
        ]

// What is needed now is a way to determine when a given setting has been chosen,
// probably with some kind of threaded state that pattern can read from.
type StringRender() = 
    interface Render<string> with
        member this.Render v isSelected = sprintf "%s%A" (if isSelected then "+" else " ") v

let pmatch (pattern : IPatternMatch<'t>) (x : Setting<'t>) = x.Match pattern
let rec pattern<'t, 'out> (state: WizardState) (render:Render<'out>) =
    let assertOutputType x = x :> obj :?> 'output list // type system can't prove that 'output and 'out are the same type, so we assert it by casting because it always will be
    {
        new IPatternMatch<'t> with
            member __.Const x = Complete x, []
            member __.Choice options = 
                let currentIx = state |> Map.tryFind (options.GetHashCode())
                let current = currentIx |> Option.bind (fun ix -> if ix < options.Length then options.[ix] |> Some else None)
                let elements = [
                    for o in options do
                        yield (render.Render o (current = Some o))
                    ]
                match current with
                | Some (child: Setting<'t>) -> 
                    let (r:'t LifecycleStage), childElements = eval child state render
                    r, (assertOutputType elements)@(assertOutputType childElements)
                | None -> Unset, assertOutputType elements
            member __.App1 f x = 
                match (eval f state render), (eval x state render) with
                | (Complete f, e1s), (Complete x, e2s) ->
                    Complete (f x), assertOutputType (e1s@e2s)
                | (Unset, e1s), _ ->
                    Unset, assertOutputType e1s
                | (_, e1s), _ -> Set, assertOutputType e1s
            member __.App2 f arg arg2 = notImpl()
    }

and eval<'t, 'output> (setting : Setting<'t>) (state: WizardState) (render:Render<'output>): 't LifecycleStage * 'output list = pmatch (pattern<'t, 'output> state render) setting
