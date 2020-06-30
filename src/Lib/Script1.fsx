#I __SOURCE_DIRECTORY__
#load @"Common.fs"
#load @"Optics.fs"

module Example =
    // Source: http://www.fssnip.net/mp/title/An-attempt-at-encoding-GADTs

    //type Expr<'t> =
    //    Const : 't -> Expr<'t>
    //    Add : Expr<int> -> Expr<int> -> Expr<int>
    //    IfThenElse : Expr<bool> -> Expr<'t> -> Expr<'t> -> Expr<'t>
    //    App : Expr<'t -> 'S> -> Expr<'t> -> Expr<'S>
    //    Lam : (Expr<'t> -> Expr<'S>) -> Expr<'t -> 'S>
    //    Fix : Expr<('t -> 'S) -> 't -> 'S> -> Expr<'t -> 'S>

    [<AbstractClass>]
    type Expr<'t> internal () =
        abstract Match : IPatternMatch<'t> -> 't

    // instaces of IPatternMatch encode a match expression

    and IPatternMatch<'t> =
        abstract Const : 't -> 't
        abstract Add : Expr<int> -> Expr<int> -> 't
        abstract IfThenElse : Expr<bool> -> Expr<'t> -> Expr<'t> -> 't
        abstract App<'S> : Expr<'S -> 't> -> Expr<'S> -> 't
        abstract Lam<'t1, 't2> : (Expr<'t1> -> Expr<'t2>) -> 't
        abstract Fix<'t1, 't2> : Expr<('t1 -> 't2) -> 't1 -> 't2> -> 't

    // concrete case implementations

    type internal Const<'t>(value : 't) =
        inherit Expr<'t> ()
        override __.Match (m : IPatternMatch<'t>) = m.Const value

    type internal Add(left : Expr<int>, right : Expr<int>) =
        inherit Expr<int> ()
        override __.Match (m : IPatternMatch<int>) = m.Add left right

    type internal IfThenElse<'t>(b : Expr<bool>, l : Expr<'t>, r : Expr<'t>) =
        inherit Expr<'t> ()
        override __.Match (m : IPatternMatch<'t>) = m.IfThenElse b l r

    type internal App<'t,'S> (f : Expr<'S -> 't>, x : Expr<'S>) =
        inherit Expr<'t> ()
        override __.Match (m : IPatternMatch<'t>) = m.App f x

    type internal Lam<'t1,'t2> (f : Expr<'t1> -> Expr<'t2>) =
        inherit Expr<'t1 -> 't2> ()
        override __.Match (m : IPatternMatch<'t1 -> 't2>) = m.Lam f

    type internal Fix<'t, 'S> (f : Expr<('t -> 'S) -> 't -> 'S>) =
        inherit Expr<'t -> 'S> ()
        override __.Match (m : IPatternMatch<'t -> 'S>) = m.Fix f

    // constructor api
    let constant x = Const<_>(x) :> Expr<_>
    let add x y = Add(x,y) :> Expr<_>
    let ifThenElse b l r = IfThenElse<_>(b,l,r) :> Expr<_>
    let app f x = App<_,_>(f,x) :> Expr<_>
    let lam f = Lam<_,_>(f) :> Expr<_>
    let fix f = Fix<_,_>(f) :> Expr<_>

    let pmatch (pattern : IPatternMatch<'t>) (x : Expr<'t>) = x.Match pattern

    // example : implement evaluator using pattern match

    let cast (x : 't) = x :> obj :?> 'S

    let rec pattern<'t> =
        {
            new IPatternMatch<'t> with
                member __.Const x = x
                member __.Add x y = unbox(eval x + eval y)
                member __.IfThenElse b x y = if eval b then eval x else eval y
                member __.App f x = (eval f) (eval x)
                member __.Lam f = cast(eval << f << constant)
                member __.Fix f = cast(eval f (fun x -> eval (fix f) x))
        }

    and eval<'t> (expr : Expr<'t>) : 't = pmatch pattern<'t> expr

    // tests

    eval (app (lam (fun b -> ifThenElse b (constant 12) (constant 42))) (constant false))

    let multiply = 
        fix (lam(fun f -> 
            lam(fun n -> 
                ifThenElse (constant (eval n = 0)) 
                    (lam (fun _ -> constant 0)) 
                    (lam (fun m -> add m (app (app f (add n (constant -1))) m))))))

    eval (app (app multiply (constant 6)) (constant 7))

    eval (app (lam(fun n -> ifThenElse (constant (eval n < 10)) (constant "A") (constant "B"))) (constant 11))

let notImpl() = failwith "Not implemented"
// Setting lifecycle: unset, set, complete
type 't LifecycleStage = Unset | Set | Complete of 't
    with static member map f = function Complete v -> Complete (f v) | v -> v
    
//type Setting<'t> =
//    Const<'t>: 't -> Setting<'t>
//    Choice : Setting<'t List> -> Setting<'t>
//    App1 :    Setting<'S -> 't> -> Setting<'S> -> Setting<'t>
//    App1 :    Setting<'S1*'S2 -> 't> -> Setting<'S1> -> Setting<'S2> -> Setting<'t>
// 't is a "free" variable to make GDT eval work, will be constrained to be equal to 't but
//    should not be referenced directly in 
type ISetting<'t> =
    abstract member Match: IPatternMatch<'t> -> 't LifecycleStage * 'output list
and IPatternMatch<'t> =
    abstract member Const: 't -> 't LifecycleStage * 'output list
    abstract member Choice: ISetting<'t> list -> 't LifecycleStage * 'output list
    abstract member App1 : ISetting<'s -> 't> -> ISetting<'s> -> 't LifecycleStage * 'output list
    abstract member App2 : ISetting<'s1*'s2 -> 't> -> ISetting<'s1> -> ISetting<'s2> -> 't LifecycleStage * 'output list
type Render<'output> = 
    abstract member Render: 't1 -> isSelected:bool -> 'output
and HashCode = int
and PatternState = Map<HashCode, int>

let compose render children (input: 't LifecycleStage) =
    input, [render input]@children
type SettingConst<'t>(v: 't) =
    interface ISetting<'t> with
        member this.Match m = m.Const v
    override this.ToString() = sprintf "%A" v
type SettingChoice<'t>(values: ISetting<'t> list) =
    interface ISetting<'t> with
        member this.Match m = m.Choice values
    override this.ToString() = values |> List.map (fun v -> v.ToString()) |> fun vs -> System.String.Join(", ", vs) |> sprintf "[%s]"
// returns a value only once the user has picked a value
type SettingCtor<'t,'s>(label: string, ctor: ISetting<'s -> 't>, arg: ISetting<'s>) =
    interface ISetting<'t> with
        member this.Match m = m.App1 ctor arg
    override this.ToString() = label
// returns a value only once the user has picked a value
type SettingCtor2<'t,'s1,'s2>(label: string, ctor: ISetting<'s1*'s2 -> 't>, arg1: ISetting<'s1>, arg2: ISetting<'s2>) =
    interface ISetting<'t> with
        member this.Match m = m.App2 ctor arg1 arg2
    override this.ToString() = label
let c v = SettingConst(v) :> ISetting<_>
let choose options = SettingChoice(options) :> ISetting<_>
let ctor(label, f, arg)= SettingCtor(label,f,arg) :> ISetting<_>
let ctor2(label, f, arg1, arg2)= SettingCtor2(label, f, arg1, arg2) :> ISetting<_>
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

let pmatch (pattern : IPatternMatch<'t>) (x : ISetting<'t>) = x.Match pattern
let rec pattern<'t, 'out> (state: PatternState) (render:Render<'out>) =
    let assertOutputType x = x :> obj :?> 'output list // type system can't prove that 'output and 'out are the same type, so we assert it by casting because it always will be
    {
        new IPatternMatch<'t> with
            member __.Const x = Complete x, []
            member __.Choice options = 
                let currentIx = state |> Map.tryFind (options.GetHashCode())
                let current = currentIx |> Option.map (fun ix -> options.[ix])
                let elements = [
                    for o in options do
                        yield (render.Render o (current = Some o))
                    ]
                match current with
                | Some (child: ISetting<'t>) -> 
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

and eval<'t, 'output> (setting : ISetting<'t>) (state: PatternState) (render:Render<'output>): 't LifecycleStage * 'output list = pmatch (pattern<'t, 'output> state render) setting

eval wizard Map.empty (StringRender())
eval (c "hello") 
eval (c 123) 
let choices = [c 123; c 456]
let mySetting = choose choices
eval mySetting (Map.ofSeq []) (StringRender())
eval mySetting (Map.ofSeq [choices.GetHashCode(), 0]) (StringRender())
eval (c (fun x -> x + 1)) Map.empty (StringRender())

let choice3 = [c XanatharDifficulty.Easy; c XanatharDifficulty.Medium; c XanatharDifficulty.Hard]
let choice2 = [
    ctor("Xanathar", c Xanathar,
        choose choice3
        )
    ctor("DMG/SS", choose [c DMG; c ShiningSword],
        choose [c Easy; c Medium; c Hard; c Deadly; c Ludicrous]
        )
    ]
let choice1 = [
    c PureCR
    ctor("Encounter", c Encounter,
        choose choice2)
    ]
let wizard1 = 
    choose choice1

eval wizard1 (Map.ofSeq [choice1.GetHashCode(), 1]) (StringRender())
