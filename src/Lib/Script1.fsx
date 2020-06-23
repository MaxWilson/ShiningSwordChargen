#I __SOURCE_DIRECTORY__
#load @"Common.fs"
#load @"Optics.fs"

module Example =
    // Source: http://www.fssnip.net/mp/title/An-attempt-at-encoding-GADTs

    //type Expr<'T> =
    //    Const : 'T -> Expr<'T>
    //    Add : Expr<int> -> Expr<int> -> Expr<int>
    //    IfThenElse : Expr<bool> -> Expr<'T> -> Expr<'T> -> Expr<'T>
    //    App : Expr<'T -> 'S> -> Expr<'T> -> Expr<'S>
    //    Lam : (Expr<'T> -> Expr<'S>) -> Expr<'T -> 'S>
    //    Fix : Expr<('T -> 'S) -> 'T -> 'S> -> Expr<'T -> 'S>

    [<AbstractClass>]
    type Expr<'T> internal () =
        abstract Match : IPatternMatch<'T, 'R> -> 'R

    // instaces of IPatternMatch encode a match expression

    and IPatternMatch<'T, 'R> =
        abstract Const : 'T -> 'R
        abstract Add : Expr<int> -> Expr<int> -> 'R
        abstract IfThenElse : Expr<bool> -> Expr<'T> -> Expr<'T> -> 'R
        abstract App<'S> : Expr<'S -> 'T> -> Expr<'S> -> 'R
        abstract Lam<'T1, 'T2> : (Expr<'T1> -> Expr<'T2>) -> 'R
        abstract Fix<'T1, 'T2> : Expr<('T1 -> 'T2) -> 'T1 -> 'T2> -> 'R

    // concrete case implementations

    type internal Const<'T>(value : 'T) =
        inherit Expr<'T> ()
        override __.Match (m : IPatternMatch<'T, 'R>) = m.Const value

    type internal Add(left : Expr<int>, right : Expr<int>) =
        inherit Expr<int> ()
        override __.Match (m : IPatternMatch<int, 'R>) = m.Add left right

    type internal IfThenElse<'T>(b : Expr<bool>, l : Expr<'T>, r : Expr<'T>) =
        inherit Expr<'T> ()
        override __.Match (m : IPatternMatch<'T, 'R>) = m.IfThenElse b l r

    type internal App<'T,'S> (f : Expr<'S -> 'T>, x : Expr<'S>) =
        inherit Expr<'T> ()
        override __.Match (m : IPatternMatch<'T, 'R>) = m.App f x

    type internal Lam<'T1,'T2> (f : Expr<'T1> -> Expr<'T2>) =
        inherit Expr<'T1 -> 'T2> ()
        override __.Match (m : IPatternMatch<'T1 -> 'T2, 'R>) = m.Lam f

    type internal Fix<'T, 'S> (f : Expr<('T -> 'S) -> 'T -> 'S>) =
        inherit Expr<'T -> 'S> ()
        override __.Match (m : IPatternMatch<'T -> 'S, 'R>) = m.Fix f

    // constructor api
    let constant x = Const<_>(x) :> Expr<_>
    let add x y = Add(x,y) :> Expr<_>
    let ifThenElse b l r = IfThenElse<_>(b,l,r) :> Expr<_>
    let app f x = App<_,_>(f,x) :> Expr<_>
    let lam f = Lam<_,_>(f) :> Expr<_>
    let fix f = Fix<_,_>(f) :> Expr<_>

    let pmatch (pattern : IPatternMatch<'T, 'R>) (x : Expr<'T>) = x.Match pattern

    // example : implement evaluator using pattern match

    let cast (x : 'T) = x :> obj :?> 'S

    let rec pattern<'T> =
        {
            new IPatternMatch<'T, 'T> with
                member __.Const x = x
                member __.Add x y = cast(eval x + eval y)
                member __.IfThenElse b x y = if eval b then eval x else eval y
                member __.App f x = (eval f) (eval x)
                member __.Lam f = cast(eval << f << constant)
                member __.Fix f = cast(eval f (fun x -> eval (fix f) x))
        }

    and eval<'T> (expr : Expr<'T>) : 'T = pmatch pattern<'T> expr

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

// Setting lifecycle: unset, set, complete
type 't LifecycleStage = Unset | Set | Complete of 't
    with static member map f = function Complete v -> Complete (f v) | v -> v

//type Setting<'T> =
//    Const<'T>: 'T -> Setting<'T>
//    Choice : Setting<'T List> -> Setting<'T>
//    App1 :    Setting<'S -> 'T> -> Setting<'S> -> Setting<'T>
//    App1 :    Setting<'S1*'S2 -> 'T> -> Setting<'S1> -> Setting<'S2> -> Setting<'T>
// 'R is a "free" variable to make GDT eval work, will be constrained to be equal to 'T but
//    should not be referenced directly in 
type ISetting<'t> =
    abstract member Match: (IPatternMatch<'t,'r>) -> 'r LifecycleStage
and IPatternMatch<'t,'r> =
    abstract member Const: 't -> 'r LifecycleStage
    abstract member Choice: ISetting<'t> list -> 'r LifecycleStage
    abstract member App1: ISetting<'s -> 't> -> ISetting<'s> -> 'r LifecycleStage
    abstract member App2: ISetting<'s1*'s2 -> 't> -> ISetting<'s1> -> ISetting<'s2> -> 'r LifecycleStage
type SettingConst<'t>(v: 't) =
    interface ISetting<'t> with
        member this.Match(m) = m.Const v
type SettingChoice<'t>(values: ISetting<'t> list) =
    interface ISetting<'t> with
        member this.Match(m) = m.Choice values
// returns a value only once the user has picked a value
type SettingCtor<'t,'s>(ctor: ISetting<'s -> 't>, arg: ISetting<'s>) =
    interface ISetting<'t> with
        member this.Match(m: IPatternMatch<'t,'r>) = m.App1 ctor arg
// returns a value only once the user has picked a value
type SettingCtor2<'t,'s1,'s2>(ctor: ISetting<'s1*'s2 -> 't>, arg1: ISetting<'s1>, arg2: ISetting<'s2>) =
    interface ISetting<'t> with
        member this.Match(m: IPatternMatch<'t,'r>) = m.App2 ctor arg1 arg2
let c v = SettingConst(v) :> ISetting<_>
let choose options = SettingChoice(options) :> ISetting<_>
let ctor(f, arg)= SettingCtor(f,arg) :> ISetting<_>
let ctor2(f, arg1, arg2)= SettingCtor2(f, arg1, arg2) :> ISetting<_>
let both(arg1, arg2) = ctor2(c id, arg1, arg2)
type XanatharDifficulty = Easy | Medium | Hard
type XanatharType = Solo | Group | Mixed
type Difficulty = Easy | Medium | Hard | Deadly | Ludicrous
type EncounterGenerator = Xanathar of XanatharDifficulty | DMG of Difficulty | ShiningSword of Difficulty
type Analysis = PureCR | Encounter of EncounterGenerator
let wizard = 
    choose [
        c PureCR
        ctor(c Encounter,
            choose [
                ctor(c Xanathar,
                    choose [c XanatharDifficulty.Easy; c XanatharDifficulty.Medium; c XanatharDifficulty.Hard]
                    )
                ctor(choose [c DMG; c ShiningSword],
                    choose [c Easy; c Medium; c Hard; c Deadly; c Ludicrous]
                    )
                ])
        ]

let pmatch (pattern : IPatternMatch<'t, 'r>) (x : ISetting<'t>) = x.Match pattern

let rec pattern<'t> =
    {
        new IPatternMatch<'t, 't> with
            member __.Const x = Complete x
            member __.Choice x = notImpl()
            member __.App1 f arg = notImpl()
            member __.App2 f arg1 arg2 = notImpl()
    }
and eval<'t> (setting : ISetting<'t>) : 't LifecycleStage = pmatch pattern<'t> setting