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

let notImpl() = failwith "Not implemented"
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
    abstract member Match: IPatternMatch<'t, 'r, 'output> ->'r LifecycleStage * 'output list
and Render<'output> = 
    abstract member Render: 't1 -> isSelected:bool -> 'output
and HashCode = int
and PatternState = Map<HashCode, obj>
and IPatternMatch<'t,'r, 'output> =
    abstract member Const: 't -> 'r LifecycleStage * 'output list
    abstract member Choice: ISetting<'t> list -> 'r LifecycleStage * 'output list
    abstract member App1: ISetting<'s -> 't> -> ISetting<'s> -> 'r LifecycleStage * 'output list
    abstract member App2: ISetting<'s1*'s2 -> 't> -> ISetting<'s1> -> ISetting<'s2> -> 'r LifecycleStage * 'output list
let compose render children (input: 'r LifecycleStage) =
    input, [render input]@children
type SettingConst<'t>(v: 't) =
    interface ISetting<'t> with
        member this.Match m = m.Const v
type SettingChoice<'t>(values: ISetting<'t> list) =
    interface ISetting<'t> with
        member this.Match m = m.Choice values
// returns a value only once the user has picked a value
type SettingCtor<'t,'s>(ctor: ISetting<'s -> 't>, arg: ISetting<'s>) =
    interface ISetting<'t> with
        member this.Match m = m.App1 ctor arg
// returns a value only once the user has picked a value
type SettingCtor2<'t,'s1,'s2>(ctor: ISetting<'s1*'s2 -> 't>, arg1: ISetting<'s1>, arg2: ISetting<'s2>) =
    interface ISetting<'t> with
        member this.Match m = m.App2 ctor arg1 arg2
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

// What is needed now is a way to determine when a given setting has been chosen,
// probably with some kind of threaded state that pattern can read from.
type StringRender() = 
    interface Render<string> with
        member this.Render v isSelected = sprintf "%s%A" (if isSelected then "+" else " ") v
let render = StringRender() :> Render<string>
let state : PatternState = Map.empty
let rec eval (setting : ISetting<'t>) : 't LifecycleStage * _ = 
    setting.Match 
        {
            new IPatternMatch<'t, 't, string> with
                member __.Const x = Complete x, []
                member __.Choice options = 
                    let current = state |> Map.tryFind (options.GetHashCode()) |> Option.bind (function :? ISetting<'t> as v -> Some v | _ -> None)
                    let elements = [
                        for o in options do
                            yield render.Render o (current = Some o)
                        ]
                    match current with
                    | Some (child: ISetting<'t>) -> 
                        let (r:'t LifecycleStage), childElements = eval child
                        r, elements@childElements
                    | None -> Unset, elements
                member __.App1 f arg =
                    let set = c (fun x -> x + 1)
                    let y = eval set
                    //match eval f, eval arg with
                    //| (Complete f, children1), (Complete arg, children2) ->
                    //    //Complete (f arg), children1@children2
                    //    notImpl()
                    //| _ -> Unset, []
                    notImpl()
                    //let current = state |> Map.tryFind (options.GetHashCode()) |> Option.bind (function :? ISetting<'t> as v -> Some v | _ -> None)
                    //eval 
                    //let elements = [
                    //    for o in options do
                    //        yield render.Render o (current = Some o)
                    //    ]
                    //let result = 
                    //    match current with
                    //    | Some v -> Complete (unbox<'t> v)
                    //    | None -> Unset
                    //result, elements
    
                member __.App2 f arg1 arg2 = notImpl()
        }
eval wizard Map.empty (StringRender())
eval (c "hello") Map.empty (StringRender())
eval (c 123) Map.empty (StringRender())
let choices = [c 123; c 456]
let mySetting = choose choices
eval mySetting (Map.ofSeq []) (StringRender())
eval mySetting (Map.ofSeq [choices.GetHashCode(), (box choices.Head)]) (StringRender())
eval (c (fun x -> x + 1)) Map.empty (StringRender())

