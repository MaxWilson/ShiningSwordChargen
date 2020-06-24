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
        abstract Match : IPatternMatch<'t, 'R> -> 'R

    // instaces of IPatternMatch encode a match expression

    and IPatternMatch<'t, 'R> =
        abstract Const : 't -> 'R
        abstract Add : Expr<int> -> Expr<int> -> 'R
        abstract IfThenElse : Expr<bool> -> Expr<'t> -> Expr<'t> -> 'R
        abstract App<'S> : Expr<'S -> 't> -> Expr<'S> -> 'R
        abstract Lam<'t1, 't2> : (Expr<'t1> -> Expr<'t2>) -> 'R
        abstract Fix<'t1, 't2> : Expr<('t1 -> 't2) -> 't1 -> 't2> -> 'R

    // concrete case implementations

    type internal Const<'t>(value : 't) =
        inherit Expr<'t> ()
        override __.Match (m : IPatternMatch<'t, 'R>) = m.Const value

    type internal Add(left : Expr<int>, right : Expr<int>) =
        inherit Expr<int> ()
        override __.Match (m : IPatternMatch<int, 'R>) = m.Add left right

    type internal IfThenElse<'t>(b : Expr<bool>, l : Expr<'t>, r : Expr<'t>) =
        inherit Expr<'t> ()
        override __.Match (m : IPatternMatch<'t, 'R>) = m.IfThenElse b l r

    type internal App<'t,'S> (f : Expr<'S -> 't>, x : Expr<'S>) =
        inherit Expr<'t> ()
        override __.Match (m : IPatternMatch<'t, 'R>) = m.App f x

    type internal Lam<'t1,'t2> (f : Expr<'t1> -> Expr<'t2>) =
        inherit Expr<'t1 -> 't2> ()
        override __.Match (m : IPatternMatch<'t1 -> 't2, 'R>) = m.Lam f

    type internal Fix<'t, 'S> (f : Expr<('t -> 'S) -> 't -> 'S>) =
        inherit Expr<'t -> 'S> ()
        override __.Match (m : IPatternMatch<'t -> 'S, 'R>) = m.Fix f

    // constructor api
    let constant x = Const<_>(x) :> Expr<_>
    let add x y = Add(x,y) :> Expr<_>
    let ifThenElse b l r = IfThenElse<_>(b,l,r) :> Expr<_>
    let app f x = App<_,_>(f,x) :> Expr<_>
    let lam f = Lam<_,_>(f) :> Expr<_>
    let fix f = Fix<_,_>(f) :> Expr<_>

    let pmatch (pattern : IPatternMatch<'t, 'R>) (x : Expr<'t>) = x.Match pattern

    // example : implement evaluator using pattern match

    let cast (x : 't) = x :> obj :?> 'S

    let rec pattern<'t> =
        {
            new IPatternMatch<'t, 't> with
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

module Debug = 
    type Expr<'t> =
        abstract member Match: IPatternMatch<'t, 'r> ->'r
    and IPatternMatch<'t, 'r> =
        abstract member Const: 't -> 'r
        abstract member Choice: Expr<'t> list -> 'r
        abstract member App<'s> : Expr<'s -> 't> -> Expr<'s> -> 'r
    let pmatch (pattern : IPatternMatch<'t, 'R>) (x : Expr<'t>) = x.Match pattern
    let rec pattern<'t> =
        {
            new IPatternMatch<'t, 't> with
                member __.Const x = x
                member __.Choice options = notImpl()
                member __.App f x = 
                    //(eval f) (eval x)
                    let x = eval f
                    notImpl()
        }

    and eval<'t> (expr : Expr<'t>) : 't = pmatch pattern<'t> expr

    
//type Setting<'t> =
//    Const<'t>: 't -> Setting<'t>
//    Choice : Setting<'t List> -> Setting<'t>
//    App1 :    Setting<'S -> 't> -> Setting<'S> -> Setting<'t>
//    App1 :    Setting<'S1*'S2 -> 't> -> Setting<'S1> -> Setting<'S2> -> Setting<'t>
// 'R is a "free" variable to make GDT eval work, will be constrained to be equal to 't but
//    should not be referenced directly in 
type ISetting<'t> =
    abstract member Match: IPatternMatch<'t, 'r> ->'r
and IPatternMatch<'t, 'r> =
    abstract member Const: 't -> 'r
    abstract member Choice: ISetting<'t> list -> 'r
    abstract member App<'s> : ISetting<'s -> 't> * ISetting<'s> -> 'r
type Render<'output> = 
    abstract member Render: 't1 -> isSelected:bool -> 'output
and HashCode = int
and PatternState = Map<HashCode, obj>

let pmatch (pattern : IPatternMatch<'t, 'r>) (x : ISetting<'t>) = x.Match pattern
let rec pattern<'t> =
    {
        new IPatternMatch<'t, 't> with
            member __.Const x = x
            member __.Choice options = notImpl()
            member __.App(f, arg) =
                let x = eval arg
                let y = eval f
                notImpl()
    }
and eval<'t> (expr : ISetting<'t>) : 't = pmatch pattern<'t> expr

let pmatch (pattern : IPatternMatch<'t, 'r>) (x : ISetting<'t>) = x.Match pattern
let rec pattern<'t, 'state> (state: 'state) =
    {
            new IPatternMatch<'t, 't> with
                member __.Const x = x
                member __.Choice options = notImpl()
                    //let current = (state: Map<int, obj>) |> Map.tryFind (options.GetHashCode()) |> Option.bind (function :? ISetting<'t> as v -> Some v | _ -> None)
                    //let elements = [
                    //    ]
                    //match current with
                    //| Some (child: ISetting<'t>) -> 
                    //    let (r:'t LifecycleStage), childElements = eval state child
                    //    r, elements@childElements
                    //| None -> Unset, elements
                member __.App(f, arg) =
                    let x = eval(arg, state)
                    let y = eval(f, state)
                    notImpl()
                //member __.App2 f arg1 arg2 = notImpl()
        }
and eval<'t, 'state> (setting : ISetting<'t>, state: 'state) : 't = 
    pmatch (pattern<'t, 'state> state) setting

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
let pmatch (pattern : IPatternMatch<'t, 'r, string>) (x : ISetting<'t>) = x.Match pattern
let rec eval<'t> (setting : ISetting<'t>) : 't LifecycleStage * _ = 
    pmatch (pattern<'t>) setting
and pattern<'t> =
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
                member __.App1 (f: ISetting<'s -> 't>) (arg: ISetting<'s>) =
                    let x = f |> eval
                    //let y = eval<'s> arg

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
let set = c (fun x -> x + 1)
let y = eval set
eval wizard
eval (c "hello") 
eval (c 123) 
let choices = [c 123; c 456]
let mySetting = choose choices
eval mySetting (Map.ofSeq []) (StringRender())
eval mySetting (Map.ofSeq [choices.GetHashCode(), (box choices.Head)]) (StringRender())
eval (c (fun x -> x + 1)) Map.empty (StringRender())

