#I __SOURCE_DIRECTORY__
#load @"Common.fs"
#load @"Optics.fs"

[<AbstractClass>]
type Setting<'t>() =
    abstract Visit: IVisitor<'t, 'r> -> 'r
and IVisitor<'t, 'r> =
    abstract member Leaf: 't -> 'r
    abstract member Node: ('t * Setting<'t> option * Setting<'t> option) -> 'r
type Leaf<'t>(d) =
    inherit Setting<'t>()
    override this.Visit(v: IVisitor<_,_>) = v.Leaf d
type Node<'t>(d, l, r) =
    inherit Setting<'t>()
    override this.Visit(v: IVisitor<_,_>) = v.Node (d, l, r)

let leaf v =
    Leaf(v) :> Setting<_> |> Some
let node(v, l, r) = (v, l, r) |> Node :> Setting<_> |> Some
let tree (Some n) = n
let settings = 
    (node(5, node(42, leaf 4, None), node(4, None, None)))
let rec extract (v: Setting<_>) =
    v.Visit <|
        { new IVisitor<_, _> with
            member this.Leaf v = v
            member this.Node args =
                match args with
                | _, Some v, _ -> extract v
                | _, _, Some v -> extract v
                | v, None, None -> v
                }
tree(node(5, node(42, leaf 4, None), node(7, None, None))) |> extract
tree(node(5, node(42, None, None), node(7, None, None))) |> extract
tree(node(5, None, node(7, None, None))) |> extract
tree(node(5, None, None)) |> extract

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

type ISetting<'t> =
    abstract member Value: (#ISettingVisitor<'t>) -> 't LifecycleStage
and ISettingVisitor<'t> =
    abstract member Visit: ISetting<'t> -> 't LifecycleStage
type SettingChoice<'t>(values: ISetting<'t> list) =
    interface ISetting<'t> with
        member this.Value(v) = v.Visit values.Head
type SettingCtor<'t,'r>(value: ISetting<'t>, ctor: 't -> 'r) =
    interface ISetting<'r> with
        member this.Value(v) = value |> LifecycleStage.map ctor


