module UI.Chargen
open Optics
open Optics.Operations
open Domain
open Domain.Model
open Feliz

type Sex = Male | Female
type EditMode = Rearranging | Renaming | AssigningFeats | Nil
type State = {
    sex: Sex option
    stats: Stats option
    name: string option
    editMode: EditMode
    }
    with static member fresh = { sex = None; stats = None; name = None; editMode = Nil }

let stats_ = lens (fun (d: State) -> d.stats) (fun v d -> { d with stats = v })
let sex_ = lens (fun (d: State) -> d.sex) (fun v d -> { d with sex = v })
let name_ = lens (fun (d: State) -> d.name) (fun v d -> { d with name = v })
let renaming_ = lens (fun (d: State) -> d.editMode) (fun v d -> { d with editMode = v })

let generateName sex state =
    let name =
        match sex with
        | Female -> chooseRandom ["Diana"; "Kiera"; "Kelsey"; "Samantha"; "Alexandra"; "Cleo"; "Berlin"; "Jenny"; "Katherine"] |> Some
        | Male -> chooseRandom ["Ryan"; "Theodore"; "Sam"; "Alex"; "Max"; "Dante"; "Zorro"; "Vlad"] |> Some
    state |> write name_ name

type 'model API = {
    state_: Lens<'model, State>
    //name: Lens<'model, string>
    updateCmd: ('model -> 'model) -> unit
    //doneCmd: 'dispatch -> unit
    }

let view (api: API<_>) model =
    let state = model |> read api.state_
    let stats = api.state_ => stats_
    let update t = api.updateCmd (over api.state_ t)
    let set (lens: Lens<_,_>) v = api.updateCmd (write (api.state_ => lens) (Some v))
    React.fragment [
        Html.h1 "New character"
        match state.sex with
        | None ->
            Html.div "Choose sex"
            Html.button [
                prop.onClick (fun _ -> (set sex_ Male))
                prop.text "Male"
                ]
            Html.button [
                prop.onClick (fun _ -> (set sex_ Female))
                prop.text "Female"
                ]
        | Some sex ->
            Html.div "Choose stat rolling method"
            match state.stats with
            | None ->
                Html.button [
                    prop.onClick (fun _ -> update (write stats_ (Domain.Chargen.roll3d6InOrder() |> Some) >> generateName sex))
                    prop.text "Roll 3d6"
                    ]
                Html.button [
                    prop.onClick (fun _ -> update (write stats_ (Domain.Chargen.roll4d6k3() |> Some) >> generateName sex))
                    prop.text "Roll 4d6k3"
                    ]
                Html.button [
                    prop.onClick (fun _ -> update (write stats_ (Domain.Chargen.standardArray() |> Some) >> generateName sex))
                    prop.text "Standard array"
                    ]
            | Some stats ->
                if state.editMode = Renaming then
                    Html.form [
                        prop.children [
                            Html.div [
                                prop.style [style.display.flex]
                                prop.children [
                                    Html.input [prop.value state.name.Value; prop.onChange (fun (name': string) -> update (write name_ (Some name')))]
                                    Html.button [prop.text "OK"; prop.type'.submit]
                                    ]
                                ]
                            ]
                        prop.onSubmit (fun ev -> ev.preventDefault(); update (write renaming_ Nil))
                        ]
                else
                    Html.div [
                        prop.style [
                            style.display.flex
                            ]
                        prop.children [
                            Html.div (sprintf "%s (%A)" state.name.Value sex)
                            Html.button [prop.text "Rename"; prop.onClick (fun _ -> update (write renaming_ Renaming)); prop.style [style.marginLeft 10]]
                            ]
                        ]
                for st in Stat.values do
                    Html.div (sprintf "%s: %d" (Stat.toString st) (stats |> read (Stat.lens st)))
                Html.button [
                    prop.onClick (fun _ -> update (thunk State.fresh))
                    sprintf "Abandon %s" (defaultArg state.name "") |> prop.text
                    ]
        ]