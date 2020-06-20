module UI.Chargen
open Optics
open Optics.Operations
open Domain
open Domain.Model
open Feliz

type ViewMode = Creating | Selecting | Viewing
type EditMode = Rearranging | Renaming | AssigningFeats

type State = {
    sex: Sex option
    stats: Stats option
    name: string option
    viewMode: ViewMode option
    editMode: EditMode option
    }
    with static member fresh = { sex = None; stats = None; name = None; viewMode = None; editMode = None }

let toCharSheet (state: State) : Creature =
    {
        name = state.name.Value
        stats = CharSheet {
            statBlock = {
                Model.StatBlock.stats = state.stats.Value
                hp = 20
                ac = 10
                }
            xp = 0
            sex = state.sex.Value
            yearOfBirth = 420
            }
    }

let stats_ = lens (fun (d: State) -> d.stats) (fun v d -> { d with stats = v })
let sex_ = lens (fun (d: State) -> d.sex) (fun v d -> { d with sex = v })
let name_ = lens (fun (d: State) -> d.name) (fun v d -> { d with name = v })
let viewMode_ = lens (fun (d: State) -> d.viewMode) (fun v d -> { d with viewMode = v })
let editMode_ = lens (fun (d: State) -> d.editMode) (fun v d -> { d with editMode = v })

let generateName sex state =
    let name =
        match sex with
        | Female -> chooseRandom ["Diana"; "Kiera"; "Kelsey"; "Samantha"; "Alexandra"; "Cleo"; "Berlin"; "Jenny"; "Katherine"] |> Some
        | Male | Neither -> chooseRandom ["Ryan"; "Theodore"; "Sam"; "Alex"; "Max"; "Dante"; "Zorro"; "Vlad"] |> Some
    state |> write name_ name

type 'model API = {
    chargen_: Lens<'model, State>
    roster_: Lens<'model, Creature list>
    currentIndex_: Lens<'model, int option>
    //name: Lens<'model, string>
    updateCmd: ('model -> 'model) -> unit
    //doneCmd: 'dispatch -> unit
    }

let viewCharacter (api:API<_>) (name_: Lens<_,string>) sex stats mode model =
    let name = model |> read name_
    React.fragment [
        if mode = Some Renaming then
            Html.form [
                prop.children [
                    Html.div [
                        prop.style [style.display.flex]
                        prop.children [
                            Html.input [prop.value name; prop.onChange (write name_ >> api.updateCmd)]
                            Html.button [prop.text "OK"; prop.type'.submit]
                            ]
                        ]
                    ]
                prop.onSubmit (fun ev -> ev.preventDefault(); (write (api.chargen_ => editMode_) None) |> api.updateCmd)
                ]
        else
            Html.div [
                prop.style [
                    style.display.flex
                    ]
                prop.children [
                    Html.div (sprintf "%s (%A)" name sex)
                    Html.button [prop.text "Rename"; prop.onClick (fun _ -> write (api.chargen_ => editMode_) (Some Renaming) |> api.updateCmd); prop.style [style.marginLeft 10]]
                    ]
                ]
        for st in Stat.values do
            Html.div (sprintf "%s: %d" (Stat.toString st) (stats |> read (Stat.lens st)))
        ]

let view (api: API<_>) model =
    let state = model |> read api.chargen_
    let cancel =
        Html.button [
            prop.onClick (fun _ -> api.updateCmd(write (api.chargen_ => viewMode_) None >> write api.chargen_ State.fresh))
            prop.text "Cancel"
            ]

    React.fragment [
        match state.viewMode with
        | Some Creating ->
            let stats = api.chargen_ => stats_
            let update t = api.updateCmd (over api.chargen_ t)
            let set (lens: Lens<_,_>) v = api.updateCmd (write (api.chargen_ => lens) (Some v))

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
                cancel
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
                    cancel
                | Some stats ->
                    viewCharacter api (api.chargen_ => name_ => Option.someUnchecked_) sex stats state.editMode model
                    Html.button [
                        prop.onClick (fun _ -> api.updateCmd(fun model ->
                            model
                            |> over api.roster_ (fun entries -> (model |> read api.chargen_ |> toCharSheet)::entries)
                            |> write api.chargen_ State.fresh))
                        sprintf "Abandon %s" state.name.Value |> prop.text
                        ]
        | Some Selecting ->
            let selectFor ix (sheet: Creature) =
                Html.button [
                    prop.onClick (fun _ -> api.updateCmd(writeSome api.currentIndex_ ix >> writeSome (api.chargen_ => viewMode_) Viewing))
                    prop.text sheet.name
                    ]
            yield! model |> read api.roster_ |> List.mapi selectFor
            cancel
        | Some Viewing ->
            // todo: clean this up, too many mysteries here.
            let ix = model |> read api.currentIndex_ |> Option.get
            let creature_ = api.roster_ ?=> (List.nth_ ix)
            let sheet = model |> read (creature_ ?=> Creature.stats_ ?=> StatSource.charSheet_)
            let sex = sheet.Value.sex
            viewCharacter api (api.roster_ => List.nthUnchecked_ (model |> read api.currentIndex_ |> Option.get) => Creature.name_) sex sheet.Value.statBlock.stats state.editMode model
            cancel
        | None ->
            Html.button [
                prop.onClick (fun _ -> api.updateCmd(writeSome (api.chargen_ => viewMode_) Creating))
                prop.text "Create new character"
                ]
            Html.button [
                prop.onClick (fun _ -> api.updateCmd(writeSome (api.chargen_ => viewMode_) Selecting))
                prop.text "Resume with existing"
                ]
        ]