﻿module UI.Chargen
open Optics
open Optics.Operations
open Domain
open Domain.Model
open Feliz
open AutoWizard

module NewCharacter =
    type DetailLevel = | Template of templateName: string | Custom
    type Name = string
    type Spec = DetailLevel * (Sex * Name)

let tup x y = ctor2("Invisible", c (fun(x,y) -> (x,y)), x, y)
let wizard = tup (choose [c "Template"; c "Custom"]) (tup (choose [c Male; c Female]) (choose [c "Barbarian"; c "Samurai"]))

type ViewMode = Creating | Selecting | Viewing
type EditMode = Rearranging | Renaming | AssigningFeats
type WizardChoices = Map<HashCode, ChoiceState>
type State = {
    sex: Sex option
    stats: Stats option
    name: string option
    viewMode: ViewMode option
    editMode: EditMode option
    wizardChoices: WizardChoices
    }
    with static member fresh = { sex = None; stats = None; name = None; viewMode = None; editMode = None; wizardChoices = Map.empty }

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
let wizardChoices_ = lens (fun (d: State) -> d.wizardChoices) (fun v d -> { d with wizardChoices = v })

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

let renderWizard (api: API<'model>) model setting =
    let render =
        {
        new AutoWizard.Render<'model, ReactElement> with
        member this.Render options lens = [
                Html.div [
                    let currentChoice = model |> read lens
                    for ix, o in options |> List.indexed do
                        Html.button [
                            prop.text (o.ToString());
                            prop.style [if Some (ChoiceIndex ix) = currentChoice then style.color.red]
                            prop.onClick (fun ev -> ev.preventDefault(); api.updateCmd(write lens (ChoiceIndex ix |> Some)))
                            ]
                        ]
            ]
        }
    let getLens hashCode =
        let hash_ =
            Lens.create
                (Map.tryFind hashCode)
                (function None -> Map.remove hashCode | Some v -> Map.add hashCode v)
        api.chargen_ => wizardChoices_ => hash_

    AutoWizard.eval(setting, getLens, render, model)

let view (api: API<_>) (model: 'model) =
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

            let choice, elements = renderWizard api model wizard
            match choice with
            | Unset | Set ->
                React.fragment elements
            | Complete _ ->
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
                        viewCharacter api (api.chargen_ => name_ => Option.some__) sex stats state.editMode model
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
            let ix = model |> read api.currentIndex_ |> Option.get
            let roster = (model |> read api.roster_)
            if ix >= roster.Length then shouldntHappen()
            let sheet = roster |> read (List.nth__ ix => Creature.stats_ => StatSource.charSheet_) |> Option.get
            let sex = sheet.sex
            viewCharacter api (api.roster_ => List.nth__ (model |> read api.currentIndex_ |> Option.get) => Creature.name_) sex sheet.statBlock.stats state.editMode model
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