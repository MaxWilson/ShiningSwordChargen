module UI.Chargen
open Optics
open Optics.Operations
open Domain
open Domain.Model
open Feliz
open AutoWizard
open Domain.Model.Character
open Domain.Chargen
open Domain.CharacterSheet

module NewCharacter =
    type DetailLevel = | Template of templateName: string | Custom
    type Name = string
    type Spec = DetailLevel * (Sex * Name)

type ViewMode = Creating of Draft.CharacterSheet | Selecting | Viewing
type EditMode = Rearranging of Stat | Renaming
type WizardChoices = Map<HashCode, ChoiceState>
type State = {
    viewMode: ViewMode option
    editMode: EditMode option
    wizardChoices: WizardChoices
    }
    with static member fresh = { viewMode = None; editMode = None; wizardChoices = Map.empty }

let charSheet_P =
    prism (fun (d: State) ->
            match d.viewMode with
            | Some (Creating draft) -> Some draft
            | _ -> None)
        (fun v d ->
            match d.viewMode with
            | Some (Creating _) -> { d with viewMode = Some (Creating v) }
            | _ -> d)
let viewMode_ = lens (fun (d: State) -> d.viewMode) (fun v d -> { d with viewMode = v })
let editMode_ = lens (fun (d: State) -> d.editMode) (fun v d -> { d with editMode = v })
let wizardChoices_ = lens (fun (d: State) -> d.wizardChoices) (fun v d -> { d with wizardChoices = v })

type 'model API = {
    chargen_: Lens<'model, State>
    roster_: Lens<'model, Creature list>
    currentIndex_: Lens<'model, int option>
    //name: Lens<'model, string>
    updateCmd: ('model -> 'model) -> unit
    //doneCmd: 'dispatch -> unit
    }

let renderWizard (api: API<'model>) model setting =
    let render =
        {
        new AutoWizard.Render<'model, ReactElement> with
        member this.RenderChoice options lens = [
                Html.div [
                    prop.className "choices"
                    prop.children [
                        let currentChoice = model |> read lens
                        for ix, o in options |> List.indexed do
                            Html.button [
                                prop.text (o.ToString());
                                prop.classes (if Some (ChoiceIndex ix) = currentChoice then ["chosen"; "choice"] else ["choice"])
                                prop.onClick (fun ev -> ev.preventDefault(); api.updateCmd(write lens (ChoiceIndex ix |> Some)))
                                ]
                            ]
                        ]
            ]
        member this.RenderChoiceDistinctN options n lens = [
                Html.div [
                    prop.className "choices"
                    prop.children [
                        let currentChoices = match model |> read lens with Some (MultichoiceIndex ixs) -> ixs | _ -> []
                        let toggle ix current =
                            if current |> List.exists ((=) ix) then current |> List.filter ((<>)ix)
                            elif current.Length = n then current
                            else ix::current
                        Html.text (sprintf "Choose %d:" n)
                        for ix, o in options |> List.indexed do
                            Html.button [
                                prop.text (o.ToString());
                                prop.classes (if currentChoices |> List.exists ((=) ix) then ["chosen"; "choice"] else ["choice"])
                                prop.onClick (fun ev -> ev.preventDefault(); api.updateCmd(write lens (MultichoiceIndex (currentChoices |> toggle ix) |> Some)))
                                ]
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

let tryEval api model setting =
    let getLens hashCode =
        let hash_ =
            Lens.create
                (Map.tryFind hashCode)
                (function None -> Map.remove hashCode | Some v -> Map.add hashCode v)
        api.chargen_ => wizardChoices_ => hash_
    let trivialRender =
        {
            new AutoWizard.Render<'model, unit> with
                member this.RenderChoice options lens = []
                member this.RenderChoiceDistinctN options n lens = []
        }
    match AutoWizard.eval(setting, getLens, trivialRender, model) with
    | Complete v, _ -> Some v
    | _ -> None

module Stats =
    let currentStats (unmodifiedStats: Stats) _traits = unmodifiedStats
    let view (current: Stats) (unmodified: Stats) =
        Html.div [
            prop.className "stats"
            prop.children [
                for stat in Stat.values do
                    let lens = Stat.lenses.[Stat.toTag stat]
                    let label = Stat.toString stat
                    Html.span[prop.className "statLabel"; prop.text (sprintf "%s: " label)]
                    Html.span[prop.className "statValue"; prop.text (current |> read lens)]
                    Html.span[prop.className "statUnmodifiedValue"; prop.text (unmodified |> read lens |> sprintf "(was %d)")]
                ]
            ]

let viewAndEditCharacter (api:API<_>) (model: 'model) (sheet: Draft.CharacterSheet) =
    Html.div [
        prop.className "characterView editing"
        prop.children [
            let stats = sheet.unmodifiedStats
            let update t = api.updateCmd (over api.chargen_ t)
            let set (lens: Lens<_,_>) v = api.updateCmd (write (api.chargen_ => lens) (Some v))
            let inline eval prevElements setting =
                let v, elements = renderWizard api model setting
                v, elements@prevElements
            let sexChoice, elements = sheet.sex |> eval []
            let raceChoice, elements = sheet.race |> eval elements
            let classFeatures = Domain.Chargen.classFeatures (Domain.Chargen.expandClasses [Barbarian, 20])
            let classFeatureChoice, elements = classFeatures |> List.fold (fun (accum, elements) setting -> setting |> eval elements |> fun (v, elements) -> v::accum, elements) ([], elements)
            Html.div [prop.className "characterName";defaultArg sheet.explicitName sheet.autoName |> prop.text]
            Html.button [prop.text "Rename"; prop.onClick (fun _ -> api.updateCmd(writeSome (api.chargen_ => editMode_) Renaming))]
            Stats.view (Stats.currentStats stats ()) (stats)
            yield! elements
            // only only to proceed if all settings are set
            match sexChoice, raceChoice, classFeatureChoice |> List.every (function Complete _ -> true | _ -> false) with
            | Complete _, Complete _, true ->
                Html.button [prop.text "OK"]
            | _ -> ()
        ]
    ]

let rename<'model> = React.functionComponent(fun (model, api:API<'model>, sheet:Draft.CharacterSheet) ->
    Html.form [
        let name', updateName' = React.useState (thunk (defaultArg sheet.explicitName sheet.autoName))
        let tryEval = tryEval api model
        prop.className "simpleDialog"
        prop.children [
            Html.span "New name:"
            Html.input[prop.onChange updateName'; prop.value name'; prop.autoFocus true]
            Html.button [prop.text "OK"; prop.type'.submit]
            Html.button [
                prop.className "separateLine";
                prop.onClick (fun ev -> ev.preventDefault(); updateName' (Draft.autoName tryEval sheet))
                prop.text "Autogenerate"
                ]
            Html.button [
                prop.className "separateLine";
                prop.onClick (fun ev -> ev.preventDefault(); api.updateCmd(write (api.chargen_ => editMode_) None))
                prop.text "Cancel"
                ]
            ]
        prop.onSubmit(fun ev -> ev.preventDefault(); api.updateCmd(writeSome (api.chargen_ => charSheet_P => CharacterSheet.explicitName_) name' >> write (api.chargen_ => editMode_) None))
        ])

let view (api: API<_>) (model: 'model) =
    let state = model |> read api.chargen_
    let cancel =
        Html.button [
            prop.onClick (fun _ -> api.updateCmd(write (api.chargen_ => viewMode_) None >> write api.chargen_ State.fresh))
            prop.text "Cancel"
            ]

    React.fragment [
        match model |> read (api.chargen_ => viewMode_) with
        | Some (Creating sheet) ->
            match state.editMode with
            | Some Renaming ->
                rename (model, api, sheet)
            | Some (Rearranging stat) -> ()
            | None ->
                viewAndEditCharacter api model sheet
                cancel
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
            Html.div (sprintf "Viewing character not implemented yet")
            cancel
        | None ->
            let tryEval = tryEval api model
            Html.button [
                prop.onClick (fun _ -> api.updateCmd(writeSome (api.chargen_ => viewMode_) (roll4d6k3() |> Draft.createBlank tryEval |> Creating)))
                prop.text "Create new character"
                ]
            Html.button [
                prop.onClick (fun _ -> api.updateCmd(writeSome (api.chargen_ => viewMode_) Selecting))
                prop.text "Resume with existing"
                ]
        ]