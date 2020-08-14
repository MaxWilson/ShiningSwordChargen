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

type ViewMode = Creating of Draft.DraftSheet | Selecting | Viewing of Character.CharacterSheet
type WizardChoices = Map<ChoiceKey, ChoiceState>
type State = {
    viewMode: ViewMode option
    wizardChoices: WizardChoices
    }
    with static member fresh = { viewMode = None; wizardChoices = Map.empty }

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
let wizardChoices_ = lens (fun (d: State) -> d.wizardChoices) (fun v d -> { d with wizardChoices = v })

type 'model API = {
    chargen_: Lens<'model, State>
    roster_: Lens<'model, Character.CharacterSheet list>
    //name: Lens<'model, string>
    updateCmd: ('model -> 'model) -> unit
    modalDialog: Components.Launcher<'model, ReactElement>
    //doneCmd: 'dispatch -> unit
    }

let renderWizard (api: API<'model>) model setting =
    let render =
        {
        new AutoWizard.Render<'model, ReactElement> with
        member this.RenderChoice state options lens = [
                Html.div [
                    prop.className "choices"
                    prop.children [
                        let currentChoice = model |> read lens
                        for ix, o in options |> List.indexed do
                            Html.button [
                                prop.text (o.ToString());
                                prop.classes (if Some (ChoiceIndex ix) = currentChoice then [if state = Set then "incomplete"]@["chosen"; "choice"] else ["choice"])
                                prop.onClick (fun ev -> ev.preventDefault(); api.updateCmd(write lens (ChoiceIndex ix |> Some)))
                                ]
                            ]
                        ]
            ]
        member this.RenderChoiceDistinctN state options n lens = [
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
                                prop.classes (if currentChoices |> List.exists ((=) ix) then [if state = Set then "incomplete"]@["chosen"; "choice"] else ["choice"])
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

let tryEval (chargen_: Lens<_, State>) model setting =
    let getLens hashCode =
        let hash_ =
            Lens.create
                (Map.tryFind hashCode)
                (function None -> Map.remove hashCode | Some v -> Map.add hashCode v)
        chargen_ => wizardChoices_ => hash_
    let trivialRender =
        {
            new AutoWizard.Render<'model, unit> with
                member this.RenderChoice state options lens = []
                member this.RenderChoiceDistinctN state options n lens = []
        }
    match AutoWizard.eval(setting, getLens, trivialRender, model) with
    | Complete v, _ -> Some v
    | _ -> None

module Stats =
    open Chargen.Draft
    let rearrangeStats (api, unmodifiedStats: Stats, traits) =
        api.modalDialog.Launch((unmodifiedStats, None), fun ((unmodifiedStats: Stats, srcStat: Stat option), update: _ -> unit, finishWith) ->
            let current = currentStats traits unmodifiedStats
            Html.div [
                prop.className "stats"
                prop.children [
                    for stat in Stat.values do
                        let lens = Stat.lenses.[Stat.toTag stat]
                        let label = Stat.toString stat
                        Html.span[prop.className "statLabel"; prop.text (sprintf "%s: " label)]
                        Html.span[prop.className "statValue"; prop.text (current |> read lens)]
                        Html.span[prop.className "statUnmodifiedValue"; prop.text (unmodifiedStats |> read lens |> sprintf "(was %d)")]
                        match srcStat with
                        | Some srcStat when srcStat = stat -> ()
                        | Some srcStat ->
                            let swap stats src targ =
                                let srcLens, targLens = Stat.lenses.[Stat.toTag src], Stat.lenses.[Stat.toTag targ]
                                let srcVal, targVal = (unmodifiedStats |> read srcLens), (unmodifiedStats |> read targLens)
                                unmodifiedStats |> write srcLens targVal |> write targLens srcVal, None
                            Html.button[prop.className "placeStat"; prop.text (sprintf "Swap %s/%s" (Stat.toString srcStat) (Stat.toString stat)); prop.onClick(fun _ -> update(swap unmodifiedStats srcStat stat))]
                        | None ->
                            Html.button[prop.className "pickStat"; prop.text (sprintf "Select %s" label); prop.onClick(fun _ -> update (unmodifiedStats, Some stat))]
                    Html.button[prop.className "rearrangeStats"; prop.text (sprintf "OK"); prop.onClick(fun _ -> finishWith (over api.chargen_ (fun chargen -> chargen |> write (charSheet_P => DraftSheet.unmodifiedStats_) unmodifiedStats)))]
                    Html.button[prop.className "rearrangeStats"; prop.text (sprintf "Cancel"); prop.onClick(fun _ -> finishWith id)]
                    ]
                ])


    let view (api:API<_>, current: Stats, unmodified: Stats, traits, readOnly) =
        Html.div [
            prop.className "stats"
            prop.children [
                for stat in Stat.values do
                    let lens = Stat.lenses.[Stat.toTag stat]
                    let label = Stat.toString stat
                    Html.span[prop.className "statLabel"; prop.text (sprintf "%s: " label)]
                    Html.span[prop.className "statValue"; prop.text (current |> read lens)]
                    Html.span[prop.className "statUnmodifiedValue"; prop.text (unmodified |> read lens |> sprintf "(was %d)")]
                if not readOnly then
                    Html.button[prop.className "rearrangeStats"; prop.text (sprintf "Rearrange stats"); prop.onClick(fun _ -> rearrangeStats(api, unmodified, traits))]
                ]
            ]

let renameDialog (api: API<'model>, model: 'model, sheet: Draft.DraftSheet)=
    let tryEval = tryEval api.chargen_ model
    api.modalDialog.Launch(sheet.name, fun (name': string, updateName': string -> unit, finishWith) ->
        Html.form [
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
                    prop.onClick (fun ev -> ev.preventDefault(); finishWith id)
                    prop.text "Cancel"
                    ]
                ]
            prop.onSubmit(fun ev -> ev.preventDefault(); finishWith(writeSome (api.chargen_ => charSheet_P => DraftSheet.explicitName_) name'))
            ])


let finish (api: API<_>) (sheet: Draft.DraftSheet) model =
    let arbitrarilyFulfill =
        let getLens hashCode =
            // if there's any choices left to take, arbitrarily choose the first one(s)
            let hash_ =
                Lens.create
                    (fun map -> match map |> Map.tryFind hashCode with Some choice -> Some choice | None -> match hashCode with Choice  _ -> Some (ChoiceIndex 0) | Multichoice(_, n) -> Some(MultichoiceIndex [0..(n-1)]))
                    (function None -> Map.remove hashCode | Some v -> Map.add hashCode v)
            api.chargen_ => wizardChoices_ => hash_
        let render =
            { new Render<'model, unit> with
                  member this.RenderChoice state (options: 't list) (lens: Optics.Lens<'model,ChoiceState option>): unit list =
                      []
                  member this.RenderChoiceDistinctN state (options: 't1 list) (n: int) (lens: Optics.Lens<'model,ChoiceState option>): unit list =
                      []
                }
        fun (setting: Setting<_>) -> match eval(setting, getLens, render, model) with Complete v, _ -> v | _ -> shouldntHappen()
    let finishedCharacter : Character.CharacterSheet =
        let race = arbitrarilyFulfill sheet.race
        {
            stats =
                let statBonuses = Draft.statBonuses [Draft.Trait.Race race]
                Draft.currentStats statBonuses sheet.unmodifiedStats
            unmodifiedStats = sheet.unmodifiedStats
            race = race
            sex = sheet.sex |> arbitrarilyFulfill
            name = sheet.name
            xp = 0
            allocatedLevels = []
            subclasses = sheet.subclasses
            classAbilities = sheet.classAbilities |> List.map arbitrarilyFulfill
        }
    model
    |> over api.roster_ (fun roster -> finishedCharacter :: roster)
    |> write (api.chargen_ => viewMode_) None
    |> write api.chargen_ State.fresh

let viewAndEditCharacter (api:API<_>) (model: 'model) (sheet: Draft.DraftSheet) cancelButton =
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
            let classFeatures = Domain.Chargen.classFeatures sheet.allocatedLevels
            let classFeatureChoice, elements = classFeatures |> List.fold (fun (accum, elements) setting -> setting |> eval elements |> fun (v, elements) -> v::accum, elements) ([], elements)
            Html.div [prop.className "characterName"; prop.text sheet.name]
            Html.button [prop.text "Rename"; prop.onClick (fun _ -> renameDialog(api, model, sheet))]
            let statBonuses = Draft.statBonuses [match raceChoice with Complete race -> Draft.Trait.Race race | _ -> ()]
            Stats.view (api, Draft.currentStats statBonuses stats, stats, statBonuses, false)
            yield! elements
            // only only to proceed if all settings are set
            match sexChoice, raceChoice, classFeatureChoice |> List.every (function Complete _ -> true | _ -> false) with
            | Complete sex, Complete race, true ->
                let save model =
                    model
                Components.buttons [
                    Html.button [prop.text "OK"; prop.onClick(fun  _ -> api.updateCmd(finish api sheet))]
                    cancelButton
                    ]
            | _ -> cancelButton
        ]
    ]

let viewCharacter (api:API<_>) (model: 'model) (sheet: Character.CharacterSheet) =
    Html.div [
        prop.className "characterView editing"
        prop.children [
            let stats = sheet.unmodifiedStats
            let update t = api.updateCmd (over api.chargen_ t)
            let set (lens: Lens<_,_>) v = api.updateCmd (write (api.chargen_ => lens) (Some v))
            Html.div [
                prop.text (sprintf "%A %A" sheet.sex sheet.race)
                ]
            Html.div [prop.className "characterName"; prop.text sheet.name]
            let statBonuses = Draft.statBonuses [Draft.Trait.Race sheet.race]
            Stats.view (api, Draft.currentStats statBonuses stats, stats, statBonuses, true)
        ]
    ]


let view (api: API<_>) (model: 'model) =
    let state = model |> read api.chargen_
    let cancel =
        Html.button [
            prop.onClick (fun _ -> api.updateCmd(write (api.chargen_ => viewMode_) None >> write api.chargen_ State.fresh))
            prop.text (match state.viewMode with Some (Creating sheet) -> sprintf "Abandon %s" sheet.name | _ -> "Cancel")
            ]

    React.fragment [
        match state.viewMode with
        | Some (Creating sheet) ->
            let cancel =
                Html.button [
                    prop.onClick (fun _ -> api.updateCmd(finish api sheet))
                    prop.text (match state.viewMode with Some (Creating sheet) -> sprintf "Abandon %s" sheet.name | _ -> "Cancel")
                    ]
            viewAndEditCharacter api model sheet cancel
        | Some Selecting ->
            Html.div [
                prop.className "simplePick"
                let roster = model |> read api.roster_
                let selectFor ix (sheet: Character.CharacterSheet) =
                    Html.button [
                        prop.text sheet.name
                        prop.onClick (fun _ -> api.updateCmd(writeSome (api.chargen_ => viewMode_) (Viewing roster.[ix])))
                        ]
                prop.children [
                    yield! roster |> List.mapi selectFor
                    cancel
                    ]
                ]
        | Some (Viewing sheet) ->
            viewCharacter api model sheet
            cancel
        | None ->
            let tryEval = tryEval api.chargen_ model
            Html.div [
                prop.className "simplePick"
                prop.children [
                    Html.button [
                        prop.onClick (fun _ -> api.updateCmd(writeSome (api.chargen_ => viewMode_) (roll4d6k3() |> Draft.createBlank tryEval |> Creating)))
                        prop.text "Create new character"
                        ]
                    Html.button [
                        prop.onClick (fun _ -> api.updateCmd(writeSome (api.chargen_ => viewMode_) Selecting))
                        prop.text "Resume with existing"
                        ]
                ]
            ]
        ]