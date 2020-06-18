module UI.Chargen
open Optics
open Optics.Operations
open Domain
open Domain.Model
open Feliz

type 'model API = {
    stats: Lens<'model, Stats option>
    //name: Lens<'model, string>
    updateCmd: ('model -> 'model) -> unit
    //doneCmd: 'dispatch -> unit
    }

let view (api: API<_>) model =
    React.fragment [
        Html.h1 "New character"
        Html.button [
            prop.onClick (fun e -> api.updateCmd (write api.stats (Some (Domain.Chargen.roll3d6InOrder()))))
            prop.text "Roll 3d6"
            ]
        Html.button [
            prop.onClick (fun e -> api.updateCmd (write api.stats (Some (Domain.Chargen.roll4d6k3()))))
            prop.text "Roll 4d6k3"
            ]
        match (model |> read api.stats) with
        | None -> ()
        | Some stats ->
            let attr name v =
                Html.div (sprintf "%s: %d" name v)
            for st in Stat.values do
                    attr (Stat.toString st) (stats |> read (Stat.lens st))
        ]