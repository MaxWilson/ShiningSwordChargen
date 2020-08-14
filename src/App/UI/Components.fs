module UI.Components
open Feliz

type Launcher<'model, 'output> =
    // given an initial state and a dialog factory, creates dialogs that last until terminated.
    abstract member Launch: (*initial*) 'state * ((*current*) 'state * (*update*)('state -> unit) * (*doneSignal*) (('model -> 'model) -> unit) -> 'output) -> unit

let buttons (buttons: ReactElement list) =
    Html.div [
        prop.className "buttons"
        prop.children buttons
        ]