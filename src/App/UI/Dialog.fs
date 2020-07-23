module UI.Dialog
open Fable.React


type Launcher<'model, 'output> =
    // given an initial state and a dialog factory, creates dialogs that last until terminated.
    abstract member Launch: (*initial*) 'state * ((*current*) 'state * (*update*)('state -> unit) * (*doneSignal*) (('model -> 'model) -> unit) -> 'output) -> unit
