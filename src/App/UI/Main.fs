module UI.Main

open Elmish
open Elmish.React
open Feliz
open Fable.SimpleHttp
open App
open Elmish.HMR

Program.mkProgram init update view
|> Program.withReactSynchronous "elmish-app"
|> Program.run