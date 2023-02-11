module StardewValleyStonks.WebApp.Main

open Fable.Core.JsInterop

#if DEBUG
importDefault "preact/debug"
#endif

open Feliz
open Elmish
open Elmish.React

open type Html

open StardewValleyStonks.WebApp
open type StardewValleyStonks.WebApp.Update.AppMessage
open StardewValleyStonks.WebApp.View

let saveState = debouncer 100 Data.LocalStorage.saveState
let savePresets = debouncer 100 Data.LocalStorage.savePresets

let update msg app =
  match msg with
  | SetState msg ->
    let state = app.State |> Update.state msg app.Data
    saveState state
    { app with State = state }
  | SetPresets msg ->
    let presets = app.Presets |> Update.presets msg
    savePresets presets
    { app with Presets = presets }
  | SyncPresets presets -> { app with Presets = presets }
  | NuclearReset ->
    Data.LocalStorage.clear ()
    Browser.Dom.window.location.reload ()
    app

let view app dispatch =
  try
    React.fragment [
      Visualization.section app (SetState >> dispatch)
      Settings.section app dispatch
    ]
  with e ->
    console.error e
    div [
      ofStr "Whoops, something went wrong. Please try reloading the page. If this message remains after reloading the page, try doing a "
      Settings.LoadSave.nuclearReset dispatch
      br []
      details [
        summary "Error Message"
        ofStr (sprintf "%A" e)
      ]
    ]

let localStorageSub dispatch =
  Data.LocalStorage.subscribe (SyncPresets >> dispatch)
  { new System.IDisposable with member _.Dispose () = () }

do
  let pxToFloat (px: string) = float (px.Substring(0, px.Length - 2))
  let borderWidth fontPx divisor = $"{fontPx / float divisor |> ceil}px"

  let root = Browser.Dom.document.documentElement
  let fontSize = Browser.Dom.window?getComputedStyle(root)?getPropertyValue("font-size")
  let fontPx = pxToFloat fontSize
  root?style?setProperty("--font-size", fontSize)
  root?style?setProperty("--eighth-border", borderWidth fontPx 8)
  root?style?setProperty("--sixth-border", borderWidth fontPx 6)

Program.mkSimple Data.LocalStorage.loadApp update view
|> Program.withErrorHandler (fun (msg, e) -> console.error (errorWithMessage msg e))
|> Program.withSubscription (fun _ -> [ [ "localStorage" ], localStorageSub ])
|> Program.withReactBatched "app"
|> Program.run
