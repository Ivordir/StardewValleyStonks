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
  let root = Browser.Dom.document.documentElement
  let fontSize: string = Browser.Dom.window?getComputedStyle(root)?getPropertyValue("font-size")
  let setVar (name: string) (px: int) = root?style?setProperty($"--{name}", $"{px}px")

  let fontPx = (fontSize.Substring(0, fontSize.Length - 2)) |> float |> ceil

  setVar "font-size" (int fontPx)
  setVar "eighth-border" ((fontPx / 8.0) |> ceil |> int)
  setVar "sixth-border" ((fontPx / 6.0) |> ceil |> int)

  let sp = fontPx / 16.0
  setVar "min-size" ((sp * 48.0) |> ceil |> int)

  // set icon size to a multiple of 8 pixels,
  // preferring to upsize instead of downsizing
  let iconSize =
    let div = fontPx / 8.0
    let rem = fontPx % 8.0
    let unit =
      if rem <= 2.0
      then floor div
      else ceil div
    int unit * 8

  setVar "icon-size" iconSize

Program.mkSimple Data.LocalStorage.loadApp update view
|> Program.withErrorHandler (fun (msg, e) -> console.error (errorWithMessage msg e))
|> Program.withSubscription (fun _ -> [ [ "localStorage" ], localStorageSub ])
|> Program.withReactBatched "app"
|> Program.run
