module StardewValleyStonks.WebApp.Main

open Fable.Core.JsInterop

#if DEBUG
importDefault "preact/debug"
#endif

open Feliz
open Elmish
open Elmish.React
#if FABLE_COMPILER
open Thoth.Json
#else
open Thoth.Json.Net
#endif

open type Html

open StardewValleyStonks.WebApp
open type StardewValleyStonks.WebApp.Update.AppMessage
open StardewValleyStonks.WebApp.View

let init () =
  let app = Data.LocalStorage.loadApp ()
  Data.LocalStorage.updateVersion ()
  app, []

let saveState = debouncer 100 Data.LocalStorage.saveState
let saveSettings = debouncer 100 Data.LocalStorage.saveSettings

let update msg app =
  match msg with
  | SetState msg ->
    let state = app.State |> Update.state msg app.Data
    saveState state
    { app with State = state }, []
  | SetSavedSettings msg ->
    let saved = app.SavedSettings |> Update.savedSettings msg app.State.Settings
    saveSettings saved
    { app with SavedSettings = saved }, []
  | SyncSavedSettings saved -> { app with SavedSettings = saved }, []
  | HardReset ->
    Data.LocalStorage.clear ()
    Browser.Dom.window.location.reload ()
    app, []

let view app dispatch =
  try view app dispatch
  with e ->
    console.error e
    div [
      ofStr "Whoops, something went wrong. Please reload the page..."
      br []
      ofStr (sprintf "%A" e)

      // ofStr "If this message remains after reloading the page, try doing a "
      // hardResetButton
    ]

let localStorageSub dispatch =
  Data.LocalStorage.subscribe (SyncSavedSettings >> dispatch)
  { new System.IDisposable with member _.Dispose () = () }

do
  let pxToFloat (px: string) = float (px.Substring(0, px.Length - 2))
  let borderWidth fontPx divisor = $"{fontPx / float divisor |> ceil}px"

  let root = Browser.Dom.document.documentElement
  let fontSize: string = Browser.Dom.window?getComputedStyle(root)?getPropertyValue("font-size")
  let fontPx = pxToFloat fontSize
  root?style?setProperty("--font-size", fontSize)
  root?style?setProperty("--eighth-border", borderWidth fontPx 8)
  root?style?setProperty("--sixth-border", borderWidth fontPx 6)

Program.mkProgram init update view
|> Program.withErrorHandler (fun (msg, e) -> console.error (msg, e))
|> Program.withSubscription (fun _ -> [ [ "localStorage" ], localStorageSub ])
|> Program.withReactBatched "app"
|> Program.run
