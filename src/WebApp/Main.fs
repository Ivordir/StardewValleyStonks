module StardewValleyStonks.WebApp.Main

open Fable.Core.JsInterop

#if DEBUG
importDefault "preact/debug"
#endif

open Feliz
open Elmish
open Elmish.React

open type Html
open type prop
open type React

open Core.Operators
open Core.ExtraTopLevelOperators

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

let viewError (e: exn) dispatch =
  div [ prop.id "app-error"; children [
    Html.span [
      Icon.errorWith (Html.span [
        ofStr """
          Whoops, something went wrong. Please try reloading the page.
          If this message remains after reloading the page, try doing a
        """
        Settings.LoadSave.nuclearReset dispatch
      ])
    ]

    details [
      summary "Error Message"
      div (sprintf "%A" e)
    ]
  ]]

[<ReactComponent(import="ErrorBoundary", from="react-error-boundary")>]
let ErrorBoundary (fallbackRender: {| error: exn |} -> ReactElement, children: ReactElement seq) = imported ()

let view app dispatch =
  try
    ErrorBoundary ((fun e -> viewError e.error dispatch), [
      Visualization.section app (SetState >> dispatch)
      Settings.section app dispatch
    ])
  with e ->
    console.log e
    viewError e dispatch

let localStorageSub dispatch =
  Data.LocalStorage.subscribe (SyncPresets >> dispatch)
  { new System.IDisposable with member _.Dispose () = () }


Program.mkSimple Data.LocalStorage.loadApp update view
|> Program.withErrorHandler (fun (msg, e) -> console.error (errorWithMessage msg e))
|> Program.withSubscription (fun _ -> [ [ "localStorage" ], localStorageSub ])
|> Program.withReactSynchronous "app"
|> Program.run
