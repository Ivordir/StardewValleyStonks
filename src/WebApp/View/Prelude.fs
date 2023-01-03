module [<AutoOpen>] StardewValleyStonks.WebApp.View.Prelude

open StardewValleyStonks

open Fable.Core.JsInterop
open Feliz

type prop with
  static member inline onChange (handler: nat -> unit) =
    Interop.mkAttr "onChange" (fun (e: Browser.Types.Event) ->
      let value: float = !!e.target?valueAsNumber
      if not <| isNullOrUndefined value
        && not <| System.Double.IsNaN value
        && value >= 0.0
      then
        round value |> unbox<nat> |> handler)

  static member inline onToggle (handler: bool -> unit) =
    Interop.mkAttr "onToggle" (fun (e: Browser.Types.Event) -> handler e.target?``open``)

  static member inline valueOrDefault (n: nat) =
    prop.ref (fun e -> if e |> isNull |> not && !!e?value <> !!n then e?value <- !!n)

  static member inline value (n: nat) = Interop.mkAttr "value" n
  static member inline min (n: nat) = Interop.mkAttr "min" n
  static member inline max (n: nat) = Interop.mkAttr "max" n


type Html with
  static member inline text (n: nat) = string n |> Html.text


let inline ofStr (str: string) = Html.text str
let inline ofNat (n: nat) = Html.text n
let inline ofInt (i: int) = Html.text i
let inline ofFloat (x: float) = Html.text x


let debouncer timeout (f : _ -> unit) =
  let mutable last = None
  fun value ->
    last |> Option.iter Browser.Dom.window.clearInterval
    last <- Some <| Browser.Dom.window.setTimeout ((fun () -> f value), timeout)

// let debounce timeout =
//   let mutable last = None
//   fun (action: unit -> unit) ->
//     last |> Option.iter Browser.Dom.window.clearInterval
//     let delayed _ = action ()
//     last <- Some (Browser.Dom.window.setTimeout (delayed, timeout))

let internal handle (event: Browser.Types.Event) =
  event.stopPropagation ()
  event.preventDefault ()
