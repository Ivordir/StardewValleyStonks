[<RequireQualifiedAccess>]
module StardewValleyStonks.WebApp.View.Input

open StardewValleyStonks
open StardewValleyStonks.WebApp

open Feliz

open type Html
open type prop
open type React

open Core.Operators

let private class' = className "input-box"

let floatRange (precision: float) (min: float) (max: float) (value: float) (dispatch: float -> unit) =
  input [
    prop.type'.range
    prop.min min
    prop.max max
    step precision
    valueOrDefault value
    onChange dispatch
  ]

let natRange (min: nat) (max: nat) (value: nat) dispatch =
  floatRange 1.0 (float min) (float max) (float value) (nat >> dispatch)


let [<ReactComponent>] private NumberInput (props: {|
    Width: Styles.ICssUnit
    Min: float option
    Max: float option
    Precision: float
    Value: float
    Dispatch: float -> unit
  |}) =
  let (value, update), setState = useState ((props.Value, true))
  let setValue value = setState (value, true)
  let updateInput () = setState (value, not update)

  useEffect ((fun () ->
    if props.Value <> value then
      setValue props.Value
  ), [| box props.Value |])

  input [
    class'
    prop.type'.number
    if props.Min.IsSome then prop.min props.Min.Value
    if props.Max.IsSome then prop.max props.Max.Value
    step (1.0 / props.Precision)
    style [ style.width props.Width ]
    valueOrDefault value
    onChange (fun (v: float) ->
      let v = System.Math.Truncate (v / props.Precision) * props.Precision
      if v <> value
      && props.Min |> Option.forall (fun m -> m <= v)
      && props.Max |> Option.forall (fun m -> v <= m) then
        props.Dispatch v)
    onBlur (fun _ -> updateInput ())
    onKeyDown (fun e ->
      match e.key with
      | "Enter" | "Escape" ->
        handleEvent e
        updateInput ()
      | _ -> ())
  ]

let natWith width (min: nat option) (max: nat option) (value: nat) dispatch =
  NumberInput {|
    Width = width
    Min = min |> Option.defaultValue System.UInt32.MinValue |> float |> Some
    Max = max |> Option.defaultValue System.UInt32.MaxValue |> float |> Some
    Precision = 1.0
    Value = float value
    Dispatch = nat >> dispatch
  |}

let inline nat width value dispatch =
  natWith width None None value dispatch

let floatWith width precision min max (value: float) dispatch =
  NumberInput {|
    Width = width
    Min = min
    Max = max
    Precision = precision
    Value = value
    Dispatch = dispatch
  |}


let text (value: string) (dispatch: string -> unit) =
  input [
    class'
    prop.type'.text
    prop.value value
    onChange dispatch
  ]

