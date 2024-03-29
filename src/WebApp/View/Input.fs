[<RequireQualifiedAccess>]
module StardewValleyStonks.WebApp.View.Input

open StardewValleyStonks
open StardewValleyStonks.WebApp

open Feliz

open type Html
open type prop
open type React

open Core.Operators

let floatRange
  (precision: float)
  (min: float)
  (max: float)
  (value: float)
  (dispatch: float -> unit)
  =
  input [
    prop.type'.range
    prop.min min
    prop.max max
    step precision
    prop.value value
    onChange (debouncer 5 dispatch)
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
    className Class.inputBox
    style [ style.width props.Width ]
    prop.type'.number
    if props.Min.IsSome then prop.min props.Min.Value
    if props.Max.IsSome then prop.max props.Max.Value
    step props.Precision
    prop.value value

    onChange (fun (v: float) ->
      let p = 1.0 / props.Precision
      let v = System.Math.Round (v * p) / p
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

let natWith width min max (value: nat) dispatch =
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

let float width precision min max value dispatch =
  NumberInput {|
    Width = width
    Min = min
    Max = max
    Precision = precision
    Value = value
    Dispatch = dispatch
  |}

let private numberWithRange label number range =
  Html.span [ className Class.numberAndRange; children [
    labeled label number
    labeledHidden label range
  ]]

let floatWithRange label width precision min max value dispatch =
  numberWithRange
    label
    (float width precision (Some min) (Some max) value dispatch)
    (floatRange precision min max value dispatch)

let natWithRange label width min max value dispatch =
  numberWithRange
    label
    (natWith width (Some min) (Some max) value dispatch)
    (natRange min max value dispatch)


let text (value: string) (dispatch: string -> unit) =
  input [
    className Class.inputBox
    prop.type'.text
    prop.value value
    onChange dispatch
  ]


let checkboxWith children value dispatch =
  Html.label [ className Class.checkbox; prop.children [
    input [
      prop.type'.checkbox
      isChecked value
      onCheckedChange dispatch
    ]

    img [ alt "" ]

    children
  ]]

let checkbox (text: string) value dispatch = checkboxWith (Html.span text) value dispatch

let checkboxHiddenText (labelText: string) value dispatch =
  Html.label [ className Class.checkbox; prop.children [
    Html.span [
      className Class.hiddenText
      prop.text labelText
    ]

    input [
      prop.type'.checkbox
      isChecked value
      onCheckedChange dispatch
    ]

    img [ alt "" ]
  ]]
