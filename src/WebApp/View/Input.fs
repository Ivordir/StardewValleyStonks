[<RequireQualifiedAccess>]
module StardewValleyStonks.WebApp.View.Input

open StardewValleyStonks
open StardewValleyStonks.WebApp

open Feliz

open type Html
open type prop
open type React

open Core.Operators

let floatRangeWith
  (label: IReactProperty option)
  (precision: float)
  (min: float)
  (max: float)
  (value: float)
  (dispatch: float -> unit)
  =
  input [
    if Option.isSome label then label.Value
    prop.type'.range
    prop.min min
    prop.max max
    step precision
    prop.value value
    onChange dispatch
  ]

let floatRange = floatRangeWith None

let natRangeWith label (min: nat) (max: nat) (value: nat) dispatch =
  floatRangeWith label 1.0 (float min) (float max) (float value) (nat >> dispatch)

let natRange = natRangeWith None


let [<ReactComponent>] private NumberInput (props: {|
    Label: IReactProperty option
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
    if props.Label.IsSome then props.Label.Value
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

let natWith label width min max (value: nat) dispatch =
  NumberInput {|
    Label = label
    Width = width
    Min = min |> Option.defaultValue System.UInt32.MinValue |> float |> Some
    Max = max |> Option.defaultValue System.UInt32.MaxValue |> float |> Some
    Precision = 1.0
    Value = float value
    Dispatch = nat >> dispatch
  |}

let inline nat width value dispatch =
  natWith None width None None value dispatch

let floatWith label width precision min max value dispatch =
  NumberInput {|
    Label = label
    Width = width
    Min = min
    Max = max
    Precision = precision
    Value = value
    Dispatch = dispatch
  |}

let float width = floatWith None width


let textWith (label: IReactProperty option) (value: string) (dispatch: string -> unit) =
  input [
    if Option.isSome label then label.Value
    className Class.inputBox
    prop.type'.text
    prop.value value
    onChange dispatch
  ]

let text = textWith None


let labeled label element =
  Html.label [
    ofStr label
    element
  ]


let checkboxWith children value dispatch =
  label [
    className [ Class.checkbox; Class.label ]
    onClick (fun e -> e.stopPropagation ())
    prop.children [
      input [
        prop.type'.checkbox
        isChecked value
        onCheckedChange dispatch
      ]

      img [ alt "" ]

      children
    ]
  ]

let inline checkboxText str value msg = checkboxWith (ofStr str) value msg

let checkbox labelText value dispatch =
  label [
    className Class.checkbox
    onClick (fun e -> e.stopPropagation ())
    children [
      input [
        prop.type'.checkbox
        isChecked value
        onCheckedChange dispatch
        ariaLabel labelText
      ]

      img [ alt "" ]
    ]
  ]
