[<RequireQualifiedAccess>]
module StardewValleyStonks.WebApp.View.Dialog

open StardewValleyStonks.WebApp
open StardewValleyStonks.WebApp.View

open Feliz

open type Html
open type prop
open type React

open Core.Operators

// assume there can only be one open dialog at any time
let [<Literal>] dialogTitleId = "dialog-title"

let [<ReactComponent>] private Dialog (props: {|
    Cancel: bool
    Title: ReactElement
    Close: bool -> unit
    Children: (unit -> unit) -> ReactElement
  |}) =
  let ref = useRef<Browser.Types.HTMLDialogElement option> None

  useLayoutEffect (fun () ->
    match ref.current with
    | Some m when not m.``open`` -> m.showModal ()
    | _ -> ())

  dialog [
    prop.ref ref
    ariaLabelledBy dialogTitleId
    Interop.mkAttr "onClose" (fun _ -> props.Close false)
    children [
      h1 [
        prop.id dialogTitleId
        children props.Title
      ]

      props.Children (fun () -> ref.current |> Option.iter (fun m -> m.close ()))

      div [
        if props.Cancel then
          button [
            className Class.cancel
            onClick (fun _ -> props.Close false)
            ariaLabel "Cancel"
          ]

        button [
          className Class.ok
          onClick (fun _ -> props.Close true)
          ariaLabel "Ok"
        ]
      ]
    ]
  ]

let [<ReactComponent>] private EditDialogToggle (props: {|
    Title: ReactElement
    Toggle: (unit -> unit) -> ReactElement
    State: 'a
    Dispatch: 'a -> unit
    Children: (unit -> unit) -> 'a -> ('a -> unit) -> ReactElement
  |}) =
  let state, setState = useState None

  fragment [
    props.Toggle (fun _ -> setState (Some props.State))

    state |> ofOption (fun state ->
      Dialog {|
        Cancel = true
        Title = props.Title
        Close = fun ok ->
          if ok then props.Dispatch state
          setState None
        Children = fun close -> props.Children close state (Some >> setState)
      |})
  ]

let private toggleButton (css: string) (label: IReactProperty) toggle =
  button [
    className css
    Interop.mkAttr "aria-has-popup" "dialog"
    onClick (ignore >> toggle)
    label
  ]

let toggleEditWith (buttonText: string) title state dispatch children =
  EditDialogToggle {|
    Title = title
    Toggle = toggleButton Class.button (text buttonText)
    State = state
    Dispatch = dispatch
    Children = children
  |}

let toggleEdit title state dispatch children =
  EditDialogToggle {|
    Title = title
    Toggle = toggleButton Class.edit (ariaLabel "Edit")
    State = state
    Dispatch = dispatch
    Children = children
  |}

let confirmAction buttonText title dispatch children =
  toggleEditWith buttonText title () dispatch (fun _ () _ -> children)
