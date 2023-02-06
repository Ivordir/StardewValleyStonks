[<RequireQualifiedAccess>]
module StardewValleyStonks.WebApp.View.Dialog

open Feliz

open type Html
open type prop
open type React

open Core.Operators

let [<ReactComponent>] private Dialog (props: {|
    Cancel: bool
    Title: string
    Close: bool -> unit
    Children: ReactElement
  |}) =
  let ref = useRef<Browser.Types.HTMLDialogElement option> None

  useLayoutEffect (fun () ->
    match ref.current with
    | Some m when not m.``open`` -> m.showModal ()
    | _ -> ())

  dialog [
    Interop.mkAttr "onClose" (fun _ -> props.Close false)
    prop.ref ref
    prop.children [
      h1 props.Title
      props.Children
      div [
        if props.Cancel then
          button [
            onClick (fun _ -> props.Close false)
            text "Cancel"
          ]
        button [
          onClick (fun _ -> props.Close true)
          text "Ok"
        ]
      ]
    ]
  ]

let [<ReactComponent>] private EditDialogToggle (props: {|
    Title: string
    ToggleText: string
    State: 'a
    Dispatch: 'a -> unit
    Children: 'a -> ('a -> unit) -> ReactElement
  |}) =
  let state, setState = useState None
  fragment [
    button [
      onClick (fun _ -> setState (Some props.State))
      text props.ToggleText
    ]

    match state with
    | None -> none
    | Some state ->
      Dialog {|
        Cancel = true
        Title = props.Title
        Close = fun ok ->
          if ok then props.Dispatch state
          setState None
        Children = props.Children state (Some >> setState)
      |}
  ]

let create title close children = Dialog {|
  Cancel = false
  Title = title
  Close = fun _ -> close ()
  Children = children
|}

let toggleEdit title toggleText state dispatch children = EditDialogToggle {|
  Title = title
  ToggleText = toggleText
  State = state
  Dispatch = dispatch
  Children = children
|}

let confirmAction title actionText dispatch children =
  toggleEdit title actionText () dispatch (fun () _ -> children)
