module [<RequireQualifiedAccess>] StardewValleyStonks.WebApp.View.Dialog

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

let [<ReactComponent>] private EditDialog (props: {|
    Title: string
    State: 'a
    OnClose: 'a option -> unit
    Children: 'a -> ('a -> unit) -> ReactElement
  |}) =
  let state, setState = useState props.State
  Dialog {|
    Cancel = true
    Title = props.Title
    Close = fun ok -> props.OnClose (if ok then Some state else None)
    Children = props.Children state setState
  |}

let [<ReactComponent>] private EditDialogToggle (props: {|
    Title: string
    ToggleText: string
    State: 'a
    Dispatch: 'a -> unit
    Children: 'a -> ('a -> unit) -> ReactElement
  |}) =
    let modal, setModal = useState false
    fragment [
      button [
        onClick (fun _ -> setModal true)
        text props.ToggleText
      ]
      if modal then EditDialog {|
        Title = props.Title
        State = props.State
        OnClose = (fun state ->
          state |> Option.iter props.Dispatch
          setModal false)
        Children = props.Children
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
