[<RequireQualifiedAccess>]
module StardewValleyStonks.WebApp.View.Select

open StardewValleyStonks
open StardewValleyStonks.WebApp

open Fable.Core
open Fable.Core.JsInterop
open Feliz

open type Html
open type prop
open type React

open Core.Operators

type private 'a Props = {|
  Width: Styles.ICssUnit
  ToString: ('a -> string) option
  Display: 'a -> ReactElement
  Options: 'a array
  Selected: 'a
  Dispatch: 'a -> unit
|}

type private 'a State = {|
  Focused: bool
  Hover: int option
  Scroll: bool
  Search: string
  Options: 'a array
|}

let private tryScroll (list: Browser.Types.HTMLElement) (index: int) (mode: string) =
  let elm = list.children?item index
  if not (isNullOrUndefined elm) then
    elm?scrollIntoView {| block = mode |}

let selectControl initialState inputRef (props: _ Props) (state: _ State) setState =
  div [
    className "select-control"
    children [
      input [
        onFocus (fun _ -> setState {| state with Focused = true |})
        onBlur (fun _ -> setState initialState)
        prop.ref<Browser.Types.HTMLInputElement> inputRef

        match props.ToString with
        | Some toString ->
          className "select-input-search"
          prop.type'.text
          placeholder " "
          value state.Search
          onChange (fun (str: string) ->
            let lower = str.ToLower ()
            let options = props.Options |> Array.filter (fun opt -> (toString opt).ToLower().Contains lower)
            setState {|
              state with
                Search = str
                Hover = state.Hover |> Option.defaultOrMap 0 (min (options.Length - 1) >> max 0) |> Some
                Options = options
            |})
        | None ->
          className "select-input-hidden"
          prop.inputMode.none
      ]

      Html.span [
        className "select-value"
        children (props.Display props.Selected)
      ]
    ]
  ]

let selectList listRef clearHover (props: _ Props) (state: _ State) setState hover =
  div [
    className "select-list"
    tabIndex -1
    onMouseDown handleEvent
    children [
      if Array.isEmpty state.Options then
        div "No results found..."
      else
        ul [
          prop.ref<Browser.Types.HTMLElement> listRef
          children (state.Options |> Array.mapi (fun i opt ->
            li [
              onMouseDown (fun e ->
                if e.button = 0 then
                  props.Dispatch opt
                  clearHover e)

              onMouseMove (fun _ ->
                if state.Hover <> Some i then
                  setState {|
                    state with
                      Focused = true
                      Hover = Some i
                      Scroll = false
                  |})

              if hover = i then className "hover"
              children (props.Display opt)
            ]
          ))
        ]
      ]
    ]

let [<ReactComponent>] private Select (props: _ Props) =
  let selectedIndex =
    props.Options
    |> Array.tryFindIndex ((=) props.Selected)
    |> Option.defaultValue 0

  let selectOffset e offset =
    handleEvent e
    let index = selectedIndex + offset |> min (props.Options.Length - 1) |> max 0
    if index <> selectedIndex then
      props.Dispatch props.Options[index]

  let initialState = {|
    Focused = false
    Hover = None
    Scroll = false
    Search = ""
    Options = props.Options
  |}

  let (prev, state), setState = useState ((initialState, initialState))
  let setState next = setState (state, next)
  let inputRef = useInputRef ()
  let listRef = useElementRef ()

  useEffect ((fun () ->
    setState {| initialState with Options = props.Options |}
  ), [| box props.Options |])

  useLayoutEffect (fun () ->
    match prev.Hover, state.Hover, inputRef.current with
    | None, Some _, Some input -> input.focus ()
    | _ -> ()

    match prev.Hover, state.Hover, listRef.current with
    | None, Some _, Some list -> tryScroll list selectedIndex "center"
    | Some _, Some i, Some list when state.Scroll -> tryScroll list i "nearest"
    | _ -> ())

  let setHover e i =
    handleEvent e
    if state.Hover <> Some i then
      setState {|
        state with
          Focused = true
          Hover = Some i
          Scroll = true
      |}

  let clearHover e =
    handleEvent e
    setState {| initialState with Focused = true |}

  div [
    className "select-container"

    onMouseDown (fun e ->
      if e.button = 0 then
        match state.Hover with
        | Some _ -> clearHover e
        | None -> setHover e selectedIndex)

    onKeyDown (fun e ->
      match e.key, state.Hover with
      | "Escape", Some _ -> clearHover e

      | "Enter", Some hover
      | " ", Some hover when props.ToString.IsNone || e.key = "Enter" ->
        state.Options |> Array.tryItem hover |> Option.iter props.Dispatch
        clearHover e

      | "Enter", None
      | " ", None when props.ToString.IsNone || e.key = "Enter" ->
        setHover e selectedIndex

      | "ArrowRight", None
      | "ArrowDown", None -> selectOffset e 1
      | "ArrowDown", Some hover -> setHover e (if hover = state.Options.Length - 1 then 0 else hover + 1)

      | "ArrowLeft", None
      | "ArrowUp", None -> selectOffset e -1
      | "ArrowUp", Some hover -> setHover e (if hover = 0 then state.Options.Length - 1 else hover - 1)

      | "PageDown", None -> selectOffset e 5
      | "PageDown", Some hover -> setHover e (hover + 5 |> min (state.Options.Length - 1))

      | "PageUp", None -> selectOffset e -5
      | "PageUp", Some hover -> setHover e (hover - 5 |> max 0)

      | "Home", None -> selectOffset e -props.Options.Length
      | "Home", Some _ -> setHover e 0

      | "End", None -> selectOffset e props.Options.Length
      | "End", Some _ -> setHover e (state.Options.Length - 1)

      | _ -> ())

    children [
      div [
        className "select"
        style [ style.width props.Width ]
        children [
          selectControl initialState inputRef props state setState
          state.Hover |> Option.defaultOrMap none (selectList listRef clearHover props state setState)
        ]
      ]
    ]
  ]

let search width toString display options selected dispatch =
  Select {|
    Width = width
    ToString = Some toString
    Display = display
    Options = options
    Selected = selected
    Dispatch = dispatch
  |}

let options width display options selected dispatch =
  Select {|
    Width = width
    ToString = None
    Display = display
    Options = options
    Selected = selected
    Dispatch = dispatch
  |}

let inline unitUnion width (selected: 'a) dispatch =
  options width (Reflection.getCaseName >> ofStr) unitUnionCases<'a> selected dispatch
