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
  list.children?item index |> Option.iter (fun elm -> elm?scrollIntoView {| block = mode |})

let selectControl initialState inputRef (props: _ Props) (state: _ State) setState =
  div [
    className Class.selectControl
    children [
      input [
        onFocus (fun _ -> setState {| state with Focused = true |})
        onBlur (fun _ -> setState initialState)
        prop.ref<Browser.Types.HTMLInputElement> inputRef

        match props.ToString with
        | Some toString ->
          prop.type'.search
          placeholder " "
          value state.Search
          onChange (fun str ->
            let lower = lowerCase str
            let options = props.Options |> Array.filter (fun opt -> (opt |> toString |> lowerCase).Contains lower)
            setState {|
              state with
                Search = str
                Hover = state.Hover |> Option.defaultOrMap 0 (min (options.Length - 1) >> max 0) |> Some
                Options = options
            |})
        | None ->
          prop.inputMode.none
      ]

      Html.span [
        style [ style.width props.Width ]
        children (props.Display props.Selected)
      ]
    ]
  ]

let selectList listRef clearHover (props: _ Props) (state: _ State) setState hover =
  div [
    className Class.selectList
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

              if hover = i then className Class.hover

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
    let index = (selectedIndex + offset) |> min (props.Options.Length - 1) |> max 0
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
    className Class.select

    onMouseDown (fun e ->
      if e.button = 0 then
        if state.Hover.IsSome
        then clearHover e
        else setHover e selectedIndex)

    onKeyDown (fun e ->
      match e.key, state.Hover with
      | "Escape", Some _ -> clearHover e

      | "Enter", Some hover
      | " ", Some hover when props.ToString.IsNone || e.key = "Enter" ->
        state.Options |> Array.tryItem hover |> Option.iter props.Dispatch
        clearHover e

      | "Enter", None
      | " ", None when props.ToString.IsNone || e.key = "Enter" -> setHover e selectedIndex

      | "ArrowRight", None
      | "ArrowDown", None -> selectOffset e 1
      | "ArrowDown", Some hover -> setHover e (if hover = state.Options.Length - 1 then 0 else hover + 1)

      | "ArrowLeft", None
      | "ArrowUp", None -> selectOffset e -1
      | "ArrowUp", Some hover -> setHover e (if hover = 0 then state.Options.Length - 1 else hover - 1)

      | "PageDown", None -> selectOffset e 5
      | "PageDown", Some hover -> setHover e (min (state.Options.Length - 1) (hover + 5))

      | "PageUp", None -> selectOffset e -5
      | "PageUp", Some hover -> setHover e (max 0 (hover - 5))

      | "Home", None -> selectOffset e -props.Options.Length
      | "Home", Some _ -> setHover e 0

      | "End", None -> selectOffset e props.Options.Length
      | "End", Some _ -> setHover e (state.Options.Length - 1)

      | _ -> ())

    children [
      div [
        selectControl initialState inputRef props state setState
        state.Hover |> ofOption (selectList listRef clearHover props state setState)
      ]
      img [ alt "" ]
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

let inline unitUnion width selected dispatch =
  options width (Reflection.getCaseName >> ofStr) unitUnionCases selected dispatch

let inline enum width (selected: 'a) dispatch =
  options width (Enum.name >> ofStr) Enum.values selected dispatch
