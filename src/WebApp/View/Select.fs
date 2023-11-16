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

// https://www.w3.org/TR/wai-aria-1.1/#combobox

type private 'a Props = {|
  Width: Styles.ICssUnit
  DisplayString: ('a -> string) option
  Display: 'a -> ReactElement
  Options: 'a array
  Selected: 'a
  Dispatch: 'a -> unit
|}

type private 'a State = {|
  Hover: int option
  Scroll: bool
  Search: string
  Options: 'a array
|}

let [<Literal>] private pageSize = 10
let [<Literal>] private listId = "select-list"
let [<Literal>] private optionId = "selected-option"

// The app should only have at most one select list and one selected option present at any given time,
// so the ids for these elements are not generated based off the given props.

let selectControl initialState inputRef (props: _ Props) (state: _ State) setState =
  div [
    className Class.selectControl
    children [
      input [
        prop.ref<Browser.Types.HTMLInputElement> inputRef
        ariaMultiLine false
        if state.Hover.IsSome then
          ariaControls listId
          ariaActiveDescendant optionId
        onBlur (fun _ -> setState initialState)

        match props.DisplayString with
        | Some toString ->
          prop.type'.search
          placeholder " "
          Interop.mkAttr "aria-autocomplete" "list"
          value state.Search
          onChange (fun str ->
            let options = props.Options |> Array.filter (toString >> lowerCase >> strContains (lowerCase str))
            setState {|
              state with
                Search = str
                Hover = state.Hover |> Option.defaultOrMap 0 (clampIndex options) |> Some
                Options = options
            |})
        | None ->
          prop.type'.text
          prop.inputMode.none
          readOnly true
      ]

      Html.span [
        style [ style.width props.Width ]
        children (props.Display props.Selected)
      ]
    ]
  ]

let selectList listRef clearHover (props: _ Props) (state: _ State) setState hover =
  ul [
    className Class.selectList
    prop.id listId
    role "listbox"
    prop.ref<Browser.Types.HTMLElement> listRef
    onMouseDown handleEvent
    if Array.isEmpty state.Options then
      children [
        li [
          className Class.empty
          ariaHidden true
          text "No results found..."
        ]
      ]
    else
      children (state.Options |> Array.mapi (fun i opt ->
        let active = hover = i
        li [
          if active then prop.id optionId
          role "option"
          ariaSelected active

          onMouseDown (fun e ->
            if e.button = 0 then
              props.Dispatch opt
              clearHover e)

          onMouseMove (fun _ ->
            if state.Hover <> Some i then
              setState {|
                state with
                  Hover = Some i
                  Scroll = false
              |})

          children (props.Display opt)
        ]
      ))
  ]

let [<ReactComponent>] private Select (props: _ Props) =
  let selectedIndex =
    props.Options
    |> Array.tryFindIndex ((=) props.Selected)
    |> Option.defaultValue 0

  let selectOffset e offset =
    handleEvent e
    let index = (selectedIndex + offset) |> clampIndex props.Options
    if index <> selectedIndex then
      props.Dispatch props.Options[index]

  let initialState = {|
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

    // manually scroll to avoid using scrollIntoView() which can scroll on parent elements
    match prev.Hover, state.Hover, listRef.current with
    | None, Some _, Some list ->
      list.children?item selectedIndex |> Option.iter (fun (elm: Browser.Types.HTMLElement) ->
        let listBounds = list.getBoundingClientRect ()
        let itemBounds = elm.getBoundingClientRect ()
        list.scrollTop <- itemBounds.top - listBounds.top - (listBounds.height - itemBounds.height) / 2.0)
    | Some _, Some i, Some list when state.Scroll ->
      list.children?item i |> Option.iter (fun (elm: Browser.Types.HTMLElement) ->
        let listBounds = list.getBoundingClientRect ()
        let itemBounds = elm.getBoundingClientRect ()
        if itemBounds.top < listBounds.top then
          list.scrollBy (0, itemBounds.top - listBounds.top)
        elif itemBounds.bottom > listBounds.bottom then
          list.scrollBy (0, itemBounds.bottom - listBounds.bottom))
    | _ -> ())

  let setHover e index =
    handleEvent e
    let index = Some index
    if state.Hover <> index then
      setState {|
        state with
          Hover = index
          Scroll = true
      |}

  let setHoverOffset e hover offset = (hover + offset) |> clampIndex state.Options |> setHover e

  let clearHover e =
    handleEvent e
    setState initialState

  div [
    className Class.select
    role "combobox"
    ariaExpanded state.Hover.IsSome

    onMouseDown (fun e ->
      if e.button = 0 then
        if state.Hover.IsSome
        then clearHover e
        else setHover e selectedIndex)

    onKeyDown (fun e ->
      match e.key, state.Hover with
      | "Escape", Some _ -> clearHover e

      | " ", Some hover
      | "Enter", Some hover
      | "Tab", Some hover ->
        if e.key <> " " || props.DisplayString.IsNone then
          state.Options |> Array.tryItem hover |> Option.iter props.Dispatch
          clearHover e

      | "ArrowUp", Some hover -> setHoverOffset e hover -1
      | "ArrowDown", Some hover -> setHoverOffset e hover 1
      | "PageUp", Some hover -> setHoverOffset e hover -pageSize
      | "PageDown", Some hover -> setHoverOffset e hover pageSize
      | "Home", Some hover -> setHoverOffset e hover -state.Options.Length
      | "End", Some hover -> setHoverOffset e hover state.Options.Length

      | " ", None
      | "Enter", None
      | "ArrowDown", None ->
        if e.key <> " " || props.DisplayString.IsNone then
          setHover e selectedIndex

      | "ArrowUp", None -> setHover e 0

      | "ArrowLeft", None -> selectOffset e -1
      | "ArrowRight", None -> selectOffset e 1
      | "PageUp", None -> selectOffset e -pageSize
      | "PageDown", None -> selectOffset e pageSize
      | "Home", None -> selectOffset e -props.Options.Length
      | "End", None -> selectOffset e props.Options.Length

      | _ -> ())

    children [
      div [
        selectControl initialState inputRef props state setState
        state.Hover |> ofOption (selectList listRef clearHover props state setState)
      ]
      img [ alt "" ]
    ]
  ]

let private ignoreSame selected dispatch value =
  if selected <> value then dispatch value

let search width toString display options selected dispatch =
  Select {|
    Width = width
    DisplayString = Some toString
    Display = display
    Options = options
    Selected = selected
    Dispatch = ignoreSame selected dispatch
  |}

let options width display options selected dispatch =
  Select {|
    Width = width
    DisplayString = None
    Display = display
    Options = options
    Selected = selected
    Dispatch = ignoreSame selected dispatch
  |}

let inline unitUnion width selected dispatch =
  options width (Reflection.getCaseName >> ofStr) unitUnionCases selected dispatch

let inline enum width (selected: 'a) dispatch =
  options width (Enum.name >> ofStr) Enum.values selected dispatch
