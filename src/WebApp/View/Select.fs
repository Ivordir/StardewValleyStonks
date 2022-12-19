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

let private handle (event: Browser.Types.Event) =
  event.stopPropagation ()
  event.preventDefault ()

[<ReactComponent>]
let private Select (props: {|
    Width: Styles.ICssUnit
    ToString: ('a -> string) option
    Display: 'a -> ReactElement
    Options: 'a array
    Selected: 'a
    Dispatch: 'a -> unit
  |})
  =
  let selectedIndex = props.Options |> Array.tryFindIndex ((=) props.Selected)

  let selectOffset e offset =
    let index =
      match selectedIndex with
      | Some selectedIndex ->
        let index = selectedIndex + offset
        if index < 0 then
          if selectedIndex = 0 then None else Some 0
        elif index >= props.Options.Length then
          let last = props.Options.Length - 1
          if selectedIndex = last then None else Some last
        else
          Some index
      | None ->
        if props.Options.Length > 0
        then Some 0
        else None

    match index with
    | Some i -> props.Dispatch props.Options[i]
    | None -> ()

    handle e


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

  useEffect (fun () ->
    match prev.Hover, state.Hover with
    | None, Some _ ->
      match inputRef.current with
      | Some input -> input.focus ()
      | None -> ()

      match listRef.current with
      | Some list ->
        let elm = list.children.item selectedIndex
        if not (isNullOrUndefined elm) then
          elm?scrollIntoView {| block = "center" |}
      | None -> ()

    | Some _, Some i when state.Scroll ->
      match listRef.current with
      | Some list ->
        let elm = list.children.item i
        if not (isNullOrUndefined elm) then
          elm?scrollIntoView {| block = "nearest" |}
      | None -> ()

    | _ -> ())

  let setHover e i =
    handle e
    if state.Hover |> Option.contains i then () else
    setState {|
      state with
        Focused = true
        Hover = Some i
        Scroll = true
    |}

  let clearHover e =
    handle e
    setState {| initialState with Focused = true |}

  div [
    className "select-container"
    style [ style.width props.Width ]
    children [
      div [
        classes [
          "select"
          if state.Focused then "focused"
        ]
        onMouseDown (fun e ->
          if e.button = 0 then
            match state.Hover with
            | Some _ -> clearHover e
            | None -> setHover e (selectedIndex |> Option.defaultValue 0))
        onKeyDown (fun e ->
          match e.key, state.Hover with
          | "Escape", Some _ -> clearHover e

          | "Enter", Some hover
          | " ", Some hover ->
            state.Options |> Array.tryItem hover |> Option.iter props.Dispatch
            clearHover e

          | "Enter", None
          | " ", None -> setHover e (selectedIndex |> Option.defaultValue 0)

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
            classes [
              "select-control"
              if state.Search <> "" then "searching"
            ]
            children [
              match props.ToString with
              | Some toString ->
                input [
                  className "select-input-search"
                  prop.ref inputRef
                  value state.Search
                  prop.type'.text
                  onFocus (fun _ -> setState {| state with Focused = true |})
                  onBlur (fun _ -> setState initialState)
                  onChange (fun (str: string) ->
                    let str = str.ToLower ()
                    let options = props.Options |> Array.filter (fun opt -> (toString opt).ToLower().Contains str)
                    setState {|
                      state with
                        Search = str
                        Hover = state.Hover |> Option.defaultOrMap 0 (min (options.Length - 1) >> max 0) |> Some
                        Options = options
                    |})
                ]
              | None ->
                input [
                  className "select-input-hidden"
                  prop.ref inputRef
                  prop.inputMode.none
                  onFocus (fun _ -> setState {| state with Focused = true |})
                  onBlur (fun _ -> setState initialState)
                ]

              div (props.Display props.Selected)
            ]
          ]
        ]
      ]

      match state.Hover with
      | None -> none
      | Some hover ->
        div [
          className "select-list"
          tabIndex -1
          if not state.Scroll then
            onMouseMove (fun _ -> setState {| state with Scroll = true |})
          children (ul [
            prop.ref listRef
            children (state.Options |> Array.mapi (fun i opt ->
              li [
                onMouseDown (fun e ->
                  if e.button = 0 then
                    props.Dispatch opt
                    clearHover e)
                if state.Scroll then
                  onMouseMove (fun _ ->
                    if state.Hover |> Option.contains i then () else
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
          ])
        ]
    ]
  ]

let search minWidth toString display options selected dispatch =
  Select {|
    Width = minWidth
    ToString = Some toString
    Display = display
    Options = options
    Selected = selected
    Dispatch = dispatch
  |}

let select minWidth display options selected dispatch =
  Select {|
    Width = minWidth
    ToString = None
    Display = display
    Options = options
    Selected = selected
    Dispatch = dispatch
  |}

let inline unitUnion minWidth (selected: 'a) dispatch =
  select minWidth (Reflection.getCaseName >> Html.text) unitUnionCases<'a> selected dispatch
