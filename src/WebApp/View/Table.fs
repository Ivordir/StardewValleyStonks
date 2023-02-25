module StardewValleyStonks.WebApp.View.Table

open StardewValleyStonks
open StardewValleyStonks.WebApp
open StardewValleyStonks.WebApp.Update

open Feliz

open type Html
open type prop
open type React

let private compareWithDirection comparer ascending =
  if ascending
  then fun a b -> comparer a b
  else fun a b -> comparer b a

let private optionCompareByWith projection compare a b =
  match projection a, projection b with
  | Some a, Some b -> compare a b
  | Some _, None -> -1
  | None, Some _ -> 1
  | None, None -> 0

let optionCompareDirection projection ascending =
  optionCompareByWith projection (compareWithDirection compare ascending)


type Column<'a, 'b> when 'b: comparison = {
  Header: ReactElement
  Display: 'a -> ReactElement
  Sort: (bool -> ('a -> 'a -> int)) option
  Select: (('b -> bool option) * ('b SelectionMessage -> unit)) option
  Key: bool
  Disabled: bool
}

[<RequireQualifiedAccess>]
module Column =
  let createWith header display sort = {
    Header = header
    Display = display
    Sort = sort
    Select = None
    Key = false
    Disabled = false
  }

  let create header display = createWith header display None

  let sortable header display sortBy = createWith header display (Some (compareByDirection sortBy))
  let sortableOpt header display sortBy = createWith header display (Some (optionCompareDirection sortBy))

  let valueSortable header value display =
    createWith header (value >> display) (Some (compareByDirection value))

  let valueOptSortable header value display =
    createWith header (value >> display) (Some (optionCompareDirection value))

  let markAsKey column = { column with Key = true }

  let withDisabled disabled column = { column with Disabled = disabled }
  let withSelect selected dispatch column = { column with Select = Some (selected, dispatch) }


let private tableHeader key columns items sortCol ascending sortDispatch =
  thead [
    tr (columns |> Array.mapi (fun i column ->
      let i = nat i
      let active = sortCol = i
      let sortable = column.Sort.IsSome

      fragment [
        column.Select |> ofOption (fun (selected, dispatch) ->
          let allSelected, keys =
            ((true, Set.empty), items) ||> Array.fold (fun acc item ->
              let key = key item
              (acc, selected key) ||> Option.fold (fun (allSelected, keys) selected ->
                allSelected && selected, keys |> Set.add key))

          th [
            scope "col"
            className [
              Class.columnSelect
              if column.Disabled then Class.disabled
            ]
            children [
              if keys.IsEmpty
              then Input.checkbox "Select All" false ignore
              else Input.checkbox "Select All" allSelected (curry SetManySelected keys >> dispatch)
            ]
          ])

        th [
          scope "col"
          className [
            if sortable then Class.columnSort
            if column.Disabled then Class.disabled
          ]

          if sortable && active then
            Interop.mkAttr "aria-sort" (if ascending then "ascending" else "descending")

          children [
            if sortable then
              button [
                onClick (fun _ -> sortDispatch (i, if active then not ascending else true))
                ariaPressed active
                children column.Header
              ]
            else
              column.Header
          ]
        ]
      ]
    ))
  ]

let private tableBody key columns items rowDisabled =
  tbody (items |> Array.map (fun item ->
    let key = key item
    tr [
      prop.key (string key)
      if rowDisabled item then className Class.disabled
      children (columns |> Array.map (fun column ->
        fragment [
          column.Select |> ofOption (fun (selected, dispatch) ->
            td [
              className [
                Class.columnSelect
                if column.Disabled then Class.disabled
              ]
              children (selected key |> ofOption (fun selected ->
                Input.checkbox "Select" selected (curry SetSelected key >> dispatch)))
            ])

          if column.Key then
            th [
              scope "row"
              if column.Disabled then className Class.disabled
              children (column.Display item)
            ]
          else
            td [
              if column.Disabled then className Class.disabled
              children (column.Display item)
            ]
        ]
      ))
    ]
  ))

let tableFromColummsWithRowDisable rowDisabled key items (sortCol: nat, ascending) sortDispatch columns =
  let items =
    columns
    |> Array.tryItem (int sortCol)
    |> Option.bind (fun col -> col.Sort)
    |> Option.fold (fun items sort -> Array.sortWith (sort ascending) items) items

  table [
    tableHeader key columns items sortCol ascending sortDispatch
    tableBody key columns items rowDisabled
  ]

let tableFromColumms key items sort sortDispatch columns =
  tableFromColummsWithRowDisable (konst false) key items sort sortDispatch columns


let private collapseButton expandLabel collapseLabel collapsed setCollapsed =
  button [
    className Class.collapseArrow
    ariaLabel (if collapsed then expandLabel else collapseLabel)
    ariaPressed (not collapsed)
    onClick (fun _ -> setCollapsed (not collapsed))
  ]

let [<ReactComponent>] private CollapsibleTableBody (props: {|
    key: string
    Header: ReactElement
    Rows: ReactElement array
    AllCollapsed: bool
    Collapsed: bool
  |}) =
  let collapsed, setCollapsed = useState props.Collapsed

  useEffect ((fun () ->
    setCollapsed props.AllCollapsed
  ), [| box props.AllCollapsed |])

  tbody [
    tr [
      className (if collapsed then Class.collapsed else Class.expanded)
      children [
        td (collapseButton "Expand" "Collapse" collapsed setCollapsed)
        props.Header
      ]
    ]

    fragment (props.Rows |> Array.map (fun row -> tr [ td []; row ]))
  ]

let [<ReactComponent>] private CollapsibleTable (props: {|
    Header: ReactElement
    Bodies: (string * ReactElement * ReactElement array) array
    Collapsed: bool
  |}) =
  let collapsed, setCollapsed = useState props.Collapsed

  fragment [
    thead [
      tr [
        td (collapseButton "Expand All" "Collapse All" collapsed setCollapsed)
        props.Header
      ]
    ]

    fragment (props.Bodies |> Array.map (fun (key, header, body) ->
      if body.Length = 0 then
        tbody [ tr [ td []; header ]]
      else
        CollapsibleTableBody {|
          key = key
          Header = header
          Rows = body
          AllCollapsed = collapsed
          Collapsed = props.Collapsed
        |}
      ))
  ]

let collapsibleHeaderAndBodies collapsed header bodies =
  CollapsibleTable {|
    Header = header
    Bodies = bodies
    Collapsed = collapsed
  |}
