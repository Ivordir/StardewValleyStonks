module StardewValleyStonks.WebApp.View.Table

open StardewValleyStonks
open StardewValleyStonks.WebApp

open Feliz

open type Html
open type prop
open type React

[<RequireQualifiedAccess>]
module Column =
  let header header = header, None
  let withSort sort header = header, Some sort

let columnCheckbox (keys: _ Set) selected dispatch =
  checkbox (keys.IsSubsetOf selected) (curry Update.SetManySelected keys >> dispatch)

let sortTable columns displayItem setSort (sortCol, ascending) items =
  let columns = Array.ofSeq columns
  let sortedItems =
    match snd columns[int sortCol] with
    | Some sort when ascending -> Seq.sortWith sort items
    | Some sort -> Seq.sortWith (fun x y -> sort y x) items
    | None -> items

  let headers = columns |> Seq.mapi (fun i (header: ReactElement, sort) ->
    let i = nat i
    th [
      if sort.IsSome then
        onClick (fun _ -> setSort (i, if sortCol = i then not ascending else true))

      children [
        header
        if sort.IsSome then
          Html.span [
            if sortCol = i then
              text "^"
              if not ascending then
                style [
                  style.display.inlineBlock
                  style.transform.scale(1, -1)
                ]
            else
              text "-"
          ]
      ]
    ])

  table [
    // className "table"
    children [
      thead [ tr headers ]
      tbody (sortedItems |> Seq.map displayItem)
    ]
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

  fragment [
    tbody [
      onClick (fun _ -> setCollapsed (not collapsed))
      children [
        tr [
          td []
          props.Header
        ]
      ]
    ]

    tbody [
      if collapsed then style [ style.visibility.collapse ]
      children (props.Rows |> Array.map (fun row -> tr [ td []; row ]))
    ]
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
        th [
          onClick (fun _ -> setCollapsed (not collapsed))
          text "v"
        ]
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
