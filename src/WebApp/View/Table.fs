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
    CollapsedRowCells: ReactElement array
    BodyCells: ReactElement array array
  |}) =
  let collapsed, setCollapsed = useState true

  fragment [
    if collapsed then
      tbody [
        onClick (fun _ -> setCollapsed false)
        children [
          tr [
            td []
            fragment props.CollapsedRowCells
          ]
        ]
      ]

    tbody [
      if collapsed then style [ style.visibility.collapse ]
      children (props.BodyCells |> Seq.mapi (fun i row ->
        tr [
          if i = 0 then onClick (fun _ -> setCollapsed true)
          children [
            td []
            fragment row
          ]
        ]
      ))
    ]
  ]

let collapsibleBody collapsedRow rows = CollapsibleTableBody {|
  CollapsedRowCells = collapsedRow
  BodyCells = rows
|}
