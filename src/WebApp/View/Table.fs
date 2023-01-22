module StardewValleyStonks.WebApp.View.Table

open StardewValleyStonks
open StardewValleyStonks.WebApp

open Feliz

open type Html
open type prop

type 'item Column = {
  Header: ReactElement
  Width: float
  Sort: (bool * ('item -> 'item -> int)) option
}

let private colWidths widths =
  colgroup (widths |> Seq.map (fun (width: float) ->
    col [ style [ style.width (length.percent width) ] ] ))

let sortTable columns displayItem setSort (sortCol, ascending) items =
  let columns = Array.ofSeq columns
  let sortedItems =
    match columns[sortCol].Sort with
    | Some (asc, sort) when asc = ascending -> Seq.sortWith sort items
    | Some (_, sort) -> Seq.sortWith (fun x y -> sort y x) items
    | None -> items

  let headers = columns |> Seq.mapi (fun i column ->
    th [
      if column.Sort.IsSome then
        onClick (fun _ -> setSort (i, if sortCol = i then not ascending else true))

      children [
        column.Header
        if columns[i].Sort.IsSome then
          Html.span [
            if sortCol = i then
              text "^"
              if not ascending then
                style [
                  style.display.inlineBlock
                  style.transform.scale(1, -1)
                ]
            else
              if column.Sort.IsSome then
                text "-"
          ]
      ]
    ] )

  table [
    // className "table"
    children [
      // colWidths (columns |> Seq.map Column.width)
      thead [ tr headers ]
      tbody (sortedItems |> Seq.map displayItem)
    ]
  ]
