module StardewValleyStonks.WebApp.View.Table

open StardewValleyStonks
open StardewValleyStonks.WebApp

open Feliz

open type Html
open type prop

type 'item Column = {
  Header: ReactElement
  Width: float
  Sort: ('item -> 'item -> int) option
}

let sortTable columns displayItem setSort sortCols items =
  let columns = Array.ofSeq columns
  let sortData = Array.create columns.Length None
  let numSorts, items =
    sortCols |> List.fold (fun (i, items) (col, asc) ->
      sortData[col] <- Some (i, asc)
      i + 1,
      match columns[col].Sort with
      | Some sort -> items |> Seq.sortDirectionWith asc sort
      | None -> items)
      (0, items)

  let headers = columns |> Seq.mapi (fun i column ->
    th [
      if column.Sort.IsSome then
        onClick (fun e -> setSort (e.shiftKey || e.ctrlKey, (i, sortData[i] |> Option.defaultOrMap true snd)))
      children [
        column.Header
        Html.span [
          match sortData[i] with
          | Some (i, asc) ->
            let num = if numSorts > 1 then string (numSorts - i) else ""
            text $"^{num}"
            if not asc then
              style [
                style.display.inlineBlock
                style.transform.scale(1, -1)
              ]
          | None ->
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
      tbody (items |> Seq.map displayItem)
    ]
  ]
