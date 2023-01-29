module StardewValleyStonks.WebApp.View.Table

open StardewValleyStonks.WebApp

open Feliz

open type Html
open type prop

[<RequireQualifiedAccess>]
module Column =
  let header header = header, None
  let withSort sort header = header, Some sort

let columnCheckbox (keys: _ Set) selected dispatch =
  checkbox
    (keys.IsSubsetOf selected)
    (curry Update.SetManySelected keys >> dispatch)

let sortTable columns displayItem setSort (sortCol, ascending) items =
  let columns = Array.ofSeq columns
  let sortedItems =
    match snd columns[sortCol] with
    | Some sort when ascending -> Seq.sortWith sort items
    | Some sort -> Seq.sortWith (fun x y -> sort y x) items
    | None -> items

  let headers = columns |> Seq.mapi (fun i (header: ReactElement, sort) ->
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
    ] )

  table [
    // className "table"
    children [
      thead [ tr headers ]
      tbody (sortedItems |> Seq.map displayItem)
    ]
  ]
