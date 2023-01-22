module StardewValleyStonks.WebApp.View.Visualization

open StardewValleyStonks
open StardewValleyStonks.WebApp
open StardewValleyStonks.WebApp.Update
open StardewValleyStonks.WebApp.View

open Fable.Core.JsInterop
open Elmish.React
open Feliz
open Feliz.Recharts

open type Html
open type prop
open type React

open Core.Operators
open Core.ExtraTopLevelOperators

let allPairData metric timeNorm data settings =
  let crops = Query.Selected.inSeasonCrops data settings |> cropOrder data |> Array.ofSeq

  let fertilizers =
    Query.Selected.fertilizers data settings
    |> fertilizerOrder
    |> Seq.map Some
    |> Array.ofSeq
  let fertilizers =
    if settings.Selected.NoFertilizer
    then Array.append [| None |] fertilizers
    else fertilizers

  let metric =
    match metric with
    | Gold -> Query.cropProfit
    | ROI -> Query.cropROI
    | XP -> Query.cropXP

  let data = crops |> Array.collect (fun crop ->
    let profit = metric data settings timeNorm crop
    fertilizers |> Array.map (fun fert ->
      (Crop.seed crop, Fertilizer.Opt.name fert), profit fert))

  {|
    Crops = crops |> Array.map Crop.seed
    Fertilizers = fertilizers |> Array.map Fertilizer.Opt.name
    Pairs = data
  |}


module Ranker =
  let selectFromGraph rankItem (pairs: _ array) dispatch i =
    let crop, fert = pairs[i]
    (match rankItem with
    | RankCropsAndFertilizers -> (Some crop, Some fert)
    | RankCrops -> (Some crop, None)
    | RankFertilizers -> (None, Some fert))
    |> Some
    |> SetSelectedCropAndFertilizer
    |> dispatch

  let pairImage (data: GameData) (pairs: (SeedId * string option) array) selectPair props =
    let index: int = props?payload?value
    let crop, fert = pairs[index]
    Svg.svg [
      svg.className "pairSelect"
      svg.onClick (fun _ -> selectPair index)
      svg.x (props?x - 10)
      svg.y (props?y: int)
      svg.width 20
      svg.height 40
      svg.children [
        Svg.image [
          svg.href <| Image.itemRoot (Crop.mainItem data.Crops[crop] |> string)
          svg.width 20
          svg.height 20
        ]
        match fert with
        | Some f ->
          Svg.image [
            svg.href <| Image.fertilizerRoot (string f)
            svg.width 20
            svg.height 20
            svg.y 20
          ]
        | None -> yield! []
      ]
    ]

  let chartTooltip (data: GameData) (pairs: (SeedId * string option) array) props =
    // number of harvests
    // item qualities / amounts per harvest
    // products sold + amount + price
    // fertilizer bought, replaced
    // seeds bought, made
    // profit / roi
    // days used / total season days
    // normalized profit /oi
    match props?payload with
    | Some (payload: _ array) when payload.Length > 0 && props?active ->
      let (index: int, result: Result<float, Query.InvalidReasons>) = payload[0]?payload
      let crop, fert = pairs[index]
      let fertDesc = Option.defaultOrMap "" (fun f -> " with " + string f)
      div [
        div (ofStr (Crop.name data.Items.Find data.Crops[crop] + fertDesc fert))
        match result with
        | Ok profit -> div (ofFloat profit)
        | Error _ -> none
      ]
    | _ -> none

  let private getPath x y width height =
    $"M {x},{y} h {width} v {height} h -{width} Z"

  let barBackground gap selectPair props =
    let x: float = props?x - gap / 2.0
    let y: float = props?y
    let width: float = props?width + gap
    let height: float = props?height
    let i: int = fst props?payload
    Svg.path [
      svg.classes [ "recharts-rectangle"; "pairSelect" ]
      svg.fill "#00000000"
      svg.onClick (fun _ -> selectPair i)
      svg.radius 0
      svg.x x
      svg.y y
      svg.width width
      svg.height height
      svg.d (getPath x y width height)
    ]

  let errorBar (pairs: (SeedId * string option) array) props =
    let x: float = props?x
    let y: float = props?y
    let width: float = props?width
    let height: float = props?height
    match snd props?payload with
    | Ok _ ->
      Svg.path [
        svg.className "recharts-rectangle"
        svg.fill "blue"
        svg.radius 0
        svg.x x
        svg.y y
        svg.width width
        svg.height height
        svg.d (getPath x y width height)
      ]
    | Error (flags: Query.InvalidReasons) ->
      let crop, fert = pairs[fst props?payload]
      let maxHeight = width * 3.0
      let y: float = props?background?height - maxHeight
      Svg.svg [
        svg.x x
        svg.y y
        svg.width width
        svg.height maxHeight
        svg.children [
          if flags.HasFlag Query.InvalidReasons.NoFertilizerPrice then
            Svg.image [
              svg.href <| Image.fertilizerRoot (string fert)
              svg.height width
            ]
          if flags.HasFlag Query.InvalidReasons.NotEnoughSeeds then
            Svg.image [
              svg.href <| Image.itemRoot (string crop)
              svg.height width
              svg.y width
            ]
          if flags.HasFlag Query.InvalidReasons.NotEnoughDays then
            Svg.image [
              svg.href <| Image.uiRoot "Time"
              svg.width width
              svg.height width
              svg.y (width * 2.0)
            ]
        ]
      ]

  let graph ranker model pairs (data: _ array) dispatch =
    let barGap = 4.0
    let selectPair = selectFromGraph ranker.RankItem pairs dispatch
    Recharts.responsiveContainer [
      responsiveContainer.width (length.percent 100)
      responsiveContainer.chart (Recharts.barChart [
        barChart.data data
        barChart.barSize 40
        barChart.barGap barGap
        barChart.children [
          Recharts.yAxis [
            yAxis.unit (RankMetric.unit ranker.RankMetric)
            yAxis.width 60
          ]
          Recharts.tooltip [
            tooltip.content (chartTooltip model pairs)
          ]
          Recharts.bar [
            bar.dataKey (snd >> (function Ok y -> y | Error _ -> 0.0))
            bar.fill "blue"
            bar.onClick (fun props -> fst props?payload |> selectPair)
            Interop.mkBarAttr "background" (barBackground barGap selectPair)
            Interop.mkBarAttr "shape" (errorBar pairs)
          ]
          Recharts.brush [
            brush.startIndex (ranker.BrushSpan |> fst |> int |> min (data.Length - 1) |> max 0)
            brush.endIndex (ranker.BrushSpan |> snd |> int |> min (data.Length - 1) |> max 0)
            brush.height 30
            Interop.mkBrushAttr "onChange" (fun i -> dispatch <| SetBrushSpan (i?startIndex, i?endIndex))
          ]
          Recharts.xAxis [
            xAxis.dataKey (fst: _ -> int)
            xAxis.tick (pairImage model pairs selectPair)
            xAxis.interval 0
            xAxis.height 50
          ]
        ]
      ] )
    ]

  let rankBy labelText ranker dispatch =
    fragment [
      ofStr labelText
      Select.options (length.rem 4) (fun metric ->
        div [
          text (string metric)
          title (RankMetric.fullName metric)
        ])
        unitUnionCases<RankMetric>
        ranker.RankMetric
        (SetRankMetric >> dispatch)
      Select.unitUnion (length.rem 7) ranker.TimeNormalization (SetTimeNormalization >> dispatch)
    ]

  let ranker ranker (data, settings) dispatch =
    let pairData = allPairData ranker.RankMetric ranker.TimeNormalization data settings
    if pairData.Pairs.Length = 0 then
      div [
        if pairData.Crops.Length = 0 then ofStr "No Crops Selected"
        if pairData.Fertilizers.Length = 0 then ofStr "No Fertilizers Selected"
      ]
    else
      let pairs =
        match ranker.RankItem with
        | RankCropsAndFertilizers -> pairData.Pairs
        | RankCrops -> pairData.Pairs |> Array.groupBy (fst >> fst) |> Array.map (snd >> Array.maxBy (snd >> Option.ofResult))
        | RankFertilizers -> pairData.Pairs |> Array.groupBy (fst >> snd) |> Array.map (snd >> Array.maxBy (snd >> Option.ofResult))

      let pairs =
        if ranker.ShowInvalid then pairs else
        pairs |> Array.filter (snd >> Result.isOk)

      if pairs.Length = 0 then
        div [
          ofStr "No valid items found"
        ]
      else
        let pairData =
          pairs
          |> Array.indexed
          |> Array.map (fun (i, (_, profit)) -> i, profit)

        let pairs = pairs |> Array.map fst

        let setOrder a b =
          let rec next a b =
            if int a = 0 || int b = 0 then
              compare a b
            else
              let c = compare (int b &&& 1) (int a &&& 1)
              if c = 0
              then next (a >>> 1) (b >>> 1)
              else c
          next a b

        pairData |> Array.sortInPlaceWith (fun a b ->
          match snd a, snd b with
          | Ok a, Ok b -> compare b a
          | Ok _, Error _ -> -1
          | Error _, Ok _ -> 1
          | Error a, Error b -> setOrder a b)

        fragment [
          div [ Class.graphControls; children [
            ofStr "Rank"
            Select.options (length.rem 6) (fun rankby ->
              div [
                prop.text (string rankby)
                title (
                  match rankby with
                  | RankCropsAndFertilizers -> "All pairs of crops and fertilizers."
                  | RankCrops -> "Pick the best fertilizer for each crop."
                  | RankFertilizers -> "Pick the best crop for each fertilizer."
                )
              ])
              unitUnionCases<RankItem>
              ranker.RankItem
              (SetRankItem >> dispatch)

            rankBy "By" ranker dispatch

            checkboxText "Show Invalid" ranker.ShowInvalid (SetShowInvalid >> dispatch)
          ] ]
          div [
            Class.graph
            children [
              lazyView3With
                (fun (data1, _) (data2, _) -> data1 = data2)
                (fun (pairs, pairData) (ranker, data) -> graph ranker data pairs pairData)
                (pairs, pairData)
                (ranker, data)
                dispatch
            ]
          ]
        ]

let growthCalender app seed fertilizer =
  let data = app.Data
  let settings = app.State.Settings
  let crop = data.Crops[seed]
  let fert = fertilizer |> Option.map data.Fertilizers.Find
  match Query.bestGrowthSpan settings.Game fert crop with
  | None -> ofStr (if Game.cropIsInSeason settings.Game crop then "No harvests possible!" else "Crop not in season!")
  | Some span ->
    let firstStageImages =
      span.Stages
      |> Array.mapi (fun i stage ->
        Array.create (int stage) (div [ Image.growthStage i seed ]))
      |> Array.concat
    let last = [| div (Image.item' <| Crop.mainItem crop) |]
    let stageImages =
      let first =
        match Crop.regrowTime crop with
        | Some time -> Array.create (int time - 1) (div [ Image.regrowStage seed ])
        | None -> Array.tail firstStageImages
      Array.append first last
    let firstStageImages = Array.append firstStageImages last

    let disabledDay = div [ Class.disabled ]

    div [
      Class.calendar
      children [
        let harvests = span.Harvests
        let span = span.Span
        let unusedDays = span.TotalDays - nat firstStageImages.Length - (harvests - 1u) * nat stageImages.Length
        let days =
          [
            if settings.Game.StartDate.Season = span.StartSeason then
              Array.create (int (settings.Game.StartDate.Day - Date.firstDay)) disabledDay

            Array.create (int unusedDays) (div [])

            firstStageImages

            yield! Seq.replicate (int (harvests - 1u)) stageImages

            if settings.Game.EndDate.Season = span.EndSeason then
              Array.create (int (Date.lastDay - settings.Game.EndDate.Day)) disabledDay
          ]
          |> Array.concat
          |> Array.chunkBySize (int Date.daysInSeason)

        for i, season in Season.span span.StartSeason span.EndSeason |> Seq.indexed do
          let days = days[i]
          div [
            Class.calendarSeason
            children [
              div [
                Class.calendarHeader
                children (Image.Icon.season season)
              ]

              div [
                Class.calendarDays
                children days
              ]
            ]
          ]
      ]
    ]

// Previous Stockpiled Seed                                               1 seed
// or
// Initial Bought Seed from vendor                             -> -gold   1 seed
//                          fertilizer (price) from vendor     -> -gold
// item quality x amount -> product * quality (price) x amount -> gold
//                          item * quality (price) x amount    -> gold
// item quality x amount -> custom * quality (price) x amount  -> gold
//        itemA x amount ->
//        itemB x amount -> Forage Seeds (price) x amount      -> gold  +x seed
//        itemC x amount ->
// item quality x amount -> seedmaker                          ->       +x seed
//                                       item quality x amount ->       +x seed
//         replacement fertilizer (price) from vendor x amount -> -gold
//                   bought seeds (price) from vendor x amount -> -gold +x seed
// Seeds Used                                                           -x seed
//                                                          Total: gold, 1 seed

// (item q) amount -> (item q) price amount : gold seed

let profitBreakdownTable roi timeNorm (data: GameData) settings seed fertName =
  let crop = data.Crops[seed]

  if not <| Game.cropIsInSeason settings.Game crop then ofStr "Crop not in season!" else

  let fert = fertName |> Option.map data.Fertilizers.Find
  match Query.cropProfitAndFertilizerData data settings timeNorm crop fert with
  | None -> ofStr "No harvests possible!"
  | Some profitData ->
    let items = Crop.items crop
    let itemData = profitData.SoldAndSeedData

    let fertilizerBoughtRow replacement amount =
      match profitData.FertilizerPrice with
      | None -> none
      | Some fertCost ->
        tr [
          td [ colSpan 4; children [
            if replacement then ofStr "Replacement "
            Image.Icon.fertilizer <| Option.get fert
            match fertCost with
            | Some (NonCustom vendor, _) ->
              ofStr " from "
              Image.Icon.vendor vendor
            | Some (Custom (), _) ->
              ofStr " (Custom)"
            | None -> ofStr " from ???"
          ] ]
          td [
            match fertCost with
            | Some (_, cost) -> gold cost |> ofStr
            | None -> ofStr "???"
          ]
          td [
            ofStr " x "
            ofStr <| floatFixedRound amount
          ]
          td [
            match fertCost with
            | Some (_, cost) -> ofStr (goldFixedRound <| amount * -float cost)
            | None -> ofStr "???"
          ]
          td []
        ]

    let totalFooter =
      tfoot [
        match profitData.NetProfit with
        | None ->
          tr [
            td [
              colSpan 6
              text "Total"
            ]
            td [
              ofStr "???"
            ]
            td [
              match settings.Profit.SeedStrategy with
              | IgnoreSeeds -> none
              | StockpileSeeds -> ofStr "1.00"
              | BuyFirstSeed -> ofStr "0.00"
            ]
          ]
        | Some profit ->
          tr [
            td [
              colSpan 6
              text "Total"
            ]
            td [
              goldFixedRound profit |> ofStr
            ]
            td [
              match settings.Profit.SeedStrategy with
              | IgnoreSeeds -> none
              | StockpileSeeds -> ofStr "1.00"
              | BuyFirstSeed -> ofStr "0.00"
            ]
          ]
          if not roi && timeNorm <> TotalPeriod then
            tr [
              td [
                colSpan 6
              ]
              td [
                ofStr "/ "
                match timeNorm with
                | PerDay ->
                  profitData.TimeNormalization |> ofFloat
                  ofStr " days"
                | PerSeason ->
                  let value = floatRound profitData.TimeNormalization
                  ofStr value
                  ofStr (if value = "1" then " season" else " seasons")
                | TotalPeriod -> ofInt 1
              ]
              td []
            ]
            tr [
              td [
                colSpan 6
                text ("Total " + string timeNorm)
              ]
              td [
                floatFixedRound (profit / profitData.TimeNormalization) |> ofStr
                match timeNorm with
                | TotalPeriod -> none
                | PerDay -> ofStr "g/day"
                | PerSeason -> ofStr "g/season"
              ]
              td []
            ]
      ]

    div [ Class.breakdownTable; children [
      // div [
      //   let span = profitData.GrowthSpan.Span
      //   Image.Icon.season span.StartSeason
      //   if span.StartSeason <> span.EndSeason then
      //     Image.rightArrow
      //     Image.Icon.season span.EndSeason
      //   ofStr $" ({profitData.GrowthSpan.Harvests} harvests)"
      // ]

      table [
        thead [
          tr [
            th [
              colSpan 6
              text "Item"
            ]
            th [ ofStr "Profit" ]
            th [ if settings.Profit.SeedStrategy <> IgnoreSeeds then ofStr "Seeds" ]
          ]
        ]

        tbody [
          if settings.Profit.SeedStrategy = StockpileSeeds then
            tr [
              td [
                colSpan 6
                text "Previous Stockpiled Seed"
              ]
              td []
              td [ ofStr "1.00" ]
            ]

          fertilizerBoughtRow false 1.0

          if profitData.FertilizerBought > 1.0 then
            fertilizerBoughtRow true (profitData.FertilizerBought - 1.0)

          if itemData.SeedsBought > 0.0 then
            tr [
              td [ colSpan 4; children [
                Image.Icon.item' data (seed * 1u<_>)
                match profitData.SeedPrice with
                | Some (NonCustom vendor, _) ->
                  ofStr " from "
                  Image.Icon.vendor vendor
                | Some (Custom (), _) ->
                  ofStr " (Custom)"
                | None -> ofStr " from ???"
              ] ]
              td [
                match profitData.SeedPrice with
                | Some (_, cost) -> gold cost |> ofStr
                | None -> none
              ]
              td [
                ofStr " x "
                ofStr <| floatFixedRound itemData.SeedsBought
              ]
              td [
                match profitData.SeedPrice with
                | Some (_, cost) -> ofStr (goldFixedRound <| itemData.SeedsBought * -float cost)
                | None -> ofStr "???"
              ]
              td [
                ofStr <| floatFixedRound itemData.SeedsBought
              ]
            ]
        ]

        tbody (itemData.SeedAmounts |> Array.map (fun (item, amounts) ->
          if nat item = nat seed then
            fragment [
              for i = Quality.highest downto 0 do
              let quality = enum i
              let amount = amounts[quality]
              if amount = 0.0 then none else
              tr [
                td []
                td []
                td []
                td [
                  Image.Icon.itemQuality' data item quality
                ]
                td []
                td [
                  ofStr " x "
                  ofStr <| floatFixedRound amount
                ]
                td []
                td [
                  ofStr <| floatFixedRound amount
                ]
              ]
            ]
          elif item = Crop.mainItem crop then
            fragment [
              for i = Quality.highest downto 0 do
              let quality = enum i
              let amount = amounts[quality]
              if amount = 0.0 then none else
              tr [
                td [
                  Image.Icon.itemQuality' data item quality
                ]
                td [
                  ofStr " x "
                  ofStr <| floatFixedRound amount
                ]
                td [ Image.rightArrow ]
                td [
                  Image.Icon.productQuality data item (SeedsFromSeedMaker (seed * 1u<_>)) Quality.Normal
                ]
                td []
                td [
                  ofStr " x "
                  ofStr <| floatFixedRound (amount * Processor.seedMakerExpectedAmount seed)
                ]
                td []
                td [
                  ofStr <| floatFixedRound (amount * Processor.seedMakerExpectedAmount seed)
                ]
              ]
            ]
          else
            assert false
            none))

        if itemData.ForageSeedsSold > 0.0 || itemData.ForageSeedsUsed > 0.0 then
          let totalMade = itemData.ForageSeedsSold + itemData.ForageSeedsUsed
          tbody (items |> Array.mapi (fun i item ->
            tr [
              td [
                Image.Icon.item' data item
              ]
              td [
                ofStr " x "
                ofStr <| floatFixedRound (totalMade / float ForageCrop.forageSeedsPerCraft)
              ]
              td [ Image.rightArrow ]
              if i = 0 then
                td [ rowSpan items.Length; children [ Image.Icon.item' data (seed * 1u<_>) ] ]
                td [ rowSpan items.Length; children [
                  ofStr <| gold (Game.seedItemSellPrice data settings.Game seed)
                ] ]
                td [ rowSpan items.Length; children [
                  ofStr " x "
                  ofStr <| floatFixedRound totalMade
                ] ]
                td [ rowSpan items.Length; children [
                  if itemData.ForageSeedsSold > 0.0 then
                    ofStr <| goldFixedRound (itemData.ForageSeedsSold * float (Game.seedItemSellPrice data settings.Game seed))
                ] ]
                if itemData.ForageSeedsUsed > 0.0 then
                  td [ rowSpan items.Length; children [
                    ofStr <| floatFixedRound itemData.ForageSeedsUsed
                  ] ]
              ]
            ))

        tbody [
          if settings.Profit.SeedStrategy <> IgnoreSeeds then
            tr [
              td [
                colSpan 6
                text $"{profitData.Harvests} Harvests"
              ]
              td []
              td [ ofStr <| floatFixedRound (if Crop.regrows crop then -1.0 else -float profitData.Harvests) ]
            ]
        ]

        tbody [
          for i = 0 to items.Length - 1 do
            let item = items[i]
            let soldAmounts = itemData.SoldAmounts[i]
            let sellAs = profitData.SellAs[i]

            for i = Quality.highest downto 0 do
              let quality = enum i
              let amount = soldAmounts[quality]
              if amount = 0.0 then none else
              match sellAs with
              | Some sellAsData ->
                let product, profit = sellAsData[quality]
                tr [
                  if product = NonCustom None then
                    td []
                    td []
                    td []
                  else
                    td [
                      Image.Icon.itemQuality' data item quality
                    ]
                    td [
                      ofStr " x "
                      ofStr <| floatFixedRound amount
                    ]
                    td [ Image.rightArrow ]

                  match product with
                  | NonCustom None ->
                    td [
                      Image.Icon.itemQuality' data item quality
                    ]
                    td [
                      ofStr <| gold (nat profit)
                    ]
                    td [
                      ofStr " x "
                      ofStr <| floatFixedRound amount
                    ]
                  | NonCustom (Some product) ->
                    let amountPerItem = Product.amountPerItem product
                    td [
                      Image.Icon.productQuality data item product (Product.outputQuality settings.Game.ModData quality product)
                    ]
                    td [
                      ofStr <| gold (nat (profit / amountPerItem))
                    ]
                    td [
                      ofStr " x "
                      ofStr <| floatFixedRound (amount * amountPerItem)
                    ]
                  | Custom (price, preservesQuality) ->
                    td [
                      ofStr "Custom"
                    ]
                    td [
                      ofStr <| gold price
                    ]
                    td [
                      ofStr " x "
                      ofStr <| floatFixedRound amount
                    ]

                  td [
                    ofStr <| goldFixedRound (profit * amount)
                  ]
                  td []
                ]
              | None ->
                  tr [
                    td [
                      Image.Icon.itemQuality' data item quality
                    ]
                    td [
                      ofStr " x "
                      ofStr <| floatFixedRound amount
                    ]
                    td [ Image.rightArrow ]
                    td [ colSpan 3; text "???" ]
                    td []
                    td []
                  ]
        ]

        totalFooter
      ]

      if roi then
        let investment = profitData.Investment (settings.Profit.SeedStrategy = BuyFirstSeed)
        let roi = investment |> Option.bind profitData.ROI
        div [
          div [
            ofStr "Investment: "
            ofStr (investment |> Option.defaultOrMap "???" gold)
          ]
          div [
            ofStr "ROI: "
            ofStr (roi |> Option.defaultOrMap "???" (sprintf "%.2f%%"))
          ]
          match roi with
          | Some roi when timeNorm <> TotalPeriod ->
            div [
              ofStr "/ "
              match timeNorm with
              | PerDay ->
                profitData.TimeNormalization |> ofFloat
                ofStr " days"
              | PerSeason ->
                let value = floatRound profitData.TimeNormalization
                ofStr value
                ofStr (if value = "1" then " season" else " seasons")
              | TotalPeriod -> ofInt 1
            ]
            div [
              floatFixedRound (roi / profitData.TimeNormalization) |> ofStr
              match timeNorm with
              | TotalPeriod -> none
              | PerDay -> ofStr "%/day"
              | PerSeason -> ofStr "%/season"
            ]
          | _ -> none
        ]
    ] ]

let xpBreakdownTable timeNorm (data: GameData) settings seed fertName =
  let crop = data.Crops[seed]
  let fert = fertName |> Option.map data.Fertilizers.Find
  match Query.cropXpAndFertilizerData data settings timeNorm crop fert with
  | Error e ->
    div [
      if e.HasFlag Query.InvalidReasons.NotEnoughDays then
        ofStr (if not <| Game.cropIsInSeason settings.Game crop then "Crop not in season!" else "No harvests possible!")
      if e.HasFlag Query.InvalidReasons.NoFertilizerPrice then
        ofStr "No Fertilizer Price!"
      if e.HasFlag Query.InvalidReasons.NotEnoughSeeds then
        ofStr "No Seed Source!"
    ]
  | Ok data ->
    div [
      let total = data.XpPerHarvest * float data.Harvests
      div [
        ofStr (sprintf "%.2fxp" data.XpPerHarvest)
        ofStr " x "
        ofStr $"{data.Harvests} harvests"
        ofStr " = "
        ofStr (sprintf "%.2fxp" total)
      ]
      if timeNorm <> TotalPeriod then
        let unit =
          match timeNorm with
          | PerDay -> "day"
          | PerSeason -> "season"
          | TotalPeriod -> ""
        div [
          ofStr (sprintf "%.2fxp" total)
          ofStr " / "
          let value = floatRound data.TimeNormalization
          ofStr value
          ofStr " "
          ofStr (if value = "1" then unit else (unit + "s"))
          ofStr " = "
          ofStr (sprintf "%.2fxp/%s" (total / data.TimeNormalization) unit)
        ]
    ]

let private selectSpecificOrBest name toString (viewItem: _ -> ReactElement) items selected dispatch =
  Select.search
    (length.rem 15)
    (function
      | Choice1Of2 item
      | Choice2Of2 (Some item) -> toString item
      | Choice2Of2 None -> "???"
    )
    (fun opt -> fragment [
      match opt with
      | Choice1Of2 item -> viewItem item
      | Choice2Of2 bestItem ->
        ofStr $"Best {name}: "
        bestItem |> Option.defaultOrMap (ofStr "???") viewItem
    ])
    items
    selected
    dispatch


let [<ReactComponent>] SelectedCropAndFertilizer (props: {|
    App: _
    Seed: _
    Fertilizer: _
    Dispatch: _
  |}) =
  let app = props.App
  let seed = props.Seed
  let fertName = props.Fertilizer
  let dispatch = props.Dispatch

  let appDispatch = dispatch
  let dispatch = SetRanker >> dispatch
  let data = app.Data
  let { UI = ui; Settings = settings } = app.State
  let ranker = ui.Ranker

  let (metric, timeNorm), setState = useState ((ranker.RankMetric, ranker.TimeNormalization))

  let crops = Query.Selected.inSeasonCrops data settings |> cropOrder data |> Array.ofSeq

  let fertilizers =
    Query.Selected.fertilizers data settings
    |> fertilizerOrder
    |> Seq.map Some
    |> Array.ofSeq
  let fertilizers =
    if settings.Selected.NoFertilizer
    then Array.append [| None |] fertilizers
    else fertilizers

  let metricValue =
    match metric with
    | Gold -> Query.cropProfit
    | ROI -> Query.cropROI
    | XP -> Query.cropXP

  let bestCrop, bestFert =
    let pairs = crops |> Array.collect (fun crop ->
      let profit = metricValue data settings timeNorm crop
      fertilizers |> Array.map (fun fert ->
        (Crop.seed crop, Fertilizer.Opt.name fert), profit fert))

    let bestCrop, bestFert =
      if pairs.Length = 0 then None, None else
      let crop, fert = pairs |> Array.maxBy (snd >> Option.ofResult) |> fst
      Some data.Crops[crop], Some (Option.map data.Fertilizers.Find fert)

    let bestCrop =
      match fertName with
      | Some fert' ->
        let fert = Option.map data.Fertilizers.Find fert'
        if crops.Length = 0 then None else
        crops |> Array.maxBy (fun crop -> metricValue data settings timeNorm crop fert |> Option.ofResult) |> Some
      | None -> bestCrop

    let bestFert =
      match seed with
      | Some seed ->
        let crop = data.Crops[seed]
        if fertilizers.Length = 0 then None else
        let profit = metricValue data settings timeNorm crop
        fertilizers |> Array.maxBy (profit >> Option.ofResult) |> Some
      | None -> bestFert

    bestCrop, bestFert

  let cropOptions =
    crops
    |> Array.map Choice1Of2
    |> Array.append [| Choice2Of2 bestCrop |]

  let fertilizerOptions =
    fertilizers
    |> Array.map Choice1Of2
    |> Array.append [| Choice2Of2 bestFert |]

  let crop = seed |> Option.defaultOrMap (Choice2Of2 bestCrop) (data.Crops.Find >> Choice1Of2)
  let fert = fertName |> Option.defaultOrMap (Choice2Of2 bestFert) (Option.map data.Fertilizers.Find >> Choice1Of2)

  div [ Class.auditGraph; children [
    button [
      onClick (fun _ -> SetSelectedCropAndFertilizer None |> dispatch)
      text "Back"
    ]

    div [ Class.auditGraphSelect; children [
      div [
        selectSpecificOrBest
          "Crop"
          (Crop.name data.Items.Find)
          (Image.Icon.crop data)
          cropOptions
          crop
          (fun opt ->
            let seed =
              match opt with
              | Choice1Of2 crop -> Some (Crop.seed crop)
              | Choice2Of2 _ -> None
            dispatch (SetSelectedCropAndFertilizer (Some (seed, fertName))))

        ofStr " with "

        selectSpecificOrBest
          "Fertilizer"
          Fertilizer.Opt.displayName
          (Option.defaultOrMap (ofStr "No Fertilizer") Image.Icon.fertilizer)
          fertilizerOptions
          fert
          (fun opt ->
            let fert =
              match opt with
              | Choice1Of2 fert -> Some (Fertilizer.Opt.name fert)
              | Choice2Of2 _ -> None
            dispatch (SetSelectedCropAndFertilizer (Some (seed, fert))))
      ]

      div [
        ofStr "Show "
        Select.options (length.rem 4) (fun metric ->
          div [
            text (string metric)
            title (RankMetric.fullName metric)
          ])
          unitUnionCases<RankMetric>
          metric
          (fun metric -> setState (metric, timeNorm))
        Select.unitUnion (length.rem 7) timeNorm (fun timeNorm -> setState (metric, timeNorm))
      ]
    ] ]

    match crop, fert with
    | (Choice1Of2 crop | Choice2Of2 (Some crop)), (Choice1Of2 fert | Choice2Of2 (Some fert)) ->
      let crop = Crop.seed crop
      let fert = Fertilizer.Opt.name fert

      animatedDetails
        (ui.OpenDetails.Contains OpenDetails.RankerProfitBreakdown)
        (ofStr "Profit Breakdown")
        [
          match metric with
          | Gold -> profitBreakdownTable false timeNorm data settings crop fert
          | XP -> xpBreakdownTable timeNorm data settings crop fert
          | ROI -> profitBreakdownTable true timeNorm data settings crop fert
        ]
        (curry SetDetailsOpen OpenDetails.RankerProfitBreakdown >> appDispatch)

      animatedDetails
        (ui.OpenDetails.Contains OpenDetails.RankerGrowthCalendar)
        (ofStr "Growth Calendar")
        [ growthCalender app crop fert ]
        (curry SetDetailsOpen OpenDetails.RankerGrowthCalendar >> appDispatch)

    | _ ->
      div [
        if fertilizers.Length = 0 then ofStr "No fertilizers selected!"
        if crops.Length = 0 then ofStr "No crops selected!"
      ]
  ] ]


let rankerOrAudit app dispatch =
  let appDispatch = dispatch
  let dispatch = SetRanker >> dispatch
  let { UI = ui; Settings = settings } = app.State
  let ranker = ui.Ranker
  match ranker.SelectedCropAndFertilizer with
  | Some (crop, fert) -> SelectedCropAndFertilizer {| App = app; Seed = crop; Fertilizer = fert; Dispatch = appDispatch |}
  | None -> lazyView3 Ranker.ranker ranker (app.Data, settings) dispatch



let private workerQueue, private workerSubscribe =
  let queue, subscribe = Solver.createWorker ()
  debouncer 200 (fun (data, settings, mode) -> queue data settings mode), subscribe

// The solver typically takes < 50ms, but with
//   StartSeason = Spring, StopSeason = Fall, Location = Greenhouse, and FarmingLevel in [0..7],
// it can take around 600ms if all fertilizers and crops are selected.
// For this reason, the solver is put in a web worker with a debouncer and queue system.

let [<ReactComponent>] solver (props: {|
  Data: GameData
  Settings: Settings
  SolverMode: SolverMode
  |})
  =
  let (solution, solving), setState = useState ((None, false))

  workerSubscribe (fun solution -> setState (Some solution, false))

  useEffect ((fun () ->
    setState (solution, true)
    workerQueue (props.Data, props.Settings, props.SolverMode)
  ), [| box props.Data; box props.Settings; props.SolverMode |])

  match solution with
  | Some (solution, total) ->
    div [
      if solving then className "disabled"
      children [
        ofStr $"Total: {total}"
        yield!
          solution
          |> Seq.map (fun res ->
            res.Variables |> Seq.map (fun ((season, var), i) ->
              div (ofStr $"{Season.name season} {var} {res.Fertilizer}: {i}")))
          |> Seq.concat
      ]
    ]
  | None ->
    div [
      ofStr "Loading..."
    ]

let section app dispatch =
  let { UI = ui; Settings = settings } = app.State
  let rankerChart = ui.Mode = Ranker && ui.Ranker.SelectedCropAndFertilizer.IsNone
  section [
    prop.id (if rankerChart then "visualization-graph" else "visualization")
    children [
      viewTabs SetAppMode unitUnionCases<AppMode> ui.Mode (SetUI >> dispatch)
      match ui.Mode with
      | Ranker -> rankerOrAudit app (SetUI >> dispatch)
      | Solver ->
        labeled "Maximize: " <| Select.unitUnion (length.rem 5) ui.SolverMode (SetSolverMode >> SetUI >> dispatch)
        solver {|
          Data = app.Data
          Settings = settings
          SolverMode = ui.SolverMode
        |}
    ]
  ]
