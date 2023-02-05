module StardewValleyStonks.WebApp.View.Visualization

open StardewValleyStonks
open StardewValleyStonks.WebApp
open StardewValleyStonks.WebApp.Update
open StardewValleyStonks.WebApp.View

open Feliz

open type Html
open type prop
open type React

open Core.Operators
open Core.ExtraTopLevelOperators

module GrowthCalendar =
  let rec private repeatCons n items list =
    if n = 0u
    then list
    else repeatCons (n - 1u) items (items :: list)

  let stageImage stage seed = div (Image.growthStage stage seed)

  let stageImages stages seed =
    stages
    |> Array.mapi (fun i stage -> Array.create (int stage) (stageImage i seed))
    |> Array.concat
    |> Array.tail

  let private solverRegrowCropCalendarDays settings (days: nat array) fertilizer stageList (season, crop, harvests) =
    let totalDays = Array.sum days[(season + 1)..]
    let stages, time = Game.growthTimeAndStages settings.Game fertilizer (FarmCrop crop)
    let usedDays = Growth.daysNeededFor crop.RegrowTime time harvests
    let stageImages = stageImages stages crop.Seed

    let harvestItem = div (Image.item' crop.Item)
    let firstHarvest = Array.append stageImages [| harvestItem |]
    let regrow = Array.create (int crop.RegrowTime.Value) (div (Image.regrowStage crop.Seed))
    regrow[regrow.Length - 1] <- harvestItem
    let filler = int totalDays - int usedDays |> max 0
    let stageList = Array.create filler (div []) :: stageList
    let stageList = repeatCons (harvests - 1u) regrow stageList
    let stageList = firstHarvest :: stageList
    season, (totalDays + days[season] - usedDays - nat filler), stageList

  let solver settings single (span: Solver.FertilizerDateSpan) =
    let stageList = [ Array.create (int (Date.daysInSeason - span.EndDate.Day)) (div [ Class.disabled ]) ]
    let days =
      if not single && settings.Game.Location = Farm
      then Date.daySpan span.StartDate span.EndDate
      else [| Date.totalDays span.StartDate span.EndDate |]

    let season, remainingDays, stageList =
      span.RegrowCrop |> Option.defaultOrMap
        (days.Length - 1, Array.last days, stageList)
        (solverRegrowCropCalendarDays settings days span.Fertilizer stageList)

    let season, days, stageList =
      (span.CropHarvests, (season, int remainingDays, stageList)) ||> Array.foldBack (fun (crop, harvests, bridgeCrop) (season, remainingDays, stageList) ->
        let seed = Crop.seed crop
        let stages, time = Game.growthTimeAndStages settings.Game span.Fertilizer crop
        let stageImages = stageImages stages seed
        let harvestItem = [| div (Image.item' <| Crop.mainItem crop) |]
        if bridgeCrop then
          let filler = int remainingDays - int time |> max 0
          let days = int remainingDays + int days[season] - int time - filler
          let stageList =
            stageImages
            :: Array.create filler (stageImage stages.Length seed)
            :: harvestItem
            :: stageList
          season - 1, days, stageList
        else
          season,
          remainingDays - int (Growth.daysNeededFor None time harvests),
          repeatCons harvests (Array.append stageImages harvestItem) stageList)

    assert (season = 0)

    let firstStage =
      [|
        span.CropHarvests |> Array.tryHead |> Option.map (fun (crop, _,_) -> crop)
        span.RegrowCrop |> Option.map (fun (_, crop, _) -> FarmCrop crop)
      |]
      |> Array.tryPick id
      |> Option.map (Crop.seed >> stageImage 0)
      |> Option.toArray

    Array.create (int (settings.Game.StartDate.Day - 1u)) (div [ Class.disabled ])
    :: Array.create (days - 1) (div [])
    :: firstStage
    :: stageList
    |> Array.concat
    |> Array.chunkBySize (int Date.daysInSeason)
    |> Array.zip (Date.seasonSpan span.StartDate span.EndDate)
    |> Array.map (fun (season, days) ->
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
      ])

  let ranker app fertilizer crop =
    let settings, _ = app.State
    match Query.bestGrowthSpan settings.Game fertilizer crop with
    | None -> ofStr (if Game.cropIsInSeason settings.Game crop then "No harvests possible!" else "Crop not in season!")
    | Some (span: Query.GrowthSpan) ->
      let cropHarvests, regrowCrop =
        match crop with
        | FarmCrop crop when crop.RegrowTime.IsSome -> [||], Some (0, crop, span.Harvests)
        | _ -> [| crop, span.Harvests, false |], None

      let span: Solver.FertilizerDateSpan = {
        StartDate = {
          Season = span.StartSeason
          Day =
            if span.StartSeason = settings.Game.StartDate.Season
            then settings.Game.StartDate.Day
            else Date.firstDay
        }
        EndDate = {
          Season = span.EndSeason
          Day =
            if span.EndSeason = settings.Game.EndDate.Season
            then settings.Game.EndDate.Day
            else Date.lastDay
        }
        Fertilizer = fertilizer
        CropHarvests = cropHarvests
        RegrowCrop = regrowCrop
      }

      div [
        Class.calendar
        children (solver settings true span)
      ]


module SummaryTable =
  open Fable.Core.JsInterop

  let private toPrecision (value: float) =
    let rounded = floatRound2 value
    if rounded = 0.0
    then value?toPrecision 3
    else floatFormat2 rounded

  let private rowEndWithContent
    rowSpan
    colSpan
    (itemCell: ReactElement)
    (price: ReactElement)
    (quantity: ReactElement)
    (profit: ReactElement)
    (seeds: ReactElement)
    =
    let rowSpan = prop.rowSpan rowSpan
    fragment [
      td [
        rowSpan
        prop.colSpan colSpan
        children itemCell
      ]
      td [ rowSpan; children price ]
      if quantity = none then
        td [ rowSpan; ]
        td [ rowSpan; ]
      else
        td [ rowSpan; text "x" ]
        td [ rowSpan; children quantity ]
      td [ rowSpan; children profit ]
      td [ rowSpan; children seeds ]
    ]

  let private rowEnd rowSpan colSpan itemCell price quantity profit seeds =
    let profit = floatRound2 profit
    let noProfit = Option.isNone price && profit = 0.0
    let seeds = floatRound2 seeds
    let noSeeds = seeds = 0.0

    if noProfit && noSeeds then None else

    let price = price |> Option.defaultOrMap none (gold >> ofStr)
    let quantity = if quantity = 0.0 then none else ofStr (toPrecision quantity)
    let profit = if noProfit then none else ofStr (goldFormat2 profit)
    let seeds = if noSeeds then none else ofStr (floatFormat2 seeds)

    Some (rowEndWithContent rowSpan colSpan itemCell price quantity profit seeds)

  let unknownRowEnd rowSpan colSpan required quantity seeds itemCell =
    let quantity = if quantity = 0.0 then none else ofStr (toPrecision quantity)
    let seeds = if seeds = 0.0 then none else ofStr (floatFormat2 seeds)
    rowEndWithContent rowSpan colSpan itemCell none quantity (if required then ofStr "???" else none) seeds

  let singleRow price quantity profit seeds itemCell =
    rowEnd 1 5 itemCell price quantity profit seeds |> Option.defaultOrMap none (fun rowEnd -> tr [ rowEnd ])

  let inputItemAmountRowsWithEnd inputItemAmounts data (rowEnd: _ -> _ -> ReactElement option) =
    let inputRows = inputItemAmounts |> Array.choose (fun (item, quantity) ->
      let quantity = floatRound2 quantity
      if quantity = 0.0
      then None
      else Some (item, quantity))

    rowEnd inputRows.Length 1 |> Option.defaultOrMap none (fun rowEnd ->
      let rowSpan = rowSpan 1
      fragment (inputRows |> Array.mapi (fun i ((item, quality), amount) ->
        tr [
          td [ rowSpan; children (Image.Icon.itemQuality' data item quality) ]
          td [ rowSpan; text "x" ]
          td [ rowSpan; text (floatFormat2 amount) ]
          td [ rowSpan; children Image.rightArrow ]
          if i = 0 then rowEnd
        ])))

  let inputItemAmountRows inputItemAmounts data price quantity profit seeds itemCell =
    inputItemAmountRowsWithEnd inputItemAmounts data (fun row col -> rowEnd row col itemCell price quantity profit seeds)

  let private harvestsSummaryHeaderRow data settings crop (harvests: nat) =
    let seeds =
      if settings.Profit.SeedStrategy = IgnoreSeeds then 0u
      elif Crop.regrows crop then 1u
      else harvests

    singleRow None 0.0 0.0 (-float seeds) (fragment [
      Image.Icon.crop data crop
      let harvestStr = pluralize (float harvests) "harvest"
      ofStr $" ({harvests} {harvestStr})"
    ])

  let private stockpiledSeedRow settings =
    if settings.Profit.SeedStrategy <> StockpileSeeds then none else
    singleRow None 0.0 0.0 1.0 (ofStr "Stockpiled Seed")

  let private boughtRow priceAndVendor amount seeds (icon: ReactElement) =
    match priceAndVendor with
    | Some (vendor, price) ->
      singleRow (Some price) amount (-float price * amount) seeds (fragment [
        icon
        match vendor with
        | NonCustom vendor ->
          ofStr " from "
          Image.Icon.vendor vendor
        | Custom () -> ofStr " (Custom)"
      ])
    | None ->
      if amount = 0.0 then none else
      let seeds = floatRound2 seeds
      tr [
        unknownRowEnd 1 5 true seeds seeds (fragment [
          icon
          ofStr " from ???"
        ])
      ]

  let private fertilizerBoughtRow replacement fertilizer price amount =
    match fertilizer, price with
    | Some fertilizer, Some price ->
      boughtRow price amount 0.0 (fragment [
          if replacement then ofStr "Replacement "
          Image.Icon.fertilizer fertilizer
      ])
    | _ -> none

  let private seedsBoughtRow data (seed: SeedId) seedPrice amount =
    boughtRow seedPrice amount amount (Image.Icon.item' data (seed * 1u<_>))

  let private harvestedSeedsRows data item (amounts: _ Qualities) =
    amounts
    |> Qualities.indexed
    |> Array.map (fun (quality, amount) ->
      singleRow None amount 0.0 amount (Image.Icon.itemQuality' data item quality))
    |> fragment

  let private seedMakerRows data crop item (amounts: _ Qualities) =
    let inputs =
      amounts
      |> Qualities.indexed
      |> Array.map (fun (quality, amount) -> (item, quality), amount)

    let seed = Crop.seed crop
    let seedAmount = Qualities.sum amounts * Processor.seedMakerExpectedAmount seed
    inputItemAmountRows inputs data None seedAmount 0.0 seedAmount (Image.Icon.item' data (seed * 1u<_>))

  let private forageSeedsItemAmounts items amount =
    let itemAmount = amount / float ForageCrop.forageSeedsPerCraft
    items |> Array.map (fun item -> (item, Quality.Normal), itemAmount)

  let private forageSeedsRows data settings items (seed: SeedId) amountSold amountUsed =
    if amountSold = 0.0 && amountUsed = 0.0 then none else
    let itemCell = Image.Icon.item' data (seed * 1u<_>)
    let price = Game.seedItemSellPrice data settings.Game seed
    fragment [
      inputItemAmountRows (forageSeedsItemAmounts items amountUsed) data None amountUsed 0.0 amountUsed itemCell
      inputItemAmountRows (forageSeedsItemAmounts items amountSold) data (Some price) amountSold (float price * amountSold) 0.0 itemCell
    ]

  let private seedAmountsRows data crop seedAmounts = fragment (seedAmounts |> Array.map (fun (item, amounts: _ Qualities) ->
    if item = Crop.seedItem crop then harvestedSeedsRows data item amounts
    elif item = Crop.mainItem crop then seedMakerRows data crop item amounts
    else assert false; none))

  let private soldItemRow data (summary: Query.SoldItemSummary) =
    singleRow (Some summary.Price) summary.Quantity (float summary.Price * summary.Quantity) 0.0 (fragment [
      Image.Icon.itemQuality' data summary.Item summary.Quality
      if summary.Custom then ofStr " (Custom)"
    ])

  let private soldProductRow data (summary: Query.SoldProductSummary) =
    inputItemAmountRows
      summary.InputItemQuantities
      data
      (Some summary.Price)
      summary.Quantity
      (float summary.Price * summary.Quantity)
      0.0
      (Image.Icon.productQuality data summary.Product summary.Quality)

  let private unsoldItemRow data (item, quantities: float Qualities) =
    let inputs =
      quantities
      |> Qualities.indexed
      |> Array.map (fun (quality, amount) -> (item, quality), amount)

    inputItemAmountRowsWithEnd inputs data (fun row col -> Some (unknownRowEnd row col false 0.0 0.0 (ofStr "???")))

  let private harvestsSummaryFooterRow settings profit =
    tr [
      td [ colSpan 8; text "Total" ]
      td [
        match profit with
        | Some profit -> ofStr <| goldFormat2 profit
        | None -> ofStr "???"
      ]
      td [
        match settings.Profit.SeedStrategy with
        | IgnoreSeeds -> none
        | StockpileSeeds -> ofStr <| floatFormat2 1.0
        | BuyFirstSeed -> ofStr <| floatFormat2 0.0
      ]
    ]

  let private harvestSummaryTable data settings fertilizer fertilizerPrice (summary: Query.CropProfitSummary) =
    let crop = summary.Crop
    let seed = Crop.seed crop
    let items = Crop.items crop
    tbody [
      harvestsSummaryHeaderRow data settings crop summary.Harvests
      stockpiledSeedRow settings
      seedsBoughtRow data seed summary.SeedPrice summary.SeedsBought
      seedAmountsRows data crop summary.SeedAmounts
      forageSeedsRows data settings items seed summary.ForageSeedsSold summary.ForageSeedsUsed
      fragment (summary.UnsoldItems |> Array.map (unsoldItemRow data))
      fragment (summary.SoldItems |> Array.map (soldItemRow data))
      fragment (summary.SoldProducts |> Array.map (soldProductRow data))
      fertilizerBoughtRow true fertilizer fertilizerPrice summary.ReplacedFertilizer
      harvestsSummaryFooterRow settings summary.NetProfit
    ]

  let private normalizationFooter colSpan roi timeNorm (profitSummary: Query.ProfitSummary) =
    if roi || profitSummary.TimeNormalization = 1.0 then none else
    let colSpan = prop.colSpan colSpan
    tfoot [
      tr [
        td [ colSpan ]
        td [
          match timeNorm with
          | TotalPeriod -> assert false; none
          | PerDay -> ofStr $"/ {profitSummary.TimeNormalization} days"
          | PerSeason ->
            let value = floatRound2 profitSummary.TimeNormalization
            let seasons = pluralize value "season"
            ofStr $"/ {value} {seasons}"
        ]
      ]
      tr [
        td [ colSpan; text $"Total {timeNorm}" ]
        td [
          match profitSummary.NetProfit with
          | Some profit -> (profit / profitSummary.TimeNormalization) |> goldFormat2 |> ofStr
          | None -> ofStr "??? "
        ]
      ]
    ]

  let rankerRoi settings timeNorm (profitSummary: Query.ProfitSummary) =
    let investment = profitSummary.Investment (settings.Profit.SeedStrategy = BuyFirstSeed)
    let roi = investment |> Option.bind profitSummary.ROI
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
            ofFloat profitSummary.TimeNormalization
            ofStr " days"
          | PerSeason ->
            let value = floatRound2 profitSummary.TimeNormalization
            ofFloat value
            ofStr " "
            ofStr (pluralize value "season")
          | TotalPeriod -> ofInt 1
        ]
        div [
          floatFormat2 (roi / profitSummary.TimeNormalization) |> ofStr
          match timeNorm with
          | TotalPeriod -> none
          | PerDay -> ofStr "%/day"
          | PerSeason -> ofStr "%/season"
        ]
      | _ -> none
    ]

  let rankerProfit roi (data: GameData) settings timeNorm fertilizer crop =
    if not <| Game.cropIsInSeason settings.Game crop then ofStr "Crop not in season!" else

    match Query.Ranker.profitSummary data settings timeNorm fertilizer crop with
    | None -> ofStr "No harvests possible!"
    | Some profitSummary ->
      div [ Class.breakdownTable; children [
        table [
          thead [
            tr [
              th [ colSpan 5; text "Item" ]
              th "Price"
              th "x"
              th "Quantity"
              th "Profit"
              th [ if settings.Profit.SeedStrategy <> IgnoreSeeds then ofStr "Seeds" ]
            ]
          ]
          tbody [ fertilizerBoughtRow false fertilizer profitSummary.FertilizerPrice 1.0 ]
          harvestSummaryTable data settings fertilizer profitSummary.FertilizerPrice profitSummary.CropSummaries[0]
          normalizationFooter 8 roi timeNorm profitSummary
        ]

        if roi then
          rankerRoi settings timeNorm profitSummary
      ]]

  // TODO
  let rankerXp (data: GameData) settings timeNorm fertilizer crop =
    match Query.Ranker.xpSummary data settings timeNorm fertilizer crop with
    | Error e ->
      div [
        if e.HasFlag Query.InvalidReasons.NotEnoughDays then
          ofStr (if not <| Game.cropIsInSeason settings.Game crop then "Crop not in season!" else "No harvests possible!")
        if e.HasFlag Query.InvalidReasons.NoFertilizerPrice then
          ofStr "No Fertilizer Price!"
        if e.HasFlag Query.InvalidReasons.NotEnoughSeeds then
          ofStr "No Seed Source!"
      ]
    | Ok xpSummary ->
      let summary = xpSummary.CropSummaries[0]
      div [
        div [
          ofStr (xp summary.XpPerItem)
          ofStr " x "
          ofStr (floatFormat2 summary.ItemQuantity)
          ofStr " = "
          ofStr (xpFormat2 xpSummary.Xp)
        ]
        if timeNorm <> TotalPeriod then
          let unit =
            match timeNorm with
            | PerDay -> "day"
            | PerSeason -> "season"
            | TotalPeriod -> assert false; ""
          div [
            let value = floatRound2 xpSummary.TimeNormalization
            ofStr $"{xpFormat2 xpSummary.Xp} / {value} {pluralize value unit} = {xpFormat2 (xpSummary.Xp / xpSummary.TimeNormalization)}/{unit}"
          ]
      ]

  let solverProfit data settings total (solutions: Solver.FertilizerDateSpan array) =
    div [ Class.breakdownTable; children [
      table [
        thead [
          tr [
            th [ colSpan 5 ]
            th "Price"
            th "x"
            th "Quantity"
            th "Profit"
            th [ if settings.Profit.SeedStrategy <> IgnoreSeeds then ofStr "Seeds" ]
          ]
        ]

        fragment (solutions |> Array.map (fun solution ->
          // TODO
          none
        ))

        if solutions |> Array.sumBy (fun summary -> Array.length summary.CropHarvests + if summary.RegrowCrop.IsSome then 1 else 0) <> 1 then
          tfoot [
            tr [
              td [ colSpan 8; text "Total" ]
              td (goldFormat2 total)
              td []
            ]
          ]
      ]
    ]]

  // TODO
  let solverXp data settings total (solutions: Solver.FertilizerDateSpan array) =
    table [
      thead [
        tr [
          th [ colSpan 2; text "Crop" ]
          th "x"
          th "Harvests"
          th "XP"
        ]
      ]
      tbody (solutions |> Array.map (fun solution ->
        solution.CropHarvests
        |> Array.map (fun (crop, harvests, _) ->
          let xpPerHarvest = Game.xpPerHarvest data settings.Game crop
          tr [
            td (Image.Icon.crop data crop)
            td (sprintf "%.2fxp" xpPerHarvest)
            td "x"
            td (ofNat harvests)
            td (sprintf "%.2fxp" (xpPerHarvest * float harvests))
          ]))
        |> Array.concat)
      tfoot [
        tr [
          td [ colSpan 4; text "Total" ]
          td (sprintf "%.2fxp" total)
        ]
      ]
    ]

  let private tooltipProfitRow bought (icon: ReactElement) price amount =
    match price with
    | Some price ->
      let profit = floatRound2 (float price * amount * if bought then -1.0 else 1.0)
      if profit = 0.0 then none else
      tr [
        td icon
        td (gold price)
        td "x"
        td (toPrecision amount)
        td (goldFormat2 profit)
      ]
    | None ->
      if amount = 0.0 then none else
      tr [
        td icon
        td (ofStr "???")
        td "x"
        td (toPrecision amount)
        td (ofStr "???")
      ]

  let inline private tooltipBoughtRow icon price amount = tooltipProfitRow true icon price amount
  let inline private tooltipSoldRow icon price amount = tooltipProfitRow false icon (Some price) amount

  let tooltipProfit data settings timeNorm roi (profitSummary: Query.ProfitSummary) =
    let summary = profitSummary.CropSummaries[0]
    table [
      thead [
        tr [
          th "Item"
          th "Price"
          th "x"
          th "Quantity"
          th "Profit"
        ]
      ]
      tbody [
        profitSummary.FertilizerPrice
        |> Option.defaultOrMap none (fun price ->
          tooltipBoughtRow
            (profitSummary.Fertilizer |> Option.defaultOrMap none Image.Icon.fertilizer)
            (Option.map snd price)
            (1.0 + summary.ReplacedFertilizer))

        tooltipBoughtRow
          (Image.Icon.item' data <| Crop.seedItem summary.Crop)
          (Option.map snd summary.SeedPrice)
          summary.SeedsBought

        tooltipSoldRow
          (Image.Icon.item' data <| Crop.seedItem summary.Crop)
          (Game.seedItemSellPrice data settings.Game (Crop.seed summary.Crop))
          summary.ForageSeedsSold

        fragment (summary.SoldItems |> Array.map (fun summary ->
          tooltipSoldRow
            (Image.Icon.itemQuality' data summary.Item summary.Quality)
            summary.Price
            summary.Quantity))

        fragment (summary.SoldProducts |> Array.map (fun summary ->
          tooltipSoldRow
            (Image.Icon.productQuality data summary.Product summary.Quality)
            summary.Price
            summary.Quantity))

        tr [
          td [ colSpan 4; text "Total" ]
          td (profitSummary.NetProfit |> Option.defaultOrMap "???" goldFormat2)
        ]
      ]
      normalizationFooter 4 roi timeNorm profitSummary
    ]


let private pairData metric timeNorm data settings =
  let crops = Query.Selected.inSeasonCrops data settings |> Array.ofSeq
  let fertilizers = Query.Selected.fertilizersOpt data settings |> Array.ofSeq
  let rankValue = Query.rankValue metric

  let data = crops |> Array.collect (fun crop ->
    let profit = rankValue data settings timeNorm crop
    fertilizers |> Array.map (fun fert ->
      (Crop.seed crop, Fertilizer.Opt.name fert), profit fert))

  {|
    Crops = crops
    Fertilizers = fertilizers
    Pairs = data
  |}


module Ranker =
  open Fable.Core.JsInterop
  open Feliz.Recharts

  let private selectFromGraph rankItem (pairs: _ array) dispatch i =
    let crop, fert = pairs[i]
    (match rankItem with
    | RankCropsAndFertilizers -> (Some crop, Some fert)
    | RankCrops -> (Some crop, None)
    | RankFertilizers -> (None, Some fert))
    |> Some
    |> SetSelectedCropAndFertilizer
    |> dispatch

  let private pairImage (data: GameData) (pairs: (SeedId * string option) array) selectPair (props: IXAxisTickProperties) =
    let index: int = props?payload?value
    let crop, fert = pairs[index]
    Svg.svg [
      svg.className "pairSelect"
      svg.onClick (fun _ -> selectPair index)
      svg.x (props.x - 10.0)
      svg.y props.y
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
        | None -> none
      ]
    ]

  let chartProfitTooltip data settings timeNorm roi fertilizer crop (profit: Result<float, Query.InvalidReasons>) =
    div [
      match Query.Ranker.profitSummary data settings timeNorm fertilizer crop with
      | Some profitSummary ->
        let summary = profitSummary.CropSummaries[0]
        let harvestDesc = pluralize (float summary.Harvests) "harvest"
        div [
          Image.Icon.crop data crop
          match fertilizer with
          | None -> none
          | Some fert ->
            ofStr " with "
            Image.Icon.fertilizer fert
          ofStr $" ({summary.Harvests} {harvestDesc})"
        ]
        match profit with
        | Ok profit ->
          assert (profitSummary.NetProfit |> Option.exists (fun x -> abs (x / profitSummary.TimeNormalization - profit) < 1e-5))
          SummaryTable.tooltipProfit data settings timeNorm roi profitSummary
        | Error e ->
            if e.HasFlag Query.InvalidReasons.NoFertilizerPrice then
              ofStr "No fertilizer price!"
            if e.HasFlag Query.InvalidReasons.NotEnoughSeeds then
              ofStr "No seed source!"
            if e.HasFlag Query.InvalidReasons.NoInvestment then
              ofStr "No investment!"
      | None ->
        div [
          Image.Icon.crop data crop
          match fertilizer with
          | None -> none
          | Some fert ->
            ofStr " with "
            Image.Icon.fertilizer fert
        ]
        match profit with
        | Error e ->
          if e.HasFlag Query.InvalidReasons.NotEnoughDays then
            ofStr "Not enough days!"
          if e.HasFlag Query.InvalidReasons.NoFertilizerPrice then
            ofStr "No fertilizer price!"
          if e.HasFlag Query.InvalidReasons.NotEnoughSeeds then
            ofStr "No seed source!"
          if e.HasFlag Query.InvalidReasons.NoInvestment then
            ofStr "No investment!"
        | Ok _ -> assert false; none
      ]

  let private chartTooltip (data: GameData) settings timeNorm rankItem (pairs: (SeedId * string option) array) props =
    match props?payload with
    | Some (payload: _ array) when payload.Length > 0 && props?active ->
      let (index: int, result: Result<float, Query.InvalidReasons>) = payload[0]?payload
      let crop, fertilizer = pairs[index]
      let crop = data.Crops[crop]
      let fertilizer = Option.map data.Fertilizers.Find fertilizer
      match rankItem with
      | Gold | ROI -> chartProfitTooltip data settings timeNorm (rankItem = ROI) fertilizer crop result
      | XP -> none // TODO
    | _ -> none

  let private svgRectPath x y width height =
    $"M {x},{y} h {width} v {height} h -{width} Z"

  let private barBackground gap selectPair props =
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
      svg.d (svgRectPath x y width height)
    ]

  let private errorBar (pairs: (SeedId * string option) array) props =
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
        svg.d (svgRectPath x y width height)
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

  let private graph ranker data settings pairs (graphData: _ array) dispatch =
    let barGap = 4.0
    let selectPair = selectFromGraph ranker.RankItem pairs dispatch
    Recharts.responsiveContainer [
      responsiveContainer.debounce 200
      responsiveContainer.width (length.percent 100)
      responsiveContainer.chart (Recharts.barChart [
        barChart.data graphData
        barChart.barSize 40
        barChart.barGap barGap
        barChart.children [
          Recharts.yAxis [
            yAxis.unit (RankMetric.unit ranker.RankMetric)
            yAxis.width 60
          ]
          Recharts.tooltip [
            tooltip.content (chartTooltip data settings ranker.TimeNormalization ranker.RankMetric pairs)
          ]
          Recharts.bar [
            bar.dataKey (snd >> (function Ok y -> y | Error _ -> 0.0))
            bar.fill "blue"
            bar.onClick (fun props -> fst props?payload |> selectPair)
            Interop.mkBarAttr "background" (barBackground barGap selectPair)
            Interop.mkBarAttr "shape" (errorBar pairs)
          ]
          Recharts.brush [
            brush.startIndex (ranker.BrushSpan |> fst |> int |> min (graphData.Length - 1) |> max 0)
            brush.endIndex (ranker.BrushSpan |> snd |> int |> min (graphData.Length - 1) |> max 0)
            brush.height 30
            Interop.mkBrushAttr "onChange" (fun i -> dispatch <| SetBrushSpan (i?startIndex, i?endIndex))
          ]
          Recharts.xAxis [
            xAxis.dataKey (fst: _ -> int)
            xAxis.tick (pairImage data pairs selectPair)
            xAxis.interval 0
            xAxis.height 50
          ]
        ]
      ])
    ]

  let rankBy labelText ranker dispatch =
    fragment [
      ofStr labelText
      Select.options (length.rem 4) (fun metric ->
        div [
          prop.text (string metric)
          title (RankMetric.fullName metric)
        ])
        unitUnionCases<RankMetric>
        ranker.RankMetric
        (SetRankMetric >> dispatch)
      Select.unitUnion (length.rem 7) ranker.TimeNormalization (SetTimeNormalization >> dispatch)
    ]

  let ranker ranker (data, settings) dispatch =
    let pairData = pairData ranker.RankMetric ranker.TimeNormalization data settings
    if Array.isEmpty pairData.Pairs then
      div [
        if Array.isEmpty pairData.Crops then ofStr "No Crops Selected"
        if Array.isEmpty pairData.Fertilizers then ofStr "No Fertilizers Selected"
      ]
    else
      let pairs =
        match ranker.RankItem with
        | RankCropsAndFertilizers -> pairData.Pairs
        | RankCrops ->
          pairData.Pairs
          |> Array.groupBy (fst >> fst)
          |> Array.map (snd >> Array.maxBy (snd >> Option.ofResult))
        | RankFertilizers ->
          pairData.Pairs
          |> Array.groupBy (fst >> snd)
          |> Array.map (snd >> Array.maxBy (snd >> Option.ofResult))

      let pairs =
        if ranker.ShowInvalid then pairs else
        pairs |> Array.filter (snd >> Result.isOk)

      if Array.isEmpty pairs then
        div "No valid items found"
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
          ]]
          div [
            Class.graph
            children [
              Elmish.React.Common.lazyView3With
                (fun (data1, _) (data2, _) -> data1 = data2)
                (fun (pairs, pairData) (ranker, data, settings) -> graph ranker data settings pairs pairData)
                (pairs, pairData)
                (ranker, data, settings)
                dispatch
            ]
          ]
        ]


let private selectSpecificOrBest name toString (viewItem: _ -> ReactElement) items selected dispatch =
  Select.search
    (length.rem 15)
    (function
      | Choice1Of2 item
      | Choice2Of2 (Some item) -> toString item
      | Choice2Of2 None -> "???")
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

let [<ReactComponent>] AuditCropAndFertilizer (props: {|
    App: _
    Seed: _
    Fertilizer: _
    Dispatch: _
  |}) =
  let app = props.App
  let seed = props.Seed
  let fertName = props.Fertilizer

  let appDispatch = props.Dispatch
  let dispatch = SetRanker >> props.Dispatch
  let data = app.Data
  let settings, ui = app.State
  let ranker = ui.Ranker

  let (metric, timeNorm), setState = useState ((ranker.RankMetric, ranker.TimeNormalization))

  let pairData = pairData metric timeNorm data settings

  let bestCrop, bestFert =
    let pairs = pairData.Pairs

    let bestCrop, bestFert =
      if Array.isEmpty pairs then None, None else
      let crop, fert = pairs |> Array.maxBy (snd >> Option.ofResult) |> fst
      Some data.Crops[crop], Some (Option.map data.Fertilizers.Find fert)

    let metricValue =
      match metric with
      | Gold -> Query.cropProfit
      | ROI -> Query.cropROI
      | XP -> Query.cropXP

    let bestCrop =
      match fertName with
      | Some fert' ->
        let fert = Option.map data.Fertilizers.Find fert'
        if Array.isEmpty pairData.Crops then None else
        pairData.Crops |> Array.maxBy (fun crop -> metricValue data settings timeNorm crop fert |> Option.ofResult) |> Some
      | None -> bestCrop

    let bestFert =
      match seed with
      | Some seed ->
        let crop = data.Crops[seed]
        if Array.isEmpty pairData.Fertilizers then None else
        let profit = metricValue data settings timeNorm crop
        pairData.Fertilizers |> Array.maxBy (profit >> Option.ofResult) |> Some
      | None -> bestFert

    Choice2Of2 bestCrop, Choice2Of2 bestFert

  let cropOptions =
    pairData.Crops
    |> Array.sortBy (Settings.Crops.sortKey data)
    |> Array.map Choice1Of2
    |> Array.append [| bestCrop |]

  let fertilizerOptions =
    // array.sort moves undefined elements to the end of the array even with a compare function
    // so we wrap with all elements with Some before sorting and then unwraping
    pairData.Fertilizers
    |> Array.map Some
    |> Array.sortBy (Option.get >> Fertilizer.Opt.name)
    |> Array.map (Option.get >> Choice1Of2)
    |> Array.append [| bestFert |]

  let crop = seed |> Option.defaultOrMap bestCrop (data.Crops.Find >> Choice1Of2)
  let fert = fertName |> Option.defaultOrMap bestFert (Option.map data.Fertilizers.Find >> Choice1Of2)

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
    ]]

    match crop, fert with
    | (Choice1Of2 crop | Choice2Of2 (Some crop)), (Choice1Of2 fert | Choice2Of2 (Some fert)) ->
      animatedDetails
        (ui.OpenDetails.Contains OpenDetails.RankerProfitBreakdown)
        (ofStr "Summary")
        (match metric with
          | Gold -> SummaryTable.rankerProfit false data settings timeNorm fert crop
          | XP -> SummaryTable.rankerXp data settings timeNorm fert crop
          | ROI -> SummaryTable.rankerProfit true data settings timeNorm fert crop)
        (curry SetDetailsOpen OpenDetails.RankerProfitBreakdown >> appDispatch)

      animatedDetails
        (ui.OpenDetails.Contains OpenDetails.RankerGrowthCalendar)
        (ofStr "Growth Calendar")
        (GrowthCalendar.ranker app fert crop)
        (curry SetDetailsOpen OpenDetails.RankerGrowthCalendar >> appDispatch)

    | _ ->
      div [
        if Array.isEmpty pairData.Fertilizers then ofStr "No fertilizers selected!"
        if Array.isEmpty pairData.Crops then ofStr "No crops selected!"
      ]
  ]]


let rankerOrAudit app dispatch =
  let settings, ui = app.State
  let ranker = ui.Ranker
  match ranker.SelectedCropAndFertilizer with
  | Some (crop, fert) -> AuditCropAndFertilizer {| App = app; Seed = crop; Fertilizer = fert; Dispatch = dispatch |}
  | None -> Elmish.React.Common.lazyView3 Ranker.ranker ranker (app.Data, settings) (SetRanker >> dispatch)


// The solver typically takes < 50ms, but with
//   StartSeason = Spring, StopSeason = Fall, Location = Greenhouse, and FarmingLevel in [0..7],
// it can take around 600ms if all fertilizers and crops are selected.
// For this reason, the solver is put in a web worker with a debouncer.

let private workerQueue, private workerSubscribe =
  let queue, subscribe = Solver.createWorker ()
  debouncer 200 (fun (data, settings, mode) -> queue data settings mode), subscribe

let [<ReactComponent>] Solver (props: {|
    Data: GameData
    Settings: Settings
    SolverMode: SolverMode
  |}) =
  let (solution, settings, solving), setState = useState ((None, props.Settings, false))

  workerSubscribe (fun solution -> setState (Some solution, props.Settings, false))

  useEffect ((fun () ->
    setState (solution, settings, true)
    workerQueue (props.Data, props.Settings, props.SolverMode)
  ), [| box props.Data; box props.Settings; props.SolverMode |])

  match solution with
  | Some (solution, total) ->
    div [
      if solving then className "disabled"
      children [
        match props.SolverMode with
        | MaximizeGold -> SummaryTable.solverProfit props.Data settings total solution
        | MaximizeXP -> SummaryTable.solverXp props.Data settings total solution
        div [
          Class.calendar
          children (solution |> Seq.collect (GrowthCalendar.solver settings false))
        ]
      ]
    ]
  | None -> div "Loading..."

let section app dispatch =
  let settings, ui = app.State
  let uiDispatch = SetUI >> dispatch

  section [
    prop.id (if ui.Mode = Ranker && ui.Ranker.SelectedCropAndFertilizer.IsNone then "visualization-graph" else "visualization")
    children [
      viewTabs ui.Mode (SetAppMode >> uiDispatch)
      match ui.Mode with
      | Ranker -> rankerOrAudit app uiDispatch
      | Solver ->
        labeled "Maximize: " <| Select.unitUnion (length.rem 5) ui.SolverMode (SetSolverMode >> uiDispatch)
        Solver {|
          Data = app.Data
          Settings = settings
          SolverMode = ui.SolverMode
        |}
    ]
  ]
