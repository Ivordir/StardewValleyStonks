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

  let private appendCropHarvests settings fertilizer (cropHarvests: _ array) remainingDays stageList =
    let rec loop i remainingDays stageList =
      if i < 0 then remainingDays, stageList else
      let crop, harvests = cropHarvests[i]
      let stages, time = Game.growthTimeAndStages settings.Game fertilizer crop
      let usedDays = Growth.daysNeededFor None time harvests
      let stageImages = stageImages stages (Crop.seed crop)

      let harvestItem = div (Image.item' <| Crop.mainItem crop)
      let stages = Array.append stageImages [| harvestItem |]
      loop (i - 1) (remainingDays - int usedDays) (repeatCons harvests stages stageList)
    loop (Array.length cropHarvests - 1) remainingDays stageList

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

    let rec loop season remainingDays stageList =
      let remainingDays, stageList = appendCropHarvests settings span.Fertilizer span.CropHarvests[season] remainingDays stageList

      if season = 0 then remainingDays, stageList else
      let season = season - 1
      let crop = span.BridgeCrops[season]
      let seed = Crop.seed crop
      let stages, time = Game.growthTimeAndStages settings.Game span.Fertilizer crop
      let stageImages = stageImages stages seed

      let harvestItem = div (Image.item' <| Crop.mainItem crop)
      let filler = int remainingDays - int time |> max 0
      let days = int remainingDays + int days[season] - int time - filler
      let stageList =
        stageImages
        :: Array.create filler (stageImage stages.Length seed)
        :: [| harvestItem |]
        :: stageList

      loop season days stageList

    let days, stageList = loop season (int remainingDays) stageList

    let firstStage =
      [|
        span.CropHarvests[0] |> Array.tryHead |> Option.map fst
        Array.tryHead span.BridgeCrops
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

  let ranker app crop fertilizer =
    let settings, _ = app.State
    match Query.bestGrowthSpan settings.Game fertilizer crop with
    | None -> ofStr (if Game.cropIsInSeason settings.Game crop then "No harvests possible!" else "Crop not in season!")
    | Some (span: Query.GrowthSpan) ->
      let cropHarvests, regrowCrop =
        match crop with
        | FarmCrop crop when crop.RegrowTime.IsSome -> [| |], Some (0, crop, span.Harvests)
        | _ -> [| crop, span.Harvests |], None

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
        CropHarvests = [| cropHarvests |]
        BridgeCrops = [||]
        RegrowCrop = regrowCrop
      }

      div [
        Class.calendar
        children (solver settings true span)
      ]


module SummaryTable =
  let private harvestsSummaryHeaderRow data settings crop (harvests: nat) =
    tr [
      td [ colSpan 5; children [
        Image.Icon.crop data crop
        let harvestStr = pluralize (float harvests) "harvest"
        ofStr $" ({harvests} {harvestStr})"
      ]]
      td []
      td []
      td []
      td []
      td [
        if settings.Profit.SeedStrategy <> IgnoreSeeds then
          ofStr <| float2Decimal (-float (if Crop.regrows crop then 1u else harvests))
      ]
    ]

  let private stockpiledSeedRow settings =
    if settings.Profit.SeedStrategy <> StockpileSeeds then none else
    tr [
      td [ colSpan 5; text "Stockpiled Seed" ]
      td []
      td []
      td []
      td []
      td (float2Decimal 1.0)
    ]

  let private boughtRow (icon: _ -> ReactElement) (seeds: _ -> ReactElement) item price amount =
    if amount = 0.0 then none else
    tr [
      td [ colSpan 5; children [
        icon item
        match price with
        | Some (NonCustom vendor, _) ->
          ofStr " from "
          Image.Icon.vendor vendor
        | Some (Custom (), _) -> ofStr " (Custom)"
        | None -> ofStr " from ???"
      ]]
      td [
        match price with
        | Some (_, cost) -> gold cost |> ofStr
        | None -> none
      ]
      td "x"
      td (float2Decimal amount)
      td [
        match price with
        | Some (_, cost) -> ofStr (gold2Decimal <| amount * -float cost)
        | None -> ofStr "???"
      ]
      td (seeds amount)
    ]

  let private fertilizerBoughtRow replacement fertilizer price amount =
    match fertilizer, price with
    | Some fertilizer, Some price ->
      boughtRow
        (fun fertilizer ->
          fragment [
            if replacement then ofStr "Replacement "
            Image.Icon.fertilizer fertilizer
          ])
        (fun _ -> none)
        fertilizer
        price
        amount
    | _ -> none

  let private seedsBoughtRow data (seed: SeedId) seedPrice amount =
    if amount = 0.0 then none else
    boughtRow
      (fun seed -> Image.Icon.item' data (seed * 1u<_>))
      (fun amount -> ofStr <| float2Decimal amount)
      seed
      seedPrice
      amount

  let private harvestedSeedsRows data item (amounts: _ Qualities) =
    fragment [
      for i = Quality.highest downto 0 do
        let quality = enum i
        let amount = amounts[quality]
        if amount = 0.0 then none else
        tr [
          td [
            colSpan 5
            children (Image.Icon.itemQuality' data item quality)
          ]
          td []
          td "x"
          td (float2Decimal amount)
          td []
          td (float2Decimal amount)
        ]
    ]

  let private seedMakerRows data crop item (amounts: _ Qualities) =
    let seed = Crop.seed crop
    fragment [
      for i = Quality.highest downto 0 do
        let quality = enum i
        let amount = amounts[quality]
        if amount = 0.0 then none else
        let seedAmount = amount * Processor.seedMakerExpectedAmount seed
        tr [
          td (Image.Icon.itemQuality' data item quality)
          td "x"
          td (float2Decimal amount)
          td Image.rightArrow
          td (Image.Icon.productQuality data (SeedsFromSeedMaker (seed * 1u<_>)) Quality.Normal)
          td []
          td "x"
          td (float2Decimal seedAmount)
          td []
          td (float2Decimal seedAmount)
        ]
    ]

  let private forageSeedsRows data settings items (seed: SeedId) amountSold amountUsed =
    let totalAmount = amountSold + amountUsed
    if totalAmount = 0.0 then none else
    fragment (items |> Array.mapi (fun i item ->
      tr [
        td (Image.Icon.item' data item)
        td "x"
        td (float2Decimal (totalAmount / float ForageCrop.forageSeedsPerCraft))
        td Image.rightArrow
        if i = 0 then
          let span = rowSpan items.Length
          td [ span; children (Image.Icon.item' data (seed * 1u<_>)) ]
          td [ span; children (ofStr <| gold (Game.seedItemSellPrice data settings.Game seed)) ]
          td [ span; children (ofStr "x") ]
          td [ span; children (ofStr <| float2Decimal totalAmount) ]
          td [ span; children [
            if amountSold > 0.0 then
              ofStr <| gold2Decimal (amountSold * float (Game.seedItemSellPrice data settings.Game seed))
          ]]
          td [ span; children [
            if amountUsed > 0.0 then
              ofStr <| float2Decimal amountUsed
          ]]
      ]
    ))

  let private seedAmountsRows data crop seedAmounts = fragment (seedAmounts |> Array.map (fun (item, amounts: _ Qualities) ->
    if item = Crop.seedItem crop then harvestedSeedsRows data item amounts
    elif item = Crop.mainItem crop then seedMakerRows data crop item amounts
    else assert false; none))

  let private rawItemRow data item quality profit amount =
    fragment [
      td [
        colSpan 5
        children (Image.Icon.itemQuality' data item quality)
      ]
      td (gold (nat profit))
      td "x"
      td (float2Decimal amount)
    ]

  let private productRow data settings item quality product profit amount =
    fragment [
      let amountPerItem = Product.amountPerItem product
      td (Image.Icon.itemQuality' data item quality)
      td "x"
      td (float2Decimal amount)
      td Image.rightArrow
      td (Image.Icon.productQuality data product (Product.outputQuality settings.Game.ModData quality product))
      td (gold (nat (profit / amountPerItem)))
      td "x"
      td (float2Decimal (amount * amountPerItem))
    ]

  let private customSellPriceRow data item quality custom amount =
    fragment [
      td [
        colSpan 5
        children [
          Image.Icon.itemQuality' data item quality
          ofStr " (Custom)"
        ]
      ]
      td (gold (Query.customSellPriceValue quality custom))
      td "x"
      td (float2Decimal amount)
    ]

  let private unknownSellAs data item quality amount =
    tr [
      td (Image.Icon.itemQuality' data item quality)
      td "x"
      td (float2Decimal amount)
      td Image.rightArrow
      td [ colSpan 4; text "???" ]
      td []
      td []
    ]

  let private soldAmountsRows data settings (items: _ array) (summary: Query.HarvestsSummary) =
    fragment [
      for i = 0 to items.Length - 1 do
        let item = items[i]
        let sellAs = summary.SellAs[i]
        let soldAmounts = summary.ItemAndSeedSummary.SoldAmounts[i]

        for i = Quality.highest downto 0 do
          let quality = enum i
          let amount = soldAmounts[quality]
          if amount = 0.0 then none else
          match sellAs with
          | Some sellAsData ->
            let product, profit = sellAsData[quality]
            tr [
              match product with
              | NonCustom None -> rawItemRow data item quality profit amount
              | NonCustom (Some product) -> productRow data settings item quality product profit amount
              | Custom (price, preservesQuality) -> customSellPriceRow data item quality (price, preservesQuality) amount

              td (gold2Decimal (profit * amount))
              td []
            ]
          | None -> unknownSellAs data item quality amount
      ]

  let private harvestsSummaryFooterRow settings profit =
    tr [
      td [ colSpan 8; text "Total" ]
      td [
        match profit with
        | Some profit -> ofStr <| gold2Decimal profit
        | None -> ofStr "???"
      ]
      td [
        match settings.Profit.SeedStrategy with
        | IgnoreSeeds -> none
        | StockpileSeeds -> ofStr <| float2Decimal 1.0
        | BuyFirstSeed -> ofStr <| float2Decimal 0.0
      ]
    ]

  let private harvestSummaryTable data settings fertilizer fertilizerPrice profit (summary: Query.HarvestsSummary) =
    let itemSummary = summary.ItemAndSeedSummary
    let crop = summary.Crop
    let seed = Crop.seed crop
    let items = Crop.items crop
    tbody [
      harvestsSummaryHeaderRow data settings crop summary.Harvests
      stockpiledSeedRow settings
      seedsBoughtRow data seed summary.SeedPrice itemSummary.SeedsBought
      seedAmountsRows data crop itemSummary.SeedAmounts
      forageSeedsRows data settings items seed itemSummary.ForageSeedsSold itemSummary.ForageSeedsUsed
      soldAmountsRows data settings items summary
      fertilizerBoughtRow true fertilizer fertilizerPrice summary.ReplacedFertilizer
      harvestsSummaryFooterRow settings profit
    ]

  let private rankerProfitFooter roi timeNorm (profitSummary: Query.Ranker.ProfitSummary) =
    tfoot [
      if not roi && timeNorm <> TotalPeriod then
        tr [
          td [ colSpan 8 ]
          td [
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
            | TotalPeriod ->
              assert false
              ofFloat profitSummary.TimeNormalization
          ]
          td []
        ]
        tr [
          td [ colSpan 8; text $"Total {timeNorm}" ]
          td [
            match profitSummary.NetProfit with
            | Some profit ->
              float2Decimal (profit / profitSummary.TimeNormalization) |> ofStr
              match timeNorm with
              | TotalPeriod -> none
              | PerDay -> ofStr "g/day"
              | PerSeason -> ofStr "g/season"
            | None -> ofStr "??? "
          ]
          td []
        ]
    ]

  let rankerRoi settings timeNorm (profitSummary: Query.Ranker.ProfitSummary) =
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
          float2Decimal (roi / profitSummary.TimeNormalization) |> ofStr
          match timeNorm with
          | TotalPeriod -> none
          | PerDay -> ofStr "%/day"
          | PerSeason -> ofStr "%/season"
        ]
      | _ -> none
    ]

  let rankerProfit roi (data: GameData) settings timeNorm crop fertilizer =
    if not <| Game.cropIsInSeason settings.Game crop then ofStr "Crop not in season!" else

    match Query.Ranker.profitSummary data settings timeNorm crop fertilizer with
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
          harvestSummaryTable data settings fertilizer profitSummary.FertilizerPrice profitSummary.NetProfit profitSummary.HarvestSummary
          rankerProfitFooter roi timeNorm profitSummary
        ]

        if roi then
          rankerRoi settings timeNorm profitSummary
      ]]

  let rankerXp (data: GameData) settings timeNorm crop fertilizer =
    match Query.Ranker.xpSummary data settings timeNorm crop fertilizer with
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
            | TotalPeriod -> assert false; ""
          div [
            ofStr (sprintf "%.2fxp" total)
            ofStr " / "
            let value = floatRound2 data.TimeNormalization
            ofFloat value
            ofStr " "
            ofStr (pluralize value unit)
            ofStr " = "
            ofStr (sprintf "%.2fxp/%s" (total / data.TimeNormalization) unit)
          ]
      ]

  let private cropFertTimeline (solution: Solver.FertilizerDateSpan) =
    Array.concat [|
      solution.CropHarvests[0]
      solution.CropHarvests
      |> Array.tail
      |> Array.zip solution.BridgeCrops
      |> Array.collect (fun (bridgeCrop, cropHarvests) ->
        cropHarvests |> Array.append [| bridgeCrop, 1u |])
      solution.RegrowCrop |> Option.map (fun (_, crop, harvests) -> FarmCrop crop, harvests) |> Option.toArray
    |]

  let solverProfit data settings total (solutions: Solver.FertilizerDateSpan list) =
    div [ Class.breakdownTable; children [
      table [
        thead [
          tr [
            th [ colSpan 6 ]
            th "Profit"
            th [ if settings.Profit.SeedStrategy <> IgnoreSeeds then ofStr "Seeds" ]
          ]
        ]

        let len = List.length solutions
        yield!
          solutions
          |> Seq.mapi (fun i solution ->
            let fertilizer = solution.Fertilizer
            let fertilizerPrice =
              match fertilizer with
              | None -> None
              | Some _ when not settings.Profit.PayForFertilizer -> None
              | Some fertilizer ->
                fertilizer
                |> Fertilizer.name
                |> Query.Price.fertilizerMinVendorAndPrice data settings
                |> Some

            [
              tbody [ fertilizerBoughtRow false fertilizer fertilizerPrice 1.0 ]
              yield! cropFertTimeline solution |> Array.map (fun (crop, harvests) ->
                let harvestsSummary = Query.Solver.profitAndHarvestsSummary data settings (i = len - 1) crop fertilizer harvests
                harvestSummaryTable data settings fertilizer fertilizerPrice harvestsSummary.NetProfit harvestsSummary)
            ])
          |> Seq.concat

        if len > 1 then
          tfoot [
            tr [
              td [ colSpan 6; text "Total" ]
              td (gold2Decimal total)
              td []
            ]
          ]
      ]
    ]]

  let solverXp data settings total (solutions: Solver.FertilizerDateSpan list) =
    table [
      thead [
        tr [
          th [ colSpan 2; text "Crop" ]
          th "x"
          th "Harvests"
          th "XP"
        ]
      ]
      tbody (solutions |> Seq.map (fun solution ->
        cropFertTimeline solution
        |> Array.map (fun (crop, harvests) ->
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

  let private tooltipProfitRow (icon: ReactElement) amount profit =
    if amount = 0.0 then none else
    tr [
      td icon
      td "x"
      td (float2Decimal amount)
      td (gold2Decimal (amount * profit))
    ]

  let inline private tooltipBoughtRow icon amount (price: nat) = tooltipProfitRow icon amount (-float price)
  let inline private tooltipSoldRow icon amount (price: nat) = tooltipProfitRow icon amount (float price)

  let tooltipProfit data settings timeNorm (profitSummary: Query.Ranker.ProfitSummary) =
    let crop = profitSummary.HarvestSummary.Crop
    table [
      tbody [
        match Option.flatten profitSummary.FertilizerPrice with
        | Some (_, price) ->
          tooltipBoughtRow (profitSummary.Fertilizer |> Option.defaultOrMap none Image.Icon.fertilizer) 1.0 price
        | _ -> none

        match profitSummary.HarvestSummary.SeedPrice with
        | Some (_, price) ->
          tooltipBoughtRow
            (Image.Icon.item' data <| Crop.seedItem crop)
            profitSummary.HarvestSummary.ItemAndSeedSummary.SeedsBought
            price
        | _ -> none

        tooltipSoldRow
          (Image.Icon.item' data <| Crop.seedItem crop)
          profitSummary.HarvestSummary.ItemAndSeedSummary.ForageSeedsSold
          (Game.seedItemSellPrice data settings.Game (Crop.seed crop))

        let items = Crop.items crop
        for i = 0 to items.Length - 1 do
          let item = items[i]
          let sellAs = profitSummary.HarvestSummary.SellAs[i]
          let soldAmounts = profitSummary.HarvestSummary.ItemAndSeedSummary.SoldAmounts[i]

          // group like products / sell as

          for i = Quality.highest downto 0 do
            let quality = enum i
            let amount = soldAmounts[quality]
            match sellAs with
            | Some sellAsData ->
              let product, profit = sellAsData[quality]
              let icon, amountPerItem =
                match product with
                | NonCustom None -> Image.Icon.itemQuality' data item quality, 1.0
                | NonCustom (Some product) -> Image.Icon.productQuality data product (Product.outputQuality settings.Game.ModData quality product), Product.amountPerItem product
                | Custom (_, preservesQuality) -> Image.Icon.itemQuality' data item (if preservesQuality then quality else Quality.Normal), 1.0

              let amount = amount * amountPerItem
              tooltipProfitRow icon amount profit
            | None -> none

        match Option.flatten profitSummary.FertilizerPrice with
        | Some (_, price) ->
          tooltipBoughtRow
            (profitSummary.Fertilizer |> Option.defaultOrMap none Image.Icon.fertilizer)
            profitSummary.HarvestSummary.ReplacedFertilizer
            price
        | _ -> none

        match profitSummary.NetProfit with
        | Some profit ->
          tr [
            td [ colSpan 3; text "Total" ]
            td (gold2Decimal profit)
          ]
        | None -> none

        if timeNorm <> TotalPeriod then
          tr [
            // days used / total season days
          ]

          tr [
            // normalized profit / roi
          ]
      ]
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

  let private chartTooltip (data: GameData) settings timeNorm (pairs: (SeedId * string option) array) props =
    match props?payload with
    | Some (payload: _ array) when payload.Length > 0 && props?active ->
      let (index: int, result: Result<float, Query.InvalidReasons>) = payload[0]?payload
      let crop, fertilizer = pairs[index]
      let crop = data.Crops[crop]
      let fertilizer = Option.map data.Fertilizers.Find fertilizer
      div [
        match Query.Ranker.profitSummary data settings timeNorm crop fertilizer with
        | Some profitSummary ->
          let harvestDesc = pluralize (float profitSummary.HarvestSummary.Harvests) "harvest"
          div [
            Image.Icon.crop data crop
            match fertilizer with
            | None -> none
            | Some fert ->
              ofStr " with "
              Image.Icon.fertilizer fert
            ofStr $" ({profitSummary.HarvestSummary.Harvests} {harvestDesc})"
          ]
          match result with
          | Ok profit ->
            assert (profitSummary.NetProfit |> Option.exists (fun x -> abs (x / profitSummary.TimeNormalization - profit) < 1e-6))
            SummaryTable.tooltipProfit data settings timeNorm profitSummary
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
          match result with
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
            tooltip.content (chartTooltip data settings ranker.TimeNormalization pairs)
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
          | Gold -> SummaryTable.rankerProfit false data settings timeNorm crop fert
          | XP -> SummaryTable.rankerXp data settings timeNorm crop fert
          | ROI -> SummaryTable.rankerProfit true data settings timeNorm crop fert)
        (curry SetDetailsOpen OpenDetails.RankerProfitBreakdown >> appDispatch)

      animatedDetails
        (ui.OpenDetails.Contains OpenDetails.RankerGrowthCalendar)
        (ofStr "Growth Calendar")
        (GrowthCalendar.ranker app crop fert)
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
