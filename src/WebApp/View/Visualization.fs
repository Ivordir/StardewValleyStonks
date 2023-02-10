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

let noHarvestsMessage settings crop =
  if Game.cropIsInSeason settings.Game crop
  then "No harvests possible!"
  else "Crop not in season!"
  |> ofStr

let invalidReasons settings crop (reasons: Query.InvalidReasons) =
  div [
    if reasons.HasFlag Query.InvalidReasons.NotEnoughDays then
      noHarvestsMessage settings crop
    if reasons.HasFlag Query.InvalidReasons.NoFertilizerPrice then
      ofStr "No fertilizer price!"
    if reasons.HasFlag Query.InvalidReasons.NotEnoughSeeds then
      ofStr "No seed source!"
    if reasons.HasFlag Query.InvalidReasons.NoInvestment then
      ofStr "No investment!"
  ]


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

  let private regrowCropCalendarDays settings (days: nat array) fertilizer stageList (season, crop, harvests) =
    let totalDays = Array.sum days[(season + 1)..]
    let stages, time = Game.growthTimeAndStages settings.Game fertilizer (FarmCrop crop)
    let usedDays = Growth.daysNeededFor crop.RegrowTime time harvests
    let stageImages = stageImages stages crop.Seed

    let harvestItem = div (Image.item' crop.Item)
    let firstHarvest = Array.append stageImages [| harvestItem |]
    let regrow = Array.create (int crop.RegrowTime.Value) (div (Image.regrowStage crop.Seed))
    regrow[regrow.Length - 1] <- harvestItem
    let filler = max 0 (int totalDays - int usedDays)
    let stageList = Array.create filler (div []) :: stageList
    let stageList = repeatCons (harvests - 1u) regrow stageList
    let stageList = firstHarvest :: stageList
    season, totalDays + days[season] - usedDays - nat filler, stageList

  let private fromDateSpan settings single (span: Solver.FertilizerDateSpan) =
    let stageList = [ Array.create (int (Date.daysInSeason - span.EndDate.Day)) (div [ Class.disabled ]) ]
    let days =
      if not single && settings.Game.Location = Farm
      then Date.daySpan span.StartDate span.EndDate
      else [| Date.totalDays span.StartDate span.EndDate |]

    let season, remainingDays, stageList =
      span.RegrowCrop |> Option.defaultOrMap
        (days.Length - 1, Array.last days, stageList)
        (regrowCropCalendarDays settings days span.Fertilizer stageList)

    let season, days, stageList =
      (span.CropHarvests, (season, int remainingDays, stageList)) ||> Array.foldBack (fun (crop, harvests, bridgeCrop) (season, remainingDays, stageList) ->
        let seed = Crop.seed crop
        let stages, time = Game.growthTimeAndStages settings.Game span.Fertilizer crop
        let stageImages = stageImages stages seed
        let harvestItem = [| div (Image.item' (Crop.mainItem crop)) |]
        if bridgeCrop then
          let filler = max 0 (int remainingDays - int time)
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

  let solver settings span = fromDateSpan settings false span

  let ranker app fertilizer crop =
    let settings, _ = app.State
    match Query.bestGrowthSpan settings.Game fertilizer crop with
    | None -> noHarvestsMessage settings crop
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
        children (fromDateSpan settings true span)
      ]

let private fertilizerAndCropHarvests data fertilizer crop (harvests: nat) =
  div [
    Image.Icon.crop data crop

    fertilizer |> ofOption (fun fertilizer ->
      fragment [
        ofStr " with "
        Image.Icon.fertilizer fertilizer
      ])

    if harvests = 0u then none else
      let unit = "harvest" |> pluralizeTo (float harvests)
      ofStr $" ({harvests} {unit})"
  ]

module SummaryTable =
  let private toPrecision (value: float) =
    let rounded = round2 value
    if rounded = 0.0
    then sprintf "%.2e" value
    else float2 rounded

  let private rowCells
    (inputsCell: ReactElement)
    (itemCell: ReactElement)
    (unitValue: ReactElement)
    (quantity: ReactElement)
    (value: ReactElement)
    (seeds: ReactElement)
    =
    [|
      if inputsCell = none then
        td [ colSpan 2; children itemCell ]
      else
        td inputsCell
        td itemCell
      td unitValue
      if quantity = none then
        td []
        td []
      else
        td "x"
        td quantity
      td value
      td seeds
    |]

  let private rowFromCells cells =
    tr [
      td []
      fragment cells
    ]

  let rowWithCells itemCell unitValue quantity value seeds =
    rowCells none itemCell unitValue quantity value seeds |> rowFromCells

  let private significantRow inputs itemCell (price: (nat * bool) option) quantity seeds =
    let profit = price |> Option.defaultOrMap 0.0 (fun (price, bought) ->
      round2 (float price * quantity * if bought then -1.0 else 1.0))
    let noProfit = profit = 0.0
    let seeds = round2 seeds
    let noSeeds = seeds = 0.0

    if noProfit && noSeeds then None else

    let price = price |> ofOption (fst >> gold >> ofStr)
    let quantity = if quantity = 0.0 then none else ofStr (toPrecision quantity)
    let profit = if noProfit then none else ofStr (gold2 profit)
    let seeds = if noSeeds then none else ofStr (float2 seeds)

    Some (rowCells inputs itemCell price quantity profit seeds)

  let unknownRow inputs itemCell required quantity seeds =
    let quantity = if quantity = 0.0 then none else ofStr (toPrecision quantity)
    let profit = if required then ofStr "???" else none
    let seeds = if seeds = 0.0 then none else ofStr (float2 seeds)
    rowCells inputs itemCell none quantity profit seeds

  let inputsCell data consumedItemQuantities =
    let inputRows = consumedItemQuantities |> Array.choose (fun (item, quantity) ->
      let quantity = round2 quantity
      if quantity = 0.0
      then None
      else Some (item, quantity))

    table [
      tbody (inputRows |> Array.map (fun ((item, quality), amount) ->
        tr [
          td (Image.Icon.itemQuality' data item quality)
          td "x"
          td (float2 amount)
          td Image.rightArrow
        ]
      ))
    ]

  let private harvestsSummaryHeaderRow data settings crop (harvests: nat) =
    let seeds =
      if settings.Profit.SeedStrategy = IgnoreSeeds then 0u
      elif Crop.regrows crop then 1u
      else harvests

    let itemCell = fertilizerAndCropHarvests data None crop harvests

    significantRow none itemCell None 0.0 (-float seeds) |> Option.toArray

  let private stockpiledSeedRow settings =
    if settings.Profit.SeedStrategy <> StockpileSeeds then Array.empty else
    significantRow none (ofStr "Stockpiled Seed") None 0.0 1.0 |> Option.toArray

  let private boughtRowEnd priceAndVendor amount seeds (icon: ReactElement) =
    match priceAndVendor with
    | Some (vendor, price) ->
      let itemCell = fragment [
        icon
        match vendor with
        | NonCustom vendor ->
          ofStr " from "
          Image.Icon.vendor vendor
        | Custom () -> ofStr " (Custom)"
      ]
      significantRow none itemCell (Some (price, true)) amount seeds
    | None ->
      if amount = 0.0 then None else
      let seeds = round2 seeds
      let itemCell = fragment [
        icon
        ofStr " from ???"
      ]

      Some (unknownRow none itemCell true seeds seeds)

  let private fertilizerBoughtRow replacement fertilizer price amount =
    match fertilizer, price with
    | Some fertilizer, Some price ->
      boughtRowEnd price amount 0.0 (fragment [
        if replacement then ofStr "Replacement "
        Image.Icon.fertilizer fertilizer
      ])
    | _ -> None

  let private seedsBoughtRow data (seed: SeedId) seedPrice amount =
    boughtRowEnd seedPrice amount amount (Image.Icon.seed data seed) |> Option.toArray

  let private harvestedSeedsRows data item (amounts: _ Qualities) =
    amounts
    |> Qualities.indexed
    |> Array.choose (fun (quality, amount) ->
      significantRow none (Image.Icon.itemQuality' data item quality) None amount amount)

  let private seedMakerRows data crop item (amounts: _ Qualities) =
    let inputsCell =
      amounts
      |> Qualities.indexed
      |> Array.map (fun (quality, amount) -> (item, quality), amount)
      |> inputsCell data

    let seed = Crop.seed crop
    let seedAmount = Qualities.sum amounts * Processor.seedMakerExpectedQuantity seed

    significantRow inputsCell (Image.Icon.seed data seed) None seedAmount seedAmount |> Option.toArray

  let private forageSeedsRows data settings items (seed: SeedId) amountSold amountUsed =
    if amountSold = 0.0 && amountUsed = 0.0 then Array.empty else
    let inputsCell amount =
      let itemAmount = amount / float ForageCrop.forageSeedsPerCraft
      items
      |> Array.map (fun item -> (item, Quality.Normal), itemAmount)
      |> inputsCell data

    let itemCell = Image.Icon.seed data seed
    let price = Game.seedItemSellPrice data settings.Game seed
    Array.choose id [|
      significantRow (inputsCell amountUsed) itemCell None amountUsed amountUsed
      significantRow (inputsCell amountSold) itemCell (Some (price, false)) amountSold 0.0
    |]

  let private seedAmountsRows data crop seedAmounts =
    seedAmounts |> Array.collect (fun (item, amounts: _ Qualities) ->
      if item = Crop.seedItem crop then harvestedSeedsRows data item amounts
      elif item = Crop.mainItem crop then seedMakerRows data crop item amounts
      else assert false; Array.empty)

  let private soldItemRow data (summary: Query.SoldItemSummary) =
    let itemCell = fragment [
      Image.Icon.itemQuality' data summary.Item summary.Quality
      if summary.Custom then ofStr " (Custom)"
    ]

    significantRow none itemCell (Some (summary.Price, false)) summary.Quantity 0.0

  let private soldProductRow data (summary: Query.SoldProductSummary) =
    significantRow
      (inputsCell data summary.ConsumedItemQuantities)
      (Image.Icon.productQuality data summary.Product summary.Quality)
      (Some (summary.Price, false))
      summary.Quantity
      0.0

  let private unsoldItemRow data (item, quantities: float Qualities) =
    let inputsCell =
      quantities
      |> Qualities.indexed
      |> Array.map (fun (quality, amount) -> (item, quality), amount)
      |> inputsCell data

    unknownRow inputsCell (ofStr "???") false 0.0 0.0

  let private footerCells profit seeds = rowCells none (ofStr "Total") none none profit seeds

  let private cropProfitSummaryFooterRow showSeeds settings profit =
    let profit = profit |> Option.defaultOrMap "???" gold2 |> ofStr
    let seeds =
      match settings.Profit.SeedStrategy with
      | _ when not showSeeds -> none
      | IgnoreSeeds -> none
      | StockpileSeeds -> ofStr (float2 1.0)
      | BuyFirstSeed -> ofStr (float2 0.0)

    footerCells profit seeds

  let private cropProfitSummaryCollapsedRow data (summary: Query.CropProfitSummary) =
    let itemCell = fertilizerAndCropHarvests data None summary.Crop summary.Harvests
    let profit = summary.NetProfit |> Option.defaultOrMap "???" gold2 |> ofStr

    rowCells none itemCell none none profit none

  let private cropProfitSummaryBody
    collapsible
    oneCrop
    data
    settings
    fertilizer
    fertilizerPrice
    (summary: Query.CropProfitSummary)
    =
    let crop = summary.Crop
    let seed = Crop.seed crop
    let items = Crop.items crop
    let body = Array.concat [|
      harvestsSummaryHeaderRow data settings crop summary.Harvests
      stockpiledSeedRow settings
      seedsBoughtRow data seed summary.SeedPrice summary.SeedsBought
      seedAmountsRows data crop summary.SeedAmounts
      forageSeedsRows data settings items seed summary.ForageSeedsSold summary.ForageSeedsUsed
      summary.UnsoldItems |> Array.map (unsoldItemRow data)
      summary.SoldItems |> Array.choose (soldItemRow data)
      summary.SoldProducts |> Array.choose (soldProductRow data)
      fertilizerBoughtRow true fertilizer fertilizerPrice summary.ReplacedFertilizer |> Option.toArray
      [| if not oneCrop then cropProfitSummaryFooterRow true settings summary.NetProfit |]
    |]

    if collapsible then
      let row = cropProfitSummaryCollapsedRow data summary
      Table.collapsibleBody true row body
    else
      tbody (body |> Array.map rowFromCells)

  let private normalizationFooter valueFormat timeNorm normDivisor value =
    if normDivisor = 1.0 then none else
    fragment [
      let divisor = ofStr $"/ {round2 normDivisor} {TimeNormalization.unit timeNorm |> pluralizeTo normDivisor}"

      rowWithCells none none none divisor none

      let normalizedValue =
        value
        |> Option.defaultOrMap "???" (fun value -> valueFormat (value / normDivisor))
        |> ofStr

      rowWithCells (ofStr $"Total {timeNorm}") none none normalizedValue none
    ]

  let private summaryHeader
    (item: string)
    (unitValue: string)
    (quantity: string)
    (value: string)
    (seeds: string option)
    =
    thead [
      tr [
        th []
        th [ colSpan 2; text item ]
        th unitValue
        th "x"
        th quantity
        th value
        th (seeds |> ofOption ofStr)
      ]
    ]

  let private profitSummary collapsible data settings timeNorm total (summaries: Query.ProfitSummary array) =
    let oneCrop = summaries |> Array.sumBy (fun summary -> summary.CropSummaries.Length) = 1

    table [
      let seeds =
        if settings.Profit.SeedStrategy = IgnoreSeeds
        then None
        else Some "Seeds"

      summaryHeader "Item" "Price" "Quantity" "Profit" seeds

      fragment (summaries |> Array.map (fun summary ->
        fragment [
          fertilizerBoughtRow false summary.Fertilizer summary.FertilizerPrice 1.0
          |> ofOption (rowFromCells >> Array.singleton >> tbody)

          summary.CropSummaries
          |> Array.map (cropProfitSummaryBody collapsible oneCrop data settings summary.Fertilizer summary.FertilizerPrice)
          |> fragment
        ]
      ))

      tfoot [
        cropProfitSummaryFooterRow oneCrop settings total |> rowFromCells

        if timeNorm <> TotalPeriod then
          assert (summaries.Length = 1)
          normalizationFooter gold2 timeNorm summaries[0].TimeNormalization summaries[0].NetProfit
      ]
    ]

  // TODO
  let roiSummary settings timeNorm (profitSummary: Query.ProfitSummary) =
    let investment = profitSummary.Investment (settings.Profit.SeedStrategy = BuyFirstSeed)
    let roi = investment |> Option.bind profitSummary.ROI
    div [
      div [
        ofStr "Investment: "
        ofStr (investment |> Option.defaultOrMap "???" gold)
      ]
      div [
        let roi = roi |> Option.defaultOrMap "???" percent2
        ofStr $"ROI: {roi}"
      ]

      if profitSummary.TimeNormalization <> 1.0 then
        div $"/ {round2 profitSummary.TimeNormalization} {TimeNormalization.unit timeNorm |> pluralizeTo profitSummary.TimeNormalization}"

        let roi = roi |> Option.defaultOrMap "???" (fun roi ->
          $"{percent2 (roi / profitSummary.TimeNormalization)} / {TimeNormalization.unit timeNorm}")

        div $"ROI {timeNorm}: {roi}"
    ]

  let rankerProfit roi data settings timeNorm fertilizer crop =
    match Query.Ranker.profitSummary data settings timeNorm fertilizer crop with
    | Some summary ->
      div [ Class.breakdownTable; children [
        profitSummary false data settings (if roi then TotalPeriod else timeNorm) summary.NetProfit [| summary |]
        if roi then roiSummary settings timeNorm summary
      ]]
    | None -> noHarvestsMessage settings crop

  let mergeCropHarvests (solution: Solver.FertilizerDateSpan) =
    let regrowHarvests = solution.RegrowCrop |> Option.map (fun (_, crop, harvests) -> FarmCrop crop, harvests)
    let cropHarvests = solution.CropHarvests |> Array.map (fun (crop, harvests, _) -> crop, harvests)
    regrowHarvests
    |> Option.toArray
    |> Array.append cropHarvests
    |> Array.fold (fun list (crop, harvests) ->
      match list with
      | (lastCrop, lastHarvests) :: list when Crop.seed lastCrop = Crop.seed crop ->
        (crop, harvests + lastHarvests) :: list
      | list -> (crop, harvests) :: list)
      []
    |> Array.ofList
    |> Array.rev

  let private solverSummary summaryTable summaries data settings total (solutions: Solver.FertilizerDateSpan array) =
    let summaries = solutions |> Array.map (fun solution ->
      mergeCropHarvests solution |> summaries data settings solution.Fertilizer)

    div [ Class.breakdownTable; children (summaryTable TotalPeriod total summaries: ReactElement) ]

  let solverProfit data settings total solutions =
    solverSummary (profitSummary true data settings) Query.Solver.profitSummary data settings (Some total) solutions

  let private tooltipRow bought (icon: ReactElement) price amount =
    match price with
    | Some price ->
      significantRow none icon (Some (price, bought)) amount 0.0 |> Option.defaultOrMap none rowFromCells
    | None ->
      if amount = 0.0 then none else
      rowFromCells (unknownRow none icon true amount 0.0)

  let inline private tooltipBoughtRow icon price amount = tooltipRow true icon price amount
  let inline private tooltipSoldRow icon price amount = tooltipRow false icon (Some price) amount

  let tooltipProfit data settings timeNorm roi (profitSummary: Query.ProfitSummary) =
    let summary = profitSummary.CropSummaries[0]
    let seed = Crop.seed summary.Crop
    table [
      summaryHeader "Item" "Price" "Quantity" "Profit" None

      tbody [
        profitSummary.FertilizerPrice |> ofOption (fun price ->
          tooltipBoughtRow
            (profitSummary.Fertilizer |> ofOption Image.Icon.fertilizer)
            (Option.map snd price)
            (1.0 + summary.ReplacedFertilizer))

        tooltipBoughtRow
          (Image.Icon.seed data seed)
          (Option.map snd summary.SeedPrice)
          summary.SeedsBought

        tooltipSoldRow
          (Image.Icon.seed data seed)
          (Game.seedItemSellPrice data settings.Game seed)
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

        rowFromCells (footerCells (profitSummary.NetProfit |> Option.defaultOrMap "???" gold2 |> ofStr) none)
      ]

      if roi
      then none
      else normalizationFooter gold2 timeNorm profitSummary.TimeNormalization profitSummary.NetProfit
    ]

  module XP =
    let private keyValueRowWithOperation (operation: string option) (key: string) (valueCell: ReactElement) =
      tr [
        td (key + ":")
        td (operation |> ofOption ofStr)
        td valueCell
      ]

    let private keyValueRowOperation key operation value = keyValueRowWithOperation (Some operation) key value

    let private keyValueRow key value = keyValueRowWithOperation None key value

    let private verticalSummary timeNorm (xpSummary: Query.XpSummary) =
      let summary = xpSummary.CropSummaries[0]
      let timeNormUnit = TimeNormalization.unit timeNorm |> pluralize
      table [
        tbody [
          keyValueRow "XP" (summary.XpPerItem |> xp |> ofStr)
          keyValueRowOperation "Quantity" "x" (summary.ItemQuantity |> round2 |> ofFloat)
          keyValueRow "Total XP" (xpSummary.Xp |> round2 |> xpFloat |> ofStr)

          if xpSummary.TimeNormalization <> 1.0 then
            keyValueRowOperation
              $"{upperFirstChar timeNormUnit}"
              "/"
              (ofStr $"{round2 xpSummary.TimeNormalization} {timeNormUnit}")

            keyValueRow
              $"XP {(string timeNorm).ToLower()}"
              ((xpSummary.Xp / xpSummary.TimeNormalization) |> round2 |> xpFloat |> ofStr)
        ]
      ]

    let rankerAndTooltip data settings timeNorm fertilizer crop =
      fragment [
        match Query.Ranker.xpSummary data settings timeNorm fertilizer crop with
        | Ok summary ->
          fertilizerAndCropHarvests data fertilizer crop summary.CropSummaries[0].Harvests
          verticalSummary timeNorm summary
        | Error e ->
          fertilizerAndCropHarvests data fertilizer crop 0u
          invalidReasons settings crop e
      ]

    let private tableSummary data total (summaries: Query.XpSummary array) =
      table [
        thead [
          th "Fertilizer"
          th "Crop"
          th "Harvests"
          th "XP"
          th "x"
          th "Quantity"
          th "Total XP"
        ]

        tbody (summaries |> Array.collect (fun xpSummary ->
          let fertilizer = xpSummary.Fertilizer |> ofOption Image.Icon.fertilizer
          xpSummary.CropSummaries |> Array.map (fun summary ->
            tr [
              td fertilizer
              td (Image.Icon.crop data summary.Crop)
              td (ofNat summary.Harvests)
              td (summary.XpPerItem |> xp |> ofStr)
              td "x"
              td (summary.ItemQuantity |> round2 |> ofFloat)
              td (xpSummary.Xp |> round2 |> xpFloat |> ofStr)
            ])
        ))

        tfoot [
          tr [
            td [ colSpan 6; text "Total" ]
            td (total |> round2 |> xpFloat |> ofStr)
          ]
        ]
      ]

    let solver data settings total fertilizerSpans =
      fertilizerSpans
      |> Array.map (fun solution ->
        mergeCropHarvests solution |> Query.Solver.xpSummary data settings solution.Fertilizer)
      |> tableSummary data total


type PairData = {
  Crops: Crop array
  Fertilizers: Fertilizer option array
  Pairs: ((SeedId * FertilizerName option) * Result<float, Query.InvalidReasons>) array
}

let private pairData metric timeNorm data settings =
  let crops = Query.Selected.inSeasonCrops data settings |> Array.ofSeq
  let fertilizers = Query.Selected.fertilizersOpt data settings |> Array.ofSeq
  let rankValue = Query.Ranker.rankValue metric

  let data = crops |> Array.collect (fun crop ->
    let profit = rankValue data settings timeNorm crop
    fertilizers |> Array.map (fun fert ->
      (Crop.seed crop, Fertilizer.Opt.name fert), profit fert))

  {
    Crops = crops
    Fertilizers = fertilizers
    Pairs = data
  }

let private emptyPairData (pairData: PairData) =
  div [
    if Array.isEmpty pairData.Crops then ofStr "No crops selected!"
    if Array.isEmpty pairData.Fertilizers then ofStr "No fertilizers selected!"
  ]

let rankBy labelText metric timeNorm dispatchMetric dispatchTimeNorm =
  fragment [
    ofStr labelText

    Select.options (length.rem 4) (fun metric ->
      div [
        text (string metric)
        title (RankMetric.fullName metric)
      ])
      unitUnionCases
      metric
      dispatchMetric

    Select.options (length.rem 7) (fun timeNorm ->
      div [
        text (string timeNorm)
        title (TimeNormalization.description timeNorm)
      ])
      unitUnionCases
      timeNorm
      dispatchTimeNorm
  ]

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
      svg.className "pair-image"
      svg.onClick (fun _ -> selectPair index)
      svg.x (props.x - 10.0)
      svg.y props.y
      svg.width 20
      svg.height 40
      svg.children [
        Svg.image [
          svg.href (Image.itemRoot (Crop.mainItem data.Crops[crop] |> string))
          svg.width 20
          svg.height 20
        ]
        fert |> ofOption (fun fert ->
          Svg.image [
            svg.href (Image.fertilizerRoot fert)
            svg.width 20
            svg.height 20
            svg.y 20
          ])
      ]
    ]

  let chartProfitTooltip data settings timeNorm roi fertilizer crop (profit: Result<float, Query.InvalidReasons>) =
    div [
      match Query.Ranker.profitSummary data settings timeNorm fertilizer crop with
      | Some profitSummary ->
        let summary = profitSummary.CropSummaries[0]
        fertilizerAndCropHarvests data fertilizer crop summary.Harvests
        match profit with
        | Ok profit ->
          assert
            roi || profitSummary.NetProfit |> Option.exists (fun x ->
              abs (x / profitSummary.TimeNormalization - profit) < 1e-5)
          none
        | Error e -> invalidReasons settings crop e
        SummaryTable.tooltipProfit data settings timeNorm roi profitSummary
      | None ->
        fertilizerAndCropHarvests data fertilizer crop 0u
        match profit with
        | Ok _ -> assert false; none
        | Error e -> invalidReasons settings crop e
      ]

  let private chartTooltip (data: GameData) settings timeNorm rankItem (pairs: (SeedId * string option) array) props =
    match props?payload with
    | Some (payload: _ array) when payload.Length > 0 && props?active ->
      let (index: int, result: Result<float, Query.InvalidReasons>) = payload[0]?payload
      let seed, fertilizer = pairs[index]
      let crop = data.Crops[seed]
      let fertilizer = Option.map data.Fertilizers.Find fertilizer
      match rankItem with
      | Gold | ROI -> chartProfitTooltip data settings timeNorm (rankItem = ROI) fertilizer crop result
      | XP -> SummaryTable.XP.rankerAndTooltip data settings timeNorm fertilizer crop
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
      svg.className "pair-select"
      svg.onClick (fun _ -> selectPair i)
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
        // svg.className "recharts-rectangle"
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
            fert |> ofOption (fun fert ->
              Svg.image [
                svg.href (Image.fertilizerRoot fert)
                svg.height width
              ])
          if flags.HasFlag Query.InvalidReasons.NotEnoughSeeds then
            Svg.image [
              svg.href (Image.itemRoot (string crop))
              svg.height width
              svg.y width
            ]
          if flags.HasFlag Query.InvalidReasons.NotEnoughDays then
            Svg.image [
              svg.href (Image.uiRoot "Time")
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
            yAxis.domain (domain.constant 0, domain.auto)
            if ranker.RankMetric = ROI then Interop.mkYAxisAttr "tickFormatter" (fun x -> x * 100.0)
            yAxis.width 60
          ]
          Recharts.tooltip [
            tooltip.content (chartTooltip data settings ranker.TimeNormalization ranker.RankMetric pairs)
          ]
          Recharts.bar [
            bar.dataKey (snd >> function Ok y -> y | Error _ -> 0.0)
            bar.fill "blue"
            bar.onClick (fun props -> props?payload |> fst |> selectPair)
            Interop.mkBarAttr "background" (barBackground barGap selectPair)
            Interop.mkBarAttr "shape" (errorBar pairs)
          ]
          Recharts.brush [
            brush.startIndex (ranker.BrushSpan |> fst |> int |> min (graphData.Length - 1) |> max 0)
            brush.endIndex (ranker.BrushSpan |> snd |> int |> min (graphData.Length - 1) |> max 0)
            brush.height 30
            Interop.mkBrushAttr "onChange" (fun i -> SetBrushSpan (i?startIndex, i?endIndex) |> dispatch)
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

  let ranker ranker (data, settings) dispatch =
    let pairData = pairData ranker.RankMetric ranker.TimeNormalization data settings
    if Array.isEmpty pairData.Pairs then emptyPairData pairData else
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

    let pairData =
      pairs
      |> Array.indexed
      |> Array.map (fun (i, (_, profit)) -> i, profit)

    let pairs = pairs |> Array.map fst

    pairData |> Array.sortInPlaceWith (fun a b ->
      match snd a, snd b with
      | Ok a, Ok b -> compare b a
      | Ok _, Error _ -> -1
      | Error _, Ok _ -> 1
      | Error a, Error b -> Enum.bitSetOrder (int a) (int b))

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
          unitUnionCases
          ranker.RankItem
          (SetRankItem >> dispatch)

        rankBy
          "By"
          ranker.RankMetric
          ranker.TimeNormalization
          (SetRankMetric >> dispatch)
          (SetTimeNormalization >> dispatch)

        checkboxText "Show Invalid" ranker.ShowInvalid (SetShowInvalid >> dispatch)
      ]]
      if Array.isEmpty pairs then
        div "No valid pairs of crops and fertilizers!"
      else
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

let [<ReactComponent>] CropAndFertilizerSummary (props: {|
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

    let metricValue = Query.Ranker.rankValue metric

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

      div
        (rankBy
          "Show "
          metric
          timeNorm
          (fun metric -> setState (metric, timeNorm))
          (fun timeNorm -> setState (metric, timeNorm)))
    ]]

    match crop, fert with
    | (Choice1Of2 crop | Choice2Of2 (Some crop)), (Choice1Of2 fert | Choice2Of2 (Some fert)) ->
      animatedDetails
        (ui.OpenDetails.Contains OpenDetails.RankerSummary)
        (ofStr "Summary")
        (match metric with
          | Gold -> SummaryTable.rankerProfit false data settings timeNorm fert crop
          | XP -> SummaryTable.XP.rankerAndTooltip data settings timeNorm fert crop
          | ROI -> SummaryTable.rankerProfit true data settings timeNorm fert crop)
        (curry SetDetailsOpen OpenDetails.RankerSummary >> appDispatch)

      animatedDetails
        (ui.OpenDetails.Contains OpenDetails.RankerGrowthCalendar)
        (ofStr "Growth Calendar")
        (GrowthCalendar.ranker app fert crop)
        (curry SetDetailsOpen OpenDetails.RankerGrowthCalendar >> appDispatch)

    | _ -> emptyPairData pairData
  ]]


let rankerOrSummary app dispatch =
  let settings, ui = app.State
  let ranker = ui.Ranker
  match ranker.SelectedCropAndFertilizer with
  | Some (crop, fert) -> CropAndFertilizerSummary {| App = app; Seed = crop; Fertilizer = fert; Dispatch = dispatch |}
  | None -> Elmish.React.Common.lazyView3 Ranker.ranker ranker (app.Data, settings) (SetRanker >> dispatch)


// The solver typically takes < 50ms, but with
//   StartSeason = Spring, StopSeason = Fall, Location = Greenhouse, and FarmingLevel in [0..7],
// it can take around 600ms if all fertilizers and crops are selected.
// For this reason, the solver is put in a web worker with a debouncer.

let private workerQueue, private workerSubscribe =
  let queue, subscribe = Solver.createWorker ()
  debouncer 200 (fun (data, settings, mode) -> queue data settings mode), subscribe

let private solverSummary data settings mode solution =
  fragment [
    match solution with
    | Some (solution, total) ->
      match mode with
      | MaximizeGold -> SummaryTable.solverProfit data settings total solution
      | MaximizeXP -> SummaryTable.XP.solver data settings total solution
      div [
        Class.calendar
        children (solution |> Array.collect (GrowthCalendar.solver settings))
      ]
    | None -> ofStr "Loading..."
  ]

let [<ReactComponent>] Solver (props: {|
    Data: GameData
    Settings: Settings
    SolverMode: SolverMode
  |}) =
  let data = props.Data
  let settings = props.Settings
  let mode = props.SolverMode

  let (solution, solving), setState = useState ((None, false))

  useEffect ((fun () -> workerSubscribe (fun solution -> setState (Some solution, false))), [||])

  useEffect ((fun () ->
    setState (solution, true)
    workerQueue (data, settings, mode)
  ), [| box data; settings; mode |])

  div [
    if solving then className "disabled"
    children (Elmish.React.Common.lazyView (solverSummary data settings mode) solution)
  ]

let section app dispatch =
  let settings, ui = app.State
  let uiDispatch = SetUI >> dispatch

  section [
    prop.id (if ui.Mode = Ranker && ui.Ranker.SelectedCropAndFertilizer.IsNone then "visualization-graph" else "visualization")
    children [
      viewTabs ui.Mode (SetAppMode >> uiDispatch)
      match ui.Mode with
      | Ranker -> rankerOrSummary app uiDispatch
      | Solver ->
        labeled "Maximize: " (Select.unitUnion (length.rem 5) ui.SolverMode (SetSolverMode >> uiDispatch))
        Solver {|
          Data = app.Data
          Settings = settings
          SolverMode = ui.SolverMode
        |}
    ]
  ]
