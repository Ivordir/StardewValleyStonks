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

let private fertilizerAndCropHarvests data fertilizer crop =
  div [
    Image.Icon.crop data crop

    fertilizer |> ofOption (fun fertilizer ->
      fragment [
        ofStr " with "
        Image.Icon.fertilizer fertilizer
      ])
  ]

module SummaryTable =
  let private toPrecision (value: float) =
    let rounded = round2 value
    if rounded = 0.0
    then sprintf "%.2e" value
    else float2 rounded

  let private keyValueRowWithOperation (operation: string option) (key: string) (valueCell: ReactElement) =
    tr [
      td (key + ":")
      td (operation |> ofOption ofStr)
      td valueCell
    ]

  let private keyValueRowOperation key operation value = keyValueRowWithOperation (Some operation) key value
  let private keyValueRow key value = keyValueRowWithOperation None key value

  let private formatNormalizationDivisor timeNorm normDivisor =
    $"{round2 normDivisor} {TimeNormalization.unit timeNorm |> pluralize}"

  let private formatNormalizedValue format normDivisor value =
    value |> Option.defaultOrMap "???" (fun value -> format (value / normDivisor))

  let private normalizationRows key valueFormat timeNorm normDivisor value =
    if normDivisor = 1.0 then none else

    fragment [
      keyValueRowOperation
        (TimeNormalization.unit timeNorm |> pluralize |> upperFirstChar)
        "/"
        (formatNormalizationDivisor timeNorm normDivisor |> ofStr)

      keyValueRow
        $"{key} {(string timeNorm).ToLower()}"
        (value |> formatNormalizedValue valueFormat normDivisor |> ofStr)
    ]

  let private roiSummary settings timeNorm (profitSummary: Query.ProfitSummary) =
    let investment, roi = profitSummary.InvestmentAndROI (settings.Profit.SeedStrategy = BuyFirstSeed)

    table [
      tbody [
        keyValueRow "Investment" (investment |> Option.defaultOrMap "???" gold |> ofStr)
        keyValueRow "ROI" (roi |> Option.defaultOrMap "???" percent2 |> ofStr)
        normalizationRows "ROI" percent2 timeNorm profitSummary.TimeNormalization roi
      ]
    ]

  let private rowCells
    itemCellColSpan
    (itemCell: ReactElement)
    (unitValue: ReactElement)
    (quantity: ReactElement)
    (value: ReactElement)
    (seeds: ReactElement)
    =
    [|
      td [ colSpan itemCellColSpan; children itemCell ]
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

  let private significantRow seeds itemCellColSpan itemCell (price: (nat * bool) option) quantity =
    let profit = price |> Option.defaultOrMap 0.0 (fun (price, bought) ->
      round2 (float price * quantity * if bought then -1.0 else 1.0))

    if profit = 0.0 && round2 quantity = 0.0 then None else

    let price = price |> ofOption (fst >> gold >> ofStr)
    let quantity = if quantity = 0.0 then none else ofStr (toPrecision quantity)
    let profit = if profit = 0.0 then none else ofStr (gold2 profit)
    let seeds = if seeds then quantity else none

    Some (rowCells itemCellColSpan itemCell price quantity profit seeds)

  let private seedRow itemCellColSpan itemCell quantity =
    significantRow true itemCellColSpan itemCell None quantity

  let private profitRow itemCellColSpan itemCell price quantity =
    significantRow false itemCellColSpan itemCell (Some price) quantity

  let private unknownProfitRow seeds itemCellColSpan itemCell quantity =
    if quantity = 0.0 then None else
    let quantity = quantity |> toPrecision |> ofStr
    rowCells
      itemCellColSpan
      itemCell
      none
      quantity
      (ofStr "???")
      (if seeds then quantity else none)
    |> Some

  let private boughtRow seeds itemCellColSpan priceAndVendor quantity (icon: ReactElement) =
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
      significantRow seeds itemCellColSpan itemCell (Some (price, true)) quantity

    | None ->
      let itemCell = fragment [
        icon
        ofStr " from ???"
      ]
      unknownProfitRow seeds itemCellColSpan itemCell quantity

  let private itemCellWithInputs data itemCell inputs =
    let inputRows = inputs |> Array.choose (fun (item, quantity) ->
      let quantity = round2 quantity
      if quantity = 0.0
      then None
      else Some (item, quantity))

    fragment [
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
      itemCell
    ]

  let private fertilizerBoughtRow itemCellColSpan replacement fertilizer price amount =
    match fertilizer, price with
    | Some fertilizer, Some price ->
      boughtRow false itemCellColSpan price amount (fragment [
        if replacement then ofStr "Replacement "
        Image.Icon.fertilizer fertilizer
      ])
    | _ -> None

  let private harvestedSeedsRows itemCellColSpan data item (amounts: _ Qualities) =
    amounts
    |> Qualities.indexed
    |> Array.choose (fun (quality, amount) ->
      seedRow itemCellColSpan (Image.Icon.itemQuality' data item quality) amount)

  let private seedMakerRows itemCellColSpan data crop item (amounts: _ Qualities) =
    let seed = Crop.seed crop
    let itemCell =
      amounts
      |> Qualities.indexed
      |> Array.map (fun (quality, amount) -> (item, quality), amount)
      |> itemCellWithInputs data (Image.Icon.seed data seed)

    let seedAmount = Qualities.sum amounts * Processor.seedMakerExpectedQuantity seed

    seedRow itemCellColSpan itemCell seedAmount |> Option.toArray

  let private forageSeedsRows itemCellColSpan data settings items (seed: SeedId) amountSold amountUsed =
    if amountSold = 0.0 && amountUsed = 0.0 then Array.empty else
    let itemCell = Image.Icon.seed data seed
    let inputsCell amount =
      let itemAmount = amount / float ForageCrop.forageSeedsPerCraft
      items
      |> Array.map (fun item -> (item, Quality.Normal), itemAmount)
      |> itemCellWithInputs data itemCell

    let price = Game.seedItemSellPrice data settings.Game seed
    Array.collect Option.toArray [|
      seedRow itemCellColSpan (inputsCell amountUsed) amountUsed
      profitRow itemCellColSpan (inputsCell amountSold) (price, false) amountSold
    |]

  let private unsoldItemRow itemCellColSpan data (item, quantities: float Qualities) =
    let itemCell =
      quantities
      |> Qualities.indexed
      |> Array.map (fun (quality, amount) -> (item, quality), amount)
      |> itemCellWithInputs data (ofStr "???")

    rowCells itemCellColSpan itemCell none none none none

  let private soldItemRow itemCellColSpan data (summary: Query.SoldItemSummary) =
    let itemCell = fragment [
      Image.Icon.itemQuality' data summary.Item summary.Quality
      if summary.Custom then ofStr " (Custom)"
    ]

    profitRow itemCellColSpan itemCell (summary.Price, false) summary.Quantity

  let private soldProductRow itemCellColSpan data (summary: Query.SoldProductSummary) =
    let itemCell =
      itemCellWithInputs
        data
        (Image.Icon.productQuality data summary.Product summary.Quality)
        summary.ConsumedItemQuantities

    profitRow itemCellColSpan itemCell (summary.Price, false) summary.Quantity

  let private cropProfitSummaryRows
    itemCellColSpan
    data
    settings
    fertilizer
    fertilizerPrice
    (summary: Query.CropProfitSummary)
    =
    let crop = summary.Crop
    let seed = Crop.seed crop

    [|
      if settings.Profit.SeedStrategy <> IgnoreSeeds then
        let harvests = (-float (if Crop.regrows crop then 1u else summary.Harvests)) |> float2 |> ofStr
        [| rowCells itemCellColSpan (ofStr $"{summary.Harvests} harvests") none none none harvests |]

      boughtRow true itemCellColSpan summary.SeedPrice summary.SeedsBought (Image.Icon.seed data seed) |> Option.toArray

      summary.SeedAmounts |> Array.collect (fun (item, quantities) ->
        if item = Crop.seedItem crop then harvestedSeedsRows itemCellColSpan data item quantities
        elif item = Crop.mainItem crop then seedMakerRows itemCellColSpan data crop item quantities
        else assert false; Array.empty)

      forageSeedsRows itemCellColSpan data settings (Crop.items crop) seed summary.ForageSeedsSold summary.ForageSeedsUsed

      summary.UnsoldItems |> Array.map (unsoldItemRow itemCellColSpan data)
      summary.SoldItems |> Array.choose (soldItemRow itemCellColSpan data)
      summary.SoldProducts |> Array.choose (soldProductRow itemCellColSpan data)

      fertilizerBoughtRow itemCellColSpan true fertilizer fertilizerPrice summary.ReplacedFertilizer |> Option.toArray
    |]
    |> Array.concat
    |> Array.map fragment

  let private totalRow totalColSpan totalFormat total (seeds: ReactElement) =
    tr [
      td [ colSpan totalColSpan; text "Total" ]
      td (total |> Option.defaultOrMap "???" totalFormat |> ofStr)
      td seeds
    ]

  let private profitHeader seeds =
    thead [
      tr [
        th "Item"
        th "Price"
        th "x"
        th "Quantity"
        th "Profit"
        th (if seeds then ofStr "Seeds" else none)
      ]
    ]

  let private profitFooter timeNorm normDivisor profit seeds =
    tfoot [
      totalRow 4 gold2 profit seeds

      if timeNorm = TotalPeriod || normDivisor = 1.0 then none else
        tr [
          td [ colSpan 4 ]
          td $"/ {formatNormalizationDivisor timeNorm normDivisor}"
        ]
        tr [
          td [ colSpan 4; text $"Total {(string timeNorm).ToLower()}" ]
          td (profit |> formatNormalizedValue gold2 normDivisor)
        ]
    ]

  let private rankerProfitSummary data settings timeNorm (profitSummary: Query.ProfitSummary) =
    table [
      profitHeader (settings.Profit.SeedStrategy <> IgnoreSeeds)

      fertilizerBoughtRow 1 false profitSummary.Fertilizer profitSummary.FertilizerPrice 1.0
      |> ofOption (tr >> Array.singleton >> tbody)

      profitSummary.CropSummaries[0]
      |> cropProfitSummaryRows 1 data settings profitSummary.Fertilizer profitSummary.FertilizerPrice
      |> Array.map (Array.singleton >> tr)
      |> tbody

      let seeds =
        if settings.Profit.SeedStrategy = IgnoreSeeds
        then none
        else ofStr (float2 0.0)

      profitFooter timeNorm profitSummary.TimeNormalization profitSummary.NetProfit seeds
    ]

  let private solverHeader seeds (unitValue: string) (value: string) =
    fragment [
      th "Fertilizer"
      th "Crop"
      th "Harvests"
      th unitValue
      th "x"
      th "Quantity"
      th value
      th (if seeds then ofStr "Seeds" else none)
    ]

  let private solverProfitSummary data settings total (summaries: Query.ProfitSummary array) =
    let header = solverHeader (settings.Profit.SeedStrategy <> IgnoreSeeds) "Price" "Profit"

    // A key is needed for each collapsible table body to preserve the collapsed state across new solutions/renders.
    // However, crops may appear multiple times across dateSpans,
    // or possibly multiple times within the same dateSpan,
    // so a unique id is needed for each instance of the same crop.
    // This does not ensure proper preservation the collapsed state between different instances of the same crop,
    // but does between different crops -- good enough.
    let getCount =
      let counts = Dictionary ()
      fun seed ->
        let count =
          match counts.TryGetValue seed with
          | true, value -> value
          | _ -> 0
        counts[seed] <- count + 1
        count

    let bodies = summaries |> Array.collect (fun profitSummary -> Array.concat [|
      fertilizerBoughtRow 3 false profitSummary.Fertilizer profitSummary.FertilizerPrice 1.0
      |> Option.map (fun row -> "", fragment row, [||])
      |> Option.toArray

      profitSummary.CropSummaries |> Array.map (fun summary ->
        let key =
          let seed = Crop.seed summary.Crop
          let count = getCount seed
          string seed + "@" + string count

        let row = fragment [
          td (profitSummary.Fertilizer |> ofOption Image.Icon.fertilizer)
          td (Image.Icon.crop data summary.Crop)
          td (ofNat summary.Harvests)
          td [ colSpan 3 ]
          td (summary.NetProfit |> ofOption (gold2 >> ofStr))
          td []
        ]

        let body = cropProfitSummaryRows 3 data settings profitSummary.Fertilizer profitSummary.FertilizerPrice summary

        key, row, body)
    |])

    table [
      Table.collapsibleHeaderAndBodies true header bodies

      tfoot [ totalRow 7 gold2 (Some total) none ]
    ]

  let private tooltipBoughtRow itemCell price quantity =
    match price with
    | Some (_, price) -> profitRow 1 itemCell (price, true) quantity
    | None -> unknownProfitRow false 1 itemCell quantity
    |> ofOption tr

  let private tooltipProfitRow itemCell price quantity = profitRow 1 itemCell (price, false) quantity |> ofOption tr

  let private profitTooltip data settings timeNorm (profitSummary: Query.ProfitSummary) =
    let summary = profitSummary.CropSummaries[0]
    let seed = Crop.seed summary.Crop
    table [
      profitHeader false

      tbody [
        profitSummary.FertilizerPrice |> ofOption (fun price ->
          tooltipBoughtRow
            (profitSummary.Fertilizer |> ofOption Image.Icon.fertilizer)
            price
            (1.0 + summary.ReplacedFertilizer))

        tooltipBoughtRow (Image.Icon.seed data seed) summary.SeedPrice summary.SeedsBought

        tooltipProfitRow
          (Image.Icon.seed data seed)
          (Game.seedItemSellPrice data settings.Game seed)
          summary.ForageSeedsSold

        fragment (summary.SoldItems |> Array.map (fun summary ->
          tooltipProfitRow (Image.Icon.itemQuality' data summary.Item summary.Quality) summary.Price summary.Quantity))

        fragment (summary.SoldProducts |> Array.map (fun summary ->
          tooltipProfitRow
            (Image.Icon.productQuality data summary.Product summary.Quality)
            summary.Price
            summary.Quantity))
      ]

      profitFooter timeNorm profitSummary.TimeNormalization profitSummary.NetProfit none
    ]

  let private mergeCropHarvests (solution: Solver.FertilizerDateSpan) =
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

  module Profit =
    let ranker data settings timeNorm fertilizer crop =
      match Query.Ranker.profitSummary data settings timeNorm fertilizer crop with
      | Some summary -> rankerProfitSummary data settings timeNorm summary
      | None -> noHarvestsMessage settings crop

    let tooltip = profitTooltip

    let solver data settings total dateSpans =
      let summaries = dateSpans |> Array.map (fun solution ->
        mergeCropHarvests solution |> Query.Solver.profitSummary data settings solution.Fertilizer)

      solverProfitSummary data settings total summaries

  module ROI =
    let ranker data settings timeNorm fertilizer crop =
      match Query.Ranker.profitSummary data settings timeNorm fertilizer crop with
      | Some summary ->
        fragment [
          rankerProfitSummary data settings TotalPeriod summary
          roiSummary settings timeNorm summary
        ]
      | None -> noHarvestsMessage settings crop

    let tooltip data settings timeNorm summary =
      fragment [
        profitTooltip data settings TotalPeriod summary
        roiSummary settings timeNorm summary
      ]

  module XP =
    let private verticalSummary timeNorm (xpSummary: Query.XpSummary) =
      let summary = xpSummary.CropSummaries[0]
      table [
        tbody [
          keyValueRow "Harvests" (ofNat summary.Harvests)
          keyValueRow "XP" (summary.XpPerItem |> xp |> ofStr)
          keyValueRowOperation "Quantity" "x" (summary.ItemQuantity |> round2 |> ofFloat)
          keyValueRow "Total XP" (xpSummary.Xp |> round2 |> xpFloat |> ofStr)
          normalizationRows "XP" (round2 >> xpFloat) timeNorm xpSummary.TimeNormalization (Some xpSummary.Xp)
        ]
      ]

    let ranker data settings timeNorm fertilizer crop =
      match Query.Ranker.xpSummary data settings timeNorm fertilizer crop with
      | Ok summary -> verticalSummary timeNorm summary
      | Error e -> invalidReasons settings crop e

    let tooltip data settings timeNorm fertilizer crop =
      fragment [
        fertilizerAndCropHarvests data fertilizer crop
        ranker data settings timeNorm fertilizer crop
      ]

    let private tableSummary data total (summaries: Query.XpSummary array) =
      table [
        thead [ tr [ solverHeader false "XP" "Total XP" ]]

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
              td []
            ])
        ))

        tfoot [ totalRow 6 (round2 >> xpFloat) (Some total) none ]
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
        fertilizerAndCropHarvests data fertilizer crop
        ofStr $" ({summary.Harvests} harvests)"
        match profit with
        | Ok profit ->
          assert
            roi || profitSummary.NetProfit |> Option.exists (fun x ->
              abs (x / profitSummary.TimeNormalization - profit) < 1e-5)
          none
        | Error e -> invalidReasons settings crop e

        if roi
        then SummaryTable.ROI.tooltip data settings timeNorm profitSummary
        else SummaryTable.Profit.tooltip data settings timeNorm profitSummary

      | None ->
        fertilizerAndCropHarvests data fertilizer crop
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
      | XP -> SummaryTable.XP.tooltip data settings timeNorm fertilizer crop
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
          | Gold -> SummaryTable.Profit.ranker data settings timeNorm fert crop
          | ROI -> SummaryTable.ROI.ranker data settings timeNorm fert crop
          | XP -> SummaryTable.XP.ranker data settings timeNorm fert crop)
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
      | MaximizeGold -> SummaryTable.Profit.solver data settings total solution
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
