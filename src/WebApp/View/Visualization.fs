module StardewValleyStonks.WebApp.View.Visualization

open StardewValleyStonks
open StardewValleyStonks.WebApp
open StardewValleyStonks.WebApp.Update
open StardewValleyStonks.WebApp.View

open Fable.Core
open Feliz
open Elmish.React

open type Html
open type prop
open type React

open Core.Operators
open Core.ExtraTopLevelOperators

let noHarvestsMessage settings crop =
  if Game.cropIsInSeason settings.Game crop
  then "No harvests possible."
  else "Crop is not in season."
  |> Icon.warning

let invalidReasons settings crop (reasons: Query.InvalidReasons) =
  ul [ className Class.messages; children [
    if reasons.HasFlag Query.InvalidReasons.NotEnoughDays then
      li (noHarvestsMessage settings crop)

    if reasons.HasFlag Query.InvalidReasons.NotEnoughSeeds then
      li (Icon.warning (if settings.Profit.SeedStrategy = BuyFirstSeed then "No seed price." else "No seed source."))

    if reasons.HasFlag Query.InvalidReasons.NoFertilizerPrice then
      li (Icon.warning "No fertilizer price.")

    if reasons.HasFlag Query.InvalidReasons.NoInvestment then
      li (Icon.warning "No investment.")
  ]]


module GrowthCalendar =
  let rec private repeatCons n items list =
    if n = 0u
    then list
    else repeatCons (n - 1u) items (items :: list)

  let private nameItem = function
    | FarmCrop crop -> crop.Item
    | ForageCrop crop -> toItem crop.Seed

  let stageImage seed item stage = div (Icon.growthStage seed item stage)

  let stageImages seed item stages =
    stages
    |> Array.mapi (fun i stage -> Array.create (int stage) (stageImage seed item i))
    |> Array.concat
    |> Array.tail

  let private regrowCropCalendarDays data settings (days: nat array) fertilizer stageList (season, crop, harvests) =
    let totalDays = Array.sum days[(season + 1)..]
    let stages, time = Game.growthTimeAndStages settings.Game fertilizer (FarmCrop crop)
    let usedDays = Growth.daysNeededFor crop.RegrowTime time harvests
    let item = data.Items[crop.Item]
    let stageImages = stages |> stageImages crop.Seed item

    let harvestItem = div (Icon.NoText.item item)
    let firstHarvest = stageImages |> Array.insertEnd harvestItem
    let regrow = Array.create (int crop.RegrowTime.Value) (div (Icon.regrowStage crop.Seed item))
    regrow[regrow.Length - 1] <- harvestItem
    let filler = max 0 (int totalDays - int usedDays)
    let stageList = Array.create filler (div []) :: stageList
    let stageList = repeatCons (harvests - 1u) regrow stageList
    let stageList = firstHarvest :: stageList
    season, totalDays + days[season] - usedDays - nat filler, stageList

  let private fromDateSpan data settings single (span: Optimizer.FertilizerDateSpan) =
    let stageList = [ Array.create (int (Date.daysInSeason - span.EndDate.Day)) (div [ className Class.disabled ]) ]
    let days =
      if not single && settings.Game.Location = Farm
      then Date.daySpan span.StartDate span.EndDate
      else [| Date.totalDays span.StartDate span.EndDate |]

    let season, remainingDays, stageList =
      span.RegrowCrop
      |> Option.defaultOrMap
        (days.Length - 1, Array.last days, stageList)
        (regrowCropCalendarDays data settings days span.Fertilizer stageList)

    let season, days, stageList =
      (span.CropHarvests, (season, int remainingDays, stageList))
      ||> Array.foldBack (fun (crop, harvests, bridgeCrop) (season, remainingDays, stageList) ->
        let seed = Crop.seed crop
        let item = data.Items[nameItem crop]

        let stages, time = Game.growthTimeAndStages settings.Game span.Fertilizer crop
        let stageImages = stageImages seed item stages
        let harvestItem = [| div (Icon.NoText.itemId data (Crop.mainItem crop)) |]
        if bridgeCrop then
          let filler = max 0 (int remainingDays - int time)
          let days = int remainingDays + int days[season] - int time - filler
          let stageList =
            stageImages
            :: Array.create filler (stageImage seed item stages.Length)
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
      |> Option.map (fun crop -> stageImage (Crop.seed crop) data.Items[nameItem crop] 0)
      |> Option.toArray

    Array.create (int (span.StartDate.Day - 1u)) (div [ className Class.disabled ])
    :: Array.create (days - 1) (div [])
    :: firstStage
    :: stageList
    |> Array.concat
    |> Array.chunkBySize (int Date.daysInSeason)
    |> Array.zip (Date.seasonSpan span.StartDate span.EndDate)
    |> Array.map (fun (season, days) ->
      div [ className Class.calendarSeason; children [
        div [ className Class.calendarHeader; children [
          Icon.season season
          span.Fertilizer |> ofOption Icon.fertilizer
        ]]

        div [
          className Class.calendarDays
          children days
        ]
      ]])

  let optimizer data settings dateSpan = fromDateSpan data settings false dateSpan

  let ranker data settings fertilizer crop =
    match Query.bestGrowthSpan settings.Game fertilizer crop with
    | None -> noHarvestsMessage settings crop
    | Some span ->
      let cropHarvests, regrowCrop =
        match crop with
        | FarmCrop crop when crop.RegrowTime.IsSome -> [||], Some (0, crop, span.Harvests)
        | _ -> [| crop, span.Harvests, false |], None

      let dateSpan: Optimizer.FertilizerDateSpan = {
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
        className Class.calendar
        children (fromDateSpan data settings true dateSpan)
      ]

let private fertilizerAndCrop data fertilizer crop =
  div [
    Icon.crop data crop

    fertilizer |> ofOption (fun fertilizer ->
      fragment [
        ofStr " with "
        Icon.fertilizer fertilizer
      ])
  ]

module SummaryTable =
  let private toPrecision value =
    let rounded = round2 value
    if rounded = 0.0
    then sprintf "%.2e" value
    else string rounded

  let private formatNormalizationDivisor timeNorm normDivisor =
    $"{round2 normDivisor} {TimeNormalization.unit timeNorm |> pluralize}"

  let private formatNormalizedValue format normDivisor value =
    value |> Option.defaultOrMap "???" (fun value -> format (value / normDivisor))

  let private rowCells
    (itemCell: ReactElement)
    (unitValue: ReactElement)
    (quantity: ReactElement)
    (value: ReactElement)
    (seeds: ReactElement)
    =
    [|
      th [ scope "row"; children itemCell ]
      td unitValue
      td quantity
      td value
      td seeds
    |]

  let private significantRow seeds itemCell (price: (nat * bool) option) quantity =
    let profit = price |> Option.defaultOrMap 0.0 (fun (price, bought) ->
      round2 (float price * quantity * if bought then -1.0 else 1.0))

    if profit = 0.0 && round2 quantity = 0.0 then None else

    let profit = if price.IsNone then none else ofStr (gold2 profit)
    let price = price |> ofOption (fst >> gold >> ofStr)
    let quantity = if quantity = 0.0 then none else ofStr (toPrecision quantity)
    let seeds = if seeds then quantity else none

    Some (rowCells itemCell price quantity profit seeds)

  let private seedRow itemCell quantity = significantRow true itemCell None quantity
  let private profitRow itemCell price quantity = significantRow false itemCell (Some price) quantity

  let private unknownProfitRow seeds itemCell quantity =
    if quantity = 0.0 then None else
    let quantity = quantity |> toPrecision |> ofStr
    rowCells
      itemCell
      (ofStr "???")
      quantity
      (ofStr "???")
      (if seeds then quantity else none)
    |> Some

  let private boughtRow seeds priceAndVendor quantity (icon: ReactElement) =
    match priceAndVendor with
    | Some (vendor, price) ->
      let itemCell = fragment [
        icon
        match vendor with
        | NonCustom vendor ->
          ofStr " from "
          Icon.vendor vendor
        | Custom () -> ofStr " (Custom)"
      ]
      significantRow seeds itemCell (Some (price, true)) quantity

    | None ->
      let itemCell = fragment [
        icon
        ofStr " from ???"
      ]
      unknownProfitRow seeds itemCell quantity

  let private itemCellWithInputs data (itemCell: ReactElement) inputs =
    details [ className Class.inputItems; children [
      summary [ itemCell ]
      ul (inputs |> Array.map (fun ((item, quality), quantity) ->
        let quantity = round2 quantity
        if quantity = 0.0 then none else
        li [
          Icon.itemIdQuality data item quality
          ofStr $" x {quantity}"
        ]
      ))
    ]]

  let private fertilizerBoughtRow replacement fertilizer price quantity =
    match fertilizer, price with
    | Some fertilizer, Some price ->
      boughtRow false price quantity (fragment [
        if replacement then ofStr "Replacement "
        Icon.fertilizer fertilizer
      ])
    | _ -> None

  let private harvestedSeedsRows data item quantities =
    quantities
    |> Qualities.indexed
    |> Array.choose (fun (quality, quantity) ->
      seedRow (Icon.itemIdQuality data item quality) quantity)

  let private seedMakerRows data crop item quantities =
    let seed = Crop.seed crop
    let itemCell =
      quantities
      |> Qualities.indexed
      |> Array.map (fun (quality, quantity) -> (item, quality), quantity)
      |> itemCellWithInputs data (Icon.seed data seed)

    let seedQuantity = Processor.seedMakerExpectedQuantity seed * Qualities.sum quantities

    seedRow itemCell seedQuantity |> Option.toArray

  let private forageSeedsRows data settings items seed quantitySold quantityUsed =
    if quantitySold = 0.0 && quantityUsed = 0.0 then Array.empty else
    let itemCell = Icon.seed data seed
    let inputsCell quantity =
      let quantity = quantity / float ForageCrop.forageSeedsPerCraft
      items
      |> Array.map (fun item -> (item, Quality.Normal), quantity)
      |> itemCellWithInputs data itemCell

    let price = Game.seedItemSellPrice data settings.Game seed
    Array.collect Option.toArray [|
      seedRow (inputsCell quantityUsed) quantityUsed
      profitRow (inputsCell quantitySold) (price, false) quantitySold
    |]

  let private unsoldItemRow data (item, quantities) =
    let itemCell =
      quantities
      |> Qualities.indexed
      |> Array.map (fun (quality, quantity) -> (item, quality), quantity)
      |> itemCellWithInputs data (ofStr "???")

    rowCells itemCell none none none none

  let private soldItemRow data (summary: Query.SoldItemSummary) =
    let itemCell = fragment [
      Icon.itemIdQuality data summary.Item summary.Quality
      if summary.Custom then ofStr " (Custom)"
    ]

    profitRow itemCell (summary.Price, false) summary.Quantity

  let private soldProductRow data (summary: Query.SoldProductSummary) =
    let itemCell =
      itemCellWithInputs
        data
        (Icon.productQuality data summary.Product summary.Quality)
        summary.ConsumedItemQuantities

    profitRow itemCell (summary.Price, false) summary.Quantity

  let private cropProfitSummaryRows
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
        let seeds = -int (if Crop.regrows crop then 1u else summary.Harvests)
        let unit = "harvest" |> pluralizeTo summary.Harvests
        [| rowCells (ofStr $"{summary.Harvests} {unit}") none none none (ofInt seeds) |]

      boughtRow true summary.SeedPrice summary.SeedsBought (Icon.seed data seed) |> Option.toArray

      summary.SeedsUsed |> Array.collect (fun (item, quantities) ->
        if item = Crop.seedItem crop then harvestedSeedsRows data item quantities
        elif item = Crop.mainItem crop then seedMakerRows data crop item quantities
        else assert false; Array.empty)

      forageSeedsRows data settings (Crop.items crop) seed summary.ForageSeedsSold summary.ForageSeedsUsed

      summary.UnsoldItems |> Array.map (unsoldItemRow data)
      summary.SoldItems |> Array.choose (soldItemRow data)
      summary.SoldProducts |> Array.choose (soldProductRow data)

      fertilizerBoughtRow true fertilizer fertilizerPrice summary.ReplacedFertilizer |> Option.toArray
    |]
    |> Array.concat
    |> Array.map fragment

  let private footerProfitRowCells itemCell profit seeds =
    rowCells itemCell none none profit seeds

  let private totalRowCells totalFormat total (seeds: ReactElement) =
    footerProfitRowCells (ofStr "Total") (total |> Option.defaultOrMap "???" totalFormat |> ofStr) seeds

  let private thColWith (cell: ReactElement) = th [ scope "col"; children cell ]
  let private thCol = ofStr >> thColWith

  let private profitHeaderCells itemCell seeds =
    fragment [
      thCol itemCell
      thCol "Price"
      thCol "Quantity"
      thCol "Profit"
      thColWith (if seeds then ofStr "Seeds" else none)
    ]

  let private profitHeader itemCell seeds = thead [ tr [ profitHeaderCells itemCell seeds ] ]

  let private profitFooter timeNorm normDivisor profit seeds =
    tfoot [
      tr (totalRowCells gold2 profit seeds)

      if timeNorm = TotalPeriod || normDivisor = 1.0 then none else
        tr (footerProfitRowCells
          (TimeNormalization.unit timeNorm |> pluralize |> upperFirstChar |> ofStr)
          (ofStr $"{formatNormalizationDivisor timeNorm normDivisor}")
          none)

        tr (footerProfitRowCells
          (ofStr $"Total {timeNorm |> Reflection.getCaseName |> lowerCase}")
          (profit |> formatNormalizedValue gold2 normDivisor |> ofStr)
          none)
    ]

  let private rankerProfitSummary data settings timeNorm (profitSummary: Query.ProfitSummary) =
    table [
      profitHeader "Item" (settings.Profit.SeedStrategy <> IgnoreSeeds)

      tbody [
        fertilizerBoughtRow false profitSummary.Fertilizer profitSummary.FertilizerPrice 1.0 |> ofOption tr

        yield!
          profitSummary.CropSummaries[0]
          |> cropProfitSummaryRows data settings profitSummary.Fertilizer profitSummary.FertilizerPrice
          |> Array.map (Array.singleton >> tr)
      ]

      let seeds =
        if settings.Profit.SeedStrategy = IgnoreSeeds
        then none
        else ofNat 0u

      profitFooter timeNorm profitSummary.TimeNormalization profitSummary.NetProfit seeds
    ]

  let private optimizerProfitSummary data settings total (summaries: Query.ProfitSummary array) =
    let header = profitHeaderCells "Crop / Item" (settings.Profit.SeedStrategy <> IgnoreSeeds)

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
      fertilizerBoughtRow false profitSummary.Fertilizer profitSummary.FertilizerPrice 1.0
      |> Option.map (fun row -> "", fragment row, [||])
      |> Option.toArray

      profitSummary.CropSummaries |> Array.map (fun summary ->
        let key =
          let seed = Crop.seed summary.Crop
          let count = getCount seed
          string seed + "@" + string count

        let row =
          rowCells
            (fertilizerAndCrop data profitSummary.Fertilizer summary.Crop)
            none
            (ofNat summary.Harvests)
            (summary.NetProfit |> ofOption (gold2 >> ofStr))
            none
          |> fragment

        let body = cropProfitSummaryRows data settings profitSummary.Fertilizer profitSummary.FertilizerPrice summary

        key, row, body)
    |])

    table [
      Table.collapsibleHeaderAndBodies true header bodies
      tfoot [
        tr [
          td []
          yield! totalRowCells gold2 (Some total) none
        ]
      ]
    ]

  let private keyValue (key: string) (valueCell: ReactElement) =
    fragment [
      dt key
      dd valueCell
    ]

  let private normalizationRows key valueFormat timeNorm normDivisor value =
    if normDivisor = 1.0 then none else

    fragment [
      keyValue
        (TimeNormalization.unit timeNorm |> pluralize |> upperFirstChar)
        (formatNormalizationDivisor timeNorm normDivisor |> ofStr)

      keyValue
        $"{key} {timeNorm |> Reflection.getCaseName |> lowerCase}"
        (value |> formatNormalizedValue valueFormat normDivisor |> ofStr)
    ]

  let private roiSummary settings timeNorm (profitSummary: Query.ProfitSummary) =
    let investment, roi = profitSummary.InvestmentAndROI (settings.Profit.SeedStrategy = BuyFirstSeed)
    dl [
      keyValue "Investment" (investment |> Option.defaultOrMap "???" gold |> ofStr)
      keyValue "ROI" (roi |> Option.defaultOrMap "???" percent2 |> ofStr)
      normalizationRows "ROI" percent2 timeNorm profitSummary.TimeNormalization roi
    ]

  let private tooltipBoughtRow itemCell price quantity =
    (match price with
    | Some (_, price) -> profitRow itemCell (price, true) quantity
    | None -> unknownProfitRow false itemCell quantity)
    |> ofOption tr

  let private tooltipProfitRow itemCell price quantity = profitRow itemCell (price, false) quantity |> ofOption tr

  let private profitTooltip data settings timeNorm (profitSummary: Query.ProfitSummary) =
    let summary = profitSummary.CropSummaries[0]
    let seed = Crop.seed summary.Crop
    table [
      profitHeader "Item" false

      tbody [
        profitSummary.FertilizerPrice |> ofOption (fun price ->
          tooltipBoughtRow
            (profitSummary.Fertilizer |> ofOption Icon.fertilizer)
            price
            (1.0 + summary.ReplacedFertilizer))

        tooltipBoughtRow (Icon.seed data seed) summary.SeedPrice summary.SeedsBought

        tooltipProfitRow
          (Icon.seed data seed)
          (Game.seedItemSellPrice data settings.Game seed)
          summary.ForageSeedsSold

        fragment (summary.SoldItems |> Array.map (fun summary ->
          tooltipProfitRow (Icon.itemIdQuality data summary.Item summary.Quality) summary.Price summary.Quantity))

        fragment (summary.SoldProducts |> Array.map (fun summary ->
          tooltipProfitRow
            (Icon.productQuality data summary.Product summary.Quality)
            summary.Price
            summary.Quantity))
      ]

      profitFooter timeNorm profitSummary.TimeNormalization profitSummary.NetProfit none
    ]

  let private mergeCropHarvests (dateSpan: Optimizer.FertilizerDateSpan) =
    let regrowHarvests = dateSpan.RegrowCrop |> Option.map (fun (_, crop, harvests) -> FarmCrop crop, harvests)
    let cropHarvests = dateSpan.CropHarvests |> Array.map (fun (crop, harvests, _) -> crop, harvests)
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

    let optimizer data settings total dateSpans =
      let summaries = dateSpans |> Array.map (fun dateSpan ->
        mergeCropHarvests dateSpan |> Query.Optimizer.profitSummary data settings dateSpan.Fertilizer)

      optimizerProfitSummary data settings total summaries

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
    let private verticalSummary includeHarvests timeNorm (xpSummary: Query.XpSummary) =
      let summary = xpSummary.CropSummaries[0]
      dl [
        if includeHarvests then keyValue "Harvests" (ofNat summary.Harvests)
        keyValue "XP" (summary.XpPerItem |> xp |> ofStr)
        keyValue "Quantity" (summary.ItemQuantity |> round2 |> ofFloat)
        keyValue "Total XP" (xpSummary.Xp |> round2 |> xpFloat |> ofStr)
        normalizationRows "XP" (round2 >> xpFloat) timeNorm xpSummary.TimeNormalization (Some xpSummary.Xp)
      ]

    let ranker data settings timeNorm fertilizer crop =
      match Query.Ranker.xpSummary data settings timeNorm fertilizer crop with
      | Ok summary -> verticalSummary true timeNorm summary
      | Error e -> invalidReasons settings crop e

    let tooltip = verticalSummary false

    let private tableSummary data total (summaries: Query.XpSummary array) =
      table [
        thead [
          tr [
            thCol "Crop"
            thCol "Harvests"
            thCol "XP"
            thCol "Quantity"
            thCol "Total XP"
            thColWith none
          ]
        ]

        tbody (summaries |> Array.collect (fun xpSummary ->
          xpSummary.CropSummaries |> Array.map (fun summary ->
            tr [
              th [ scope "row"; children (fertilizerAndCrop data xpSummary.Fertilizer summary.Crop) ]
              td (ofNat summary.Harvests)
              td (summary.XpPerItem |> xp |> ofStr)
              td (summary.ItemQuantity |> round2 |> ofFloat)
              td (xpSummary.Xp |> round2 |> xpFloat |> ofStr)
              td []
            ])
        ))

        tfoot [
          tr [
            th "Total"
            td []
            td []
            td []
            td (total |> round2 |> xpFloat)
            td []
          ]
        ]
      ]

    let optimizer data settings total dateSpans =
      dateSpans
      |> Array.map (fun dateSpan ->
        mergeCropHarvests dateSpan |> Query.Optimizer.xpSummary data settings dateSpan.Fertilizer)
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
    fertilizers |> Array.map (fun fert -> (Crop.seed crop, Fertilizer.Opt.name fert), profit fert))

  {
    Crops = crops
    Fertilizers = fertilizers
    Pairs = data
  }

let private emptyPairData (pairData: PairData) =
  let noCrops = Array.isEmpty pairData.Crops
  let noFerts = Array.isEmpty pairData.Fertilizers
  ul [ className [ Class.messages; Class.messagesLarge ]; children [
    if noCrops then li (Icon.warning "No crops selected.")
    if noFerts then li (Icon.warning "No fertilizers selected.")
    if not (noCrops || noFerts) then li (Icon.warning "No valid entries.")
  ]]

let rankBy (label: ReactElement) (metric: RankMetric) (timeNorm: TimeNormalization) dispatchMetric dispatchTimeNorm =
  div [
    label
    labeledHidden "Metric" (Select.unitUnion (length.em 3) metric dispatchMetric)
    labeledHidden "Time Normalization" (Select.unitUnion (length.em 6) timeNorm dispatchTimeNorm)
  ]


module Ranker =
  open Fable.Core.JsInterop
  open Feliz.Recharts

  type private Pairs = (SeedId * FertilizerName option) array

  let private iconSize = Values.fontPx * 1.5 |> round
  let private iconGap = Values.fontPx * 0.25 |> round |> max 1.0
  let private barGap = iconGap

  let private pairImage (pairs: Pairs) (data: GameData) (props: IXAxisTickProperties) =
    let index: int = props?payload?value
    let seed, fert = pairs[index]
    let item = Crop.mainItem data.Crops[seed]

    fragment [
      Svg.image [
        svg.href (Icon.Path.item item)
        svg.x (props.x - iconSize / 2.0)
        svg.y (props.y + iconGap)
        svg.width iconSize
        svg.height iconSize
        svg.children [ Svg.title (Item.name data.Items[item]) ]
      ]

      fert |> ofOption (fun fert ->
        Svg.image [
          svg.href (Icon.Path.fertilizer fert)
          svg.width iconSize
          svg.height iconSize
          svg.x (props.x - iconSize / 2.0)
          svg.y (props.y + iconSize + iconGap * 2.0)
          svg.children [ Svg.title fert ]
        ])
    ]

  let private harvestsText harvests =
    let unit = "harvest" |> pluralizeTo harvests
    Html.span $" ({harvests} {unit})"

  let private chartProfitTooltip data settings timeNorm roi fertilizer crop profit =
    fragment [
      match Query.Ranker.profitSummary data settings timeNorm fertilizer crop with
      | Some profitSummary ->
        harvestsText profitSummary.CropSummaries[0].Harvests

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
        match profit with
        | Ok _ -> assert false; none
        | Error e -> invalidReasons settings crop e
      ]

  let private chartXPTooltip data settings timeNorm fertilizer crop =
    fragment [
      match Query.Ranker.xpSummary data settings timeNorm fertilizer crop with
      | Ok summary ->
        harvestsText summary.CropSummaries[0].Harvests
        SummaryTable.XP.tooltip timeNorm summary
      | Error e -> invalidReasons settings crop e
    ]

  let private chartTooltip (pairs: Pairs) (data: GameData) settings timeNorm rankItem props =
    match props?payload with
    | Some (payload: _ array) when payload.Length > 0 && props?active ->
      let index, result = payload[0]?payload
      let seed, fertilizer = pairs[index]
      let crop = data.Crops[seed]
      let fertilizer = Option.map data.Fertilizers.Find fertilizer

      div [ prop.id "chart-tooltip"; children [
        h1 (fertilizerAndCrop data fertilizer crop)

        match rankItem with
        | Gold | ROI -> chartProfitTooltip data settings timeNorm (rankItem = ROI) fertilizer crop result
        | XP -> chartXPTooltip data settings timeNorm fertilizer crop
      ]]
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
      svg.onClick (fun _ -> selectPair i)
      svg.x x
      svg.y y
      svg.width width
      svg.height height
      svg.d (svgRectPath x y width height)
    ]

  let private errorBar data seedStrategy metric (pairs: Pairs) props =
    let x: float = props?x
    let y: float = props?y
    let width: float = props?width
    let height: float = props?height
    let index, value = props?payload
    let seed, fert = pairs[index]
    let cropDesc = Crop.name data.Items.Find data.Crops[seed]
    let fertDesc = fert |> Option.defaultOrMap "" (fun fert -> $" with {fert}")
    let valueDesc = value |> Option.ofResult |> Option.defaultOrMap "???" (fun value ->
      $"{round2 (value * if metric = ROI then 100.0 else 1.0)}{RankMetric.unit metric}")

    fragment [
      Svg.title $"{cropDesc}{fertDesc}: {valueDesc}"

      match value with
      | Ok _ ->
        Svg.path [
          svg.radius 0
          svg.x x
          svg.y y
          svg.width width
          svg.height height
          svg.d (svgRectPath x y width height)
          svg.tabIndex 0
          svg.onFocus ignore
        ]

      | Error (flags: Query.InvalidReasons) ->
        let gap = width / 4.0
        let maxErrors = 3
        let maxHeight = width * (float maxErrors) + gap * (float (maxErrors - 1))
        let y: float = props?background?height - maxHeight

        let errors = [
          if flags.HasFlag Query.InvalidReasons.NoFertilizerPrice then
            Icon.Path.fertilizer fert.Value, "No fertilizer price"

          if flags.HasFlag Query.InvalidReasons.NotEnoughSeeds then
            Icon.Path.item (toItem seed), if seedStrategy = BuyFirstSeed then "No seed price" else "No seed source"

          if flags.HasFlag Query.InvalidReasons.NotEnoughDays then
            "img/Time.png", "No harvests possible"

          if flags.HasFlag Query.InvalidReasons.NoInvestment then
            "img/Gold.png", "No investment"
        ]

        Svg.g [
          svg.tabIndex 0
          svg.onFocus ignore
          svg.children (errors |> List.mapi (fun i (path, alt) ->
            Svg.image [
              svg.href path
              svg.x x
              svg.y (y + (width + gap) * float (maxErrors - 1 - i))
              svg.width width
              svg.height width
              svg.children [ Svg.title alt ]
            ]
          ))
        ]
    ]

  let private chart ranker data settings pairs dispatch =
    let pairs, indexedValues =
      pairs
      |> Array.mapi (fun i (pair, value) -> pair, (i, value))
      |> Array.unzip

    let selectPair i =
      let crop, fert = pairs[i]
      (match ranker.RankItem with
      | RankCropsAndFertilizers -> (Some crop, Some fert)
      | RankCrops -> (Some crop, None)
      | RankFertilizers -> (None, Some fert))
      |> Some
      |> SetSelectedCropAndFertilizer
      |> dispatch

    Recharts.responsiveContainer [
      responsiveContainer.debounce 100
      responsiveContainer.chart (Recharts.barChart [
        barChart.data indexedValues
        barChart.barGap barGap
        barChart.margin (0, int Values.fontPx * 2, 0, 0)
        barChart.children [
          Recharts.tooltip [
            tooltip.content (chartTooltip pairs data settings ranker.TimeNormalization ranker.RankMetric)
          ]

          Recharts.bar [
            bar.dataKey (snd >> Result.defaultValue 0.0)
            bar.barSize (float iconSize * 1.5 |> round |> int)
            bar.onClick (fun props -> props?payload |> fst |> selectPair)
            Interop.mkBarAttr "shape" (errorBar data settings.Profit.SeedStrategy ranker.RankMetric pairs)
            Interop.mkBarAttr "background" (barBackground barGap selectPair)
          ]

          Recharts.yAxis [
            yAxis.unit (RankMetric.unit ranker.RankMetric)
            yAxis.domain (domain.constant 0, domain.auto)
            yAxis.width (Values.fontPx * 5.0)
            Interop.mkYAxisAttr "tickSize" (Values.fontPx / 2.0)
            if ranker.RankMetric = ROI then Interop.mkYAxisAttr "tickFormatter" (fun x -> x * 100.0)
          ]

          Recharts.xAxis [
            xAxis.dataKey (fst: _ -> int)
            xAxis.tick (pairImage pairs data)
            xAxis.interval 0
            xAxis.height (iconSize * 2.0 + iconGap * 3.0 + 10.0)
            Interop.mkXAxisAttr "tickSize" 0
          ]

          Recharts.brush [
            brush.startIndex (ranker.BrushSpan |> fst |> int |> clampIndex pairs)
            brush.endIndex (ranker.BrushSpan |> snd |> int |> clampIndex pairs)
            brush.height (int Values.fontPx * 2)
            Interop.mkBrushAttr "travellerWidth" (Values.fontPx / 2.0)
            Interop.mkBrushAttr "onChange" (fun i -> SetBrushSpan (i?startIndex, i?endIndex) |> dispatch)
          ]

        ]
      ])
    ]

  let ranker ranker (data, settings) dispatch =
    let pairData = pairData ranker.RankMetric ranker.TimeNormalization data settings
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

    pairs |> Array.sortInPlaceWith (fun a b ->
      match snd a, snd b with
      | Ok a, Ok b -> compare b a
      | Ok _, Error _ -> -1
      | Error _, Ok _ -> 1
      | Error a, Error b -> Enum.bitSetOrder (int a) (int b))

    fragment [
      div [ prop.id "chart-controls"; children [
        div [
          div [
            ofStr "Rank"
            labeledHidden "Rank Item" (Select.unitUnion (length.em 5) ranker.RankItem (SetRankItem >> dispatch))
          ]

          rankBy
            (ofStr "By")
            ranker.RankMetric
            ranker.TimeNormalization
            (SetRankMetric >> dispatch)
            (SetTimeNormalization >> dispatch)
        ]

        Input.checkbox "Show Invalid" ranker.ShowInvalid (SetShowInvalid >> dispatch)
      ]]

      if Array.isEmpty pairs then
        emptyPairData pairData
      else
        div [
          prop.id "chart"
          // rerender only if computed pairs or RankMetric changes
          children (lazyView3 (konst (chart ranker data settings)) ranker.RankMetric pairs dispatch)
        ]
    ]


let private selectSpecificOrBest name length toString (viewItem: _ -> ReactElement) items selected dispatch =
  Select.search
    length
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
  |> labeledHidden name

let [<ReactComponent>] CropAndFertilizerSummary (props: {|
    App: _
    Seed: _
    Fertilizer: _
    Dispatch: _
  |}) =
  let app = props.App
  let seed = props.Seed
  let fertName = props.Fertilizer

  let uiDispatch = props.Dispatch
  let dispatch = SetRanker >> props.Dispatch
  let selectDispatch = Some >> SetSelectedCropAndFertilizer >> dispatch
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

    let bestCrop = fertName |> Option.defaultOrMap bestCrop (fun fert ->
      let fert = Option.map data.Fertilizers.Find fert
      if Array.isEmpty pairData.Crops then None else
      pairData.Crops
      |> Array.maxBy (fun crop -> metricValue data settings timeNorm crop fert |> Option.ofResult)
      |> Some)

    let bestFert = seed |> Option.defaultOrMap bestFert (fun seed ->
      let crop = data.Crops[seed]
      if Array.isEmpty pairData.Fertilizers then None else
      let profit = metricValue data settings timeNorm crop
      pairData.Fertilizers
      |> Array.maxBy (profit >> Option.ofResult)
      |> Some)

    Choice2Of2 bestCrop, Choice2Of2 bestFert

  let cropOptions =
    pairData.Crops
    |> Array.sortBy (Settings.Crops.sortKey data)
    |> Array.map Choice1Of2
    |> Array.insertStart bestCrop

  let fertilizerOptions =
    // array.sort moves undefined elements to the end of the array even with a compare function
    // so we wrap with all elements with Some before sorting and then unwraping
    pairData.Fertilizers
    |> Array.map Some
    |> Array.sortBy (Option.get >> Fertilizer.Opt.name)
    |> Array.map (Option.get >> Choice1Of2)
    |> Array.insertStart bestFert

  let crop = seed |> Option.defaultOrMap bestCrop (data.Crops.Find >> Choice1Of2)
  let fert = fertName |> Option.defaultOrMap bestFert (Option.map data.Fertilizers.Find >> Choice1Of2)

  fragment [
    button [
      className Class.back
      onClick (fun _ -> SetSelectedCropAndFertilizer None |> dispatch)
      text "Back"
    ]

    div [ prop.id "summary-controls"; children [
      div [
        selectSpecificOrBest
          "Crop"
          (length.em 12)
          (Crop.name data.Items.Find)
          (Icon.crop data)
          cropOptions
          crop
          (fun opt ->
            let seed =
              match opt with
              | Choice1Of2 crop -> Some (Crop.seed crop)
              | Choice2Of2 _ -> None

            selectDispatch (seed, fertName))

        Html.span " with "

        selectSpecificOrBest
          "Fertilizer"
          (length.em 15)
          Fertilizer.Opt.displayName
          (Option.defaultOrMap (ofStr "No Fertilizer") Icon.fertilizer)
          fertilizerOptions
          fert
          (fun opt ->
            let fert =
              match opt with
              | Choice1Of2 fert -> Some (Fertilizer.Opt.name fert)
              | Choice2Of2 _ -> None

            selectDispatch (seed, fert))
      ]

      rankBy
        (Html.span "Show")
        metric
        timeNorm
        (fun metric -> setState (metric, timeNorm))
        (fun timeNorm -> setState (metric, timeNorm))
    ]]

    match crop, fert with
    | (Choice1Of2 crop | Choice2Of2 (Some crop)), (Choice1Of2 fert | Choice2Of2 (Some fert)) ->
      detailsSection
        ui.OpenDetails
        OpenDetails.RankerSummary
        (ofStr "Summary")
        (match metric with
          | Gold -> SummaryTable.Profit.ranker data settings timeNorm fert crop
          | ROI -> SummaryTable.ROI.ranker data settings timeNorm fert crop
          | XP -> SummaryTable.XP.ranker data settings timeNorm fert crop)
        uiDispatch

      detailsSection
        ui.OpenDetails
        OpenDetails.RankerGrowthCalendar
        (ofStr "Growth Calendar")
        (GrowthCalendar.ranker data settings fert crop)
        uiDispatch

    | _ -> emptyPairData pairData
  ]


let rankerOrSummary app dispatch =
  let settings, ui = app.State
  let ranker = ui.Ranker
  match ranker.SelectedCropAndFertilizer with
  | Some (crop, fert) -> CropAndFertilizerSummary {| App = app; Seed = crop; Fertilizer = fert; Dispatch = dispatch |}
  | None -> lazyView3 Ranker.ranker ranker (app.Data, settings) (SetRanker >> dispatch)


let private workerQueue, private workerSubscribe = Optimizer.createWorker ()

let private optimizerSummary openDetails data settings dispatch objective solution =
  fragment [
    match solution with
    | Some solution ->
      detailsSection
        openDetails
        OpenDetails.OptimizerSummary
        (ofStr "Summary")
        (lazyView (fun (objective, (total, dateSpans)) ->
          match objective with
          | MaximizeGold -> SummaryTable.Profit.optimizer data settings total dateSpans
          | MaximizeXP -> SummaryTable.XP.optimizer data settings total dateSpans)
          (objective, solution))
        dispatch

      detailsSection
        openDetails
        OpenDetails.OptimizerGrowthCalendar
        (ofStr "Growth Calendar")
        (lazyView (fun dateSpans ->
          div [
            className Class.calendar
            children (dateSpans |> Array.collect (GrowthCalendar.optimizer data settings))
          ])
          (snd solution))
        dispatch

    | None -> ofStr "Loading..."
  ]

let [<ReactComponent>] Optimizer (props: {|
    OpenDetails: OpenDetails Set
    Data: GameData
    Settings: Settings
    Objective: OptimizationObjective
    Dispatch: UIMessage -> unit
  |}) =
  let data = props.Data
  let settings = props.Settings
  let objective = props.Objective

  let (solution, solving), setState = useState ((None, false))

  useEffect ((fun () -> workerSubscribe (fun solution -> setState (Some solution, false))), [||])

  useEffect ((fun () ->
    setState (solution, true)
    workerQueue data settings objective
  ), [| box data; settings; objective |])

  div [
    if solving then className Class.disabled
    children (optimizerSummary props.OpenDetails data settings props.Dispatch objective solution)
  ]

let section app dispatch =
  let settings, ui = app.State
  let uiDispatch = SetUI >> dispatch

  section [
    prop.id "visualization"
    if ui.Mode = Ranker && ui.Ranker.SelectedCropAndFertilizer.IsNone then className "visualization-chart"
    children [
      tabs "Visualization" ui.Mode (SetAppMode >> uiDispatch) (function
        | Ranker -> rankerOrSummary app uiDispatch
        | Optimizer ->
          fragment [
            Select.unitUnion (length.em 3) ui.OptimizationObjective (SetOptimizationObjective >> uiDispatch)
            |> labeled "Maximize"

            Optimizer {|
              OpenDetails = ui.OpenDetails
              Data = app.Data
              Settings = settings
              Objective = ui.OptimizationObjective
              Dispatch = SetUI >> dispatch
            |}
          ])
    ]
  ]
