module StardewValleyStonks.WebApp.Query

open StardewValleyStonks

/// Returns the DateSpan for this crop that has the most harvests,
/// and, in the case of a tie, the date span with the least days.
let bestGrowthSpan vars fert crop =
  let spans =
    if vars.Location = Farm
    then Crop.seasons crop
    else Seasons.All
    |> Date.spans vars.StartDate vars.EndDate

  if spans.Length = 0 then None else

  let stages, time = Game.growthTimeAndStages vars fert crop
  let span, harvests =
    spans
    |> Array.map (fun span -> span, Growth.harvestsWith (Crop.regrowTime crop) time span.TotalDays)
    |> Array.sortBy (fun (span, _) -> span.TotalDays)
    |> Array.maxBy snd

  if harvests = 0u then None else Some {
    Span = span
    Stages = stages
    GrowthTime = time
    Harvests = harvests
  }

let replacedFertilizerPerHarvest settings crop =
  if settings.Profit.ReplaceLostFertilizer
  then Game.fertilizerLossProb settings.Game crop
  else 0.0

let replacedFertilizerAmount settings crop (harvests: nat) =
  float (harvests - 1u) * replacedFertilizerPerHarvest settings crop

let fertilizerBought settings crop harvests =
  1.0 + replacedFertilizerAmount settings crop harvests

let seedPriceValueFromVendor data settings vendor seed =
  data.SeedPrices[seed].TryFind vendor |> Option.map (Game.seedPrice data settings.Game seed)

let customSellPriceValue quality (price, preserveQuality) =
  if preserveQuality
  then price |> withMultiplier Qualities.multipliers[quality]
  else price

let customSellPriceValueByQuality (price, preserveQuality) =
  if preserveQuality
  then Qualities.multipliers |> Qualities.map (fun mult -> price |> withMultiplier mult)
  else Qualities.create price

let private itemsHighest comparisonValue crop =
  Crop.items crop |> Array.mapReduce max comparisonValue

let cropItemsHighestRawPrice (data: GameData) vars crop quality =
  crop |> itemsHighest (fun item -> Game.itemPrice vars (Crop.isForage crop) data.Items[item] quality)

let cropItemsHighestProductPriceFrom data vars crop quality processor =
  crop |> itemsHighest (fun item ->
    data.Products[item].TryFind processor |> Option.map (Game.productPrice data vars item quality))

let cropItemsHighestProductNormalizedPriceFrom data vars crop quality processor =
  crop |> itemsHighest (fun item ->
    data.Products[item].TryFind processor |> Option.map (Game.productNormalizedPrice data vars item quality))

let cropItemsHighestCustomPrice model crop (quality: Quality) =
  crop |> itemsHighest (fun item ->
    model.CustomSellPrices.Values.TryFind (Crop.seed crop, item) |> Option.map (customSellPriceValue quality))


module Selected =
  let private mapSelected (table: Table<_,_>) selected = selected |> Seq.map table.Find

  let private mapSelectedPrices (prices: Table<_,_>) selected =
    selected |> Seq.map (fun vendor -> vendor, prices[vendor])

  let fertilizers data settings = settings.Selected.Fertilizers |> mapSelected data.Fertilizers

  let fertilizersOpt data settings =
    fertilizers data settings
    |> Seq.map Some
    |> Seq.append (if settings.Selected.NoFertilizer then [ None ] else [])

  let fertilizerVendorsAndPrices data settings fertilizer =
    settings.Selected.FertilizerPrices[fertilizer] |> mapSelectedPrices data.FertilizerPrices[fertilizer]

  let fertilizerPrices data settings fertilizer =
    settings.Selected.FertilizerPrices[fertilizer] |> mapSelected data.FertilizerPrices[fertilizer]

  let crops data settings = settings.Selected.Crops |> mapSelected data.Crops

  let inSeasonCrops data settings =
    crops data settings |> Game.inSeasonCrops settings.Game

  let cropHasPrice settings seed =
    not settings.Selected.SeedPrices[seed].IsEmpty
    || settings.Selected.CustomSeedPrices |> Selection.selectedValue seed |> Option.isSome

  let seedVendorsAndPrices data settings seed =
    settings.Selected.SeedPrices[seed] |> mapSelectedPrices data.SeedPrices[seed]

  let seedVendorsAndPriceValues data settings seed =
    seedVendorsAndPrices data settings seed |> Seq.map (fun (vendor, price) ->
      vendor, Game.seedPrice data settings.Game seed price)

  let seedPrices data settings seed =
    settings.Selected.SeedPrices[seed] |> mapSelected data.SeedPrices[seed]

  let seedPriceValues data settings seed =
    seedPrices data settings seed |> Seq.map (Game.seedPrice data settings.Game seed)

  let products data settings seed item =
    settings.Selected.Products.[seed, item] |> mapSelected data.Products[item]

  let unlockedProducts data settings seed item =
    products data settings seed item |> Seq.filter (Game.productUnlocked data settings.Game.Skills)

  let unlockedUseSeedsFromSeedMaker data settings crop =
    Processor.seedMaker |> Game.processorUnlocked data settings.Game.Skills
    && Crop.canGetOwnSeedsFromSeedMaker data.Items.Find crop
    && settings.Selected.UseSeedMaker.Contains (Crop.seed crop)

  let unlockedForageSeedsSellAndUse settings crop =
    let unlocked = ForageCrop.seedsRecipeUnlocked settings.Game.Skills crop
    if unlocked then
      settings.Selected.SellForageSeeds.Contains crop.Seed,
      settings.Selected.UseForageSeeds.Contains crop.Seed
    else
      false, false

  let internal useHarvestedSeeds settings (seed: SeedId) (item: ItemId) =
    nat seed = nat item && settings.Selected.UseHarvestedSeeds.Contains seed


module Price =
  let private minVendor (selectedPrices: GameData -> Settings -> _ -> _) data settings customPrices key =
    let price =
      let prices = selectedPrices data settings key |> Array.ofSeq
      if prices.Length = 0 then None else
      let vendor, price = Array.minBy snd prices
      Some (NonCustom vendor, price)

    let custom =
      customPrices
      |> Selection.selectedValue key
      |> Option.map (fun price -> Custom (), price)

    Option.merge (minBy snd) price custom

  let private minPrice selectedPriceValues data settings customPrices key =
    let prices = selectedPriceValues data settings key |> Array.ofSeq
    if prices.Length = 0 then None else Some (Array.min prices)
    |> Option.min (customPrices |> Selection.selectedValue key)

  let fertilizerMinVendorAndPrice data settings fertilizer =
    minVendor Selected.fertilizerVendorsAndPrices data settings settings.Selected.CustomFertilizerPrices fertilizer

  let fertilizerMinPrice data settings fertilizer =
    minPrice Selected.fertilizerPrices data settings settings.Selected.CustomFertilizerPrices fertilizer

  let seedMinVendorAndPrice data settings seed =
    minVendor Selected.seedVendorsAndPriceValues data settings settings.Selected.CustomSeedPrices seed

  let seedMinPrice data settings seed =
    minPrice Selected.seedPriceValues data settings settings.Selected.CustomSeedPrices seed

  open type Quality

  let private itemMaxProductPriceBy projection data settings seed item (quality: Quality) =
    let products = Selected.unlockedProducts data settings seed item |> Array.ofSeq
    if products.Length = 0 then None else
    products |> Array.mapReduce max (projection data settings.Game item quality) |> Some

  let itemMaxProductPrice data settings seed item quality = itemMaxProductPriceBy Game.productPrice data settings seed item quality
  let itemMaxProductPriceNormalized data settings seed item quality = itemMaxProductPriceBy Game.productNormalizedPrice data settings seed item quality

  let private itemMaxProductPriceByQualityBy projection data settings seed item =
    let prices =
      Selected.unlockedProducts data settings seed item
      |> Seq.map (projection data settings.Game item)
      |> Array.ofSeq
    if prices.Length = 0 then None else
    Qualities.init (fun quality -> prices |> Array.mapReduce max (Qualities.item quality)) |> Some

  let itemMaxProductPriceByQuality data settings seed item = itemMaxProductPriceByQualityBy Game.productPriceByQuality data settings seed item
  let itemMaxProductNormalizedPriceByQuality data settings seed item = itemMaxProductPriceByQualityBy Game.productNormalizedPriceByQuality data settings seed item

  let private itemSelectedRawPriceValue data settings seed item quality =
    if settings.Selected.SellRaw.Contains (seed, item)
    then Some (Game.itemPrice settings.Game (data.ForageCrops.ContainsKey seed) data.Items[item] quality)
    else None

  let private itemSelectedRawPriceValueByQuality data settings seed item =
    if settings.Selected.SellRaw.Contains (seed, item)
    then Some (Game.itemPriceByQuality settings.Game (data.ForageCrops.ContainsKey seed) data.Items[item])
    else None

  let private selectedCustomSellPriceValue selected seed item (quality: Quality) =
    selected.CustomSellPrices
    |> Selection.selectedValue (seed, item)
    |> Option.map (customSellPriceValue quality)

  let private selectedCustomSellPriceValueByQuality selected seed item =
    selected.CustomSellPrices
    |> Selection.selectedValue (seed, item)
    |> Option.map customSellPriceValueByQuality

  let itemMaxNormalizedPrice (data: GameData) settings seed item quality =
    itemSelectedRawPriceValue data settings seed item quality
    |> Option.map float
    |> Option.max (itemMaxProductPriceNormalized data settings seed item quality)
    |> Option.max (selectedCustomSellPriceValue settings.Selected seed item quality |> Option.map float)

  let itemMaxNormalizedPriceByQuality (data: GameData) settings seed item =
    itemSelectedRawPriceValueByQuality data settings seed item
    |> Option.map (Qualities.map float)
    |> Option.merge (Qualities.map2 max) (itemMaxProductNormalizedPriceByQuality data settings seed item)
    |> Option.merge (Qualities.map2 max) (selectedCustomSellPriceValueByQuality settings.Selected seed item |> Option.map (Qualities.map float))

  let private itemMaxProductAndPriceByQualityBy projection data settings seed item =
    let products =
      Selected.unlockedProducts data settings seed item
      |> Seq.map (fun product -> product, projection data settings.Game item product)
      |> Array.ofSeq
    if products.Length = 0 then None else
    Qualities.init (fun quality ->
      products |> Array.mapReduce (maxBy snd) (fun (product, prices: _ Qualities) -> product, prices[quality]))
    |> Some

  let itemMaxProductAndPriceByQuality data settings seed item =
    itemMaxProductAndPriceByQualityBy Game.productPriceByQuality data settings seed item

  let itemMaxProductAndNormalizedPriceByQuality data settings seed item =
    itemMaxProductAndPriceByQualityBy Game.productNormalizedPriceByQuality data settings seed item

  let itemMaxSellAsAndNormalizedPrice (data: GameData) settings seed item =
    let rawPrices =
      itemSelectedRawPriceValueByQuality data settings seed item
      |> Option.map (Qualities.map (fun price -> NonCustom None, float price))

    let customPrices =
      settings.Selected.CustomSellPrices
      |> Selection.selectedValue (seed, item)
      |> Option.map (fun custom -> customSellPriceValueByQuality custom |> Qualities.map (fun price -> Custom custom, float price))

    let productPrices =
      itemMaxProductAndNormalizedPriceByQuality data settings seed item
      |> Option.map (Qualities.map (fun (product, profit) -> NonCustom <| Some product, profit))

    let sellAs = Array.choose id [|
      rawPrices
      customPrices
      productPrices
    |]

    if sellAs.Length = 0 then None else
    Qualities.init (fun quality -> sellAs |> Array.mapReduce (maxBy snd) (Qualities.item quality)) |> Some


let fertilizerCost data settings fertilizer =
  if settings.Profit.PayForFertilizer
  then Price.fertilizerMinPrice data settings fertilizer
  else Some 0u

let fertilizerCostOpt data settings = Option.defaultOrMap (Some 0u) (fertilizerCost data settings)

// let productOutputQuality settings product quality = product |> Product.outputQuality settings.ModData quality

// let outputQuality settings product quality =
//   match product with
//   | NonCustom None -> quality
//   | NonCustom (Some product) -> productOutputQuality settings product quality
//   | Custom (_, preservesQuality) -> if preservesQuality then quality else Normal

let timeNormalizationDivisor (growthSpan: GrowthSpan) crop = function
  | TotalPeriod -> 1.0
  | PerDay -> Growth.daysNeededFor (Crop.regrowTime crop) growthSpan.GrowthTime growthSpan.Harvests |> float
  | PerSeason -> float growthSpan.Span.TotalDays / float Date.daysInSeason

type [<System.Flags>] InvalidReasons =
  | None              = 0b0000
  | NotEnoughDays     = 0b0001
  | NoFertilizerPrice = 0b0010
  | NotEnoughSeeds    = 0b0100
  | NoInvestment      = 0b1000
  | All               = 0b1111

type private IR = InvalidReasons

// assume that if crop regrows and any item = seed, that the harvested amount is >= 1.0
// this is always true for the main item, and is also true for the extra itme if amount is >= 1.0
// therefore, it is safe to use one harvest as the test for whether
// one seed can ever be created over any number of harvests of a regrow crop
let private farmCropMaxNonBoughtSeeds data settings crop =
  let seedsFromMainItem =
    let mainItemAmount = Qualities.sum (Game.farmCropMainItemAmountByQuality settings.Game None crop)
    if Selected.unlockedUseSeedsFromSeedMaker data settings (FarmCrop crop) then mainItemAmount * Processor.seedMakerExpectedAmount crop.Seed
    elif Selected.useHarvestedSeeds settings crop.Seed crop.Item then mainItemAmount
    else 0.0

  let seedsFromExtraItem =
    match crop.ExtraItem with
    | Some (item, amount) when Selected.useHarvestedSeeds settings crop.Seed item -> amount
    | _ -> 0.0

  seedsFromMainItem + seedsFromExtraItem

let private forageCropMaxNonBoughtSeeds data settings crop =
    let craftedAmount =
      if Selected.unlockedForageSeedsSellAndUse settings crop |> snd
      then float ForageCrop.forageSeedsPerCraft
      else 0.0

    let seedMakerAmount =
      if Selected.unlockedUseSeedsFromSeedMaker data settings (ForageCrop crop)
      then Processor.seedMakerExpectedAmount crop.Seed
      else 0.0

    let mainItemAmount = Qualities.sum (Game.forageCropItemAmountByQuality settings.Game crop)

    mainItemAmount * max craftedAmount seedMakerAmount

let private maxNonBoughtSeeds data settings crop =
  match crop with
  | FarmCrop crop -> farmCropMaxNonBoughtSeeds data settings crop
  | ForageCrop crop -> forageCropMaxNonBoughtSeeds data settings crop

let canMakeEnoughSeeds data settings crop =
  match settings.Profit.SeedStrategy with
  | IgnoreSeeds -> true
  | BuyFirstSeed -> Selected.cropHasPrice settings (Crop.seed crop)
  | StockpileSeeds ->
    Selected.cropHasPrice settings (Crop.seed crop)
    || maxNonBoughtSeeds data settings crop >= 1.0

type internal RegrowCropProfitData = {
  ProfitPerHarvest: float
  HarvestsForMinCost: nat
  MinCost: float
  Cost: nat -> float
}

module internal Profit =
  let private constantRegrowData profit seedCost = {
    ProfitPerHarvest = profit
    HarvestsForMinCost = 1u
    MinCost = float seedCost
    Cost = konst (float seedCost)
  }

  let private seedOpportunityCostsAndSeedsPerHarvest data settings seedPrice crop (profits: _ array) (amounts: _ array) =
    let seed = Crop.seed crop
    let costsAndAmounts = ResizeArray ()
    let seedPriceValue = seedPrice |> Option.defaultOrMap System.Double.PositiveInfinity float
    let addCostAndAmount (profits: _ Qualities) (amounts: _ Qualities) seedAmount =
      for i = 0 to Quality.highest do
        let cost = profits[enum i] / seedAmount
        let amount = seedAmount * amounts[enum i]
        if cost < seedPriceValue && amount > 0.0 then
          costsAndAmounts.Add (cost, amount)

    if Selected.unlockedUseSeedsFromSeedMaker data settings crop then
      addCostAndAmount profits[0] amounts[0] (Processor.seedMakerExpectedAmount seed)

    Crop.items crop
    |> Array.tryFindIndex (Selected.useHarvestedSeeds settings seed)
    |> Option.iter (fun i -> addCostAndAmount profits[i] amounts[i] 1.0)

    costsAndAmounts.Sort (compareBy fst)
    resizeToArray costsAndAmounts

  let private seedCostCalc seedPrice (opportunityCostsAndSeedsPerHarvest: _ array) (harvests: nat) =
    let mutable seedsLeft = 1.0
    let mutable totalCost = 0.0
    let mutable i = 0

    while i < opportunityCostsAndSeedsPerHarvest.Length && seedsLeft > 0.0 do
      let cost, seedsPerHarvest = opportunityCostsAndSeedsPerHarvest[i]
      let seedsMade = seedsPerHarvest * float harvests |> min seedsLeft
      totalCost <- totalCost + cost * seedsMade
      seedsLeft <- seedsLeft - seedsMade
      i <- i + 1

    match seedPrice with
    | _ when seedsLeft = 0.0 -> Some totalCost
    | Some price -> Some (totalCost + float price * seedsLeft)
    | None -> None

  let private trySeedCost data settings seedPrice crop profits amounts harvests =
    let costs = seedOpportunityCostsAndSeedsPerHarvest data settings seedPrice crop profits amounts
    seedCostCalc seedPrice costs harvests

  let private farmCropTrySeedCost data settings seedPrice crop profits =
    if Option.isSome seedPrice || farmCropMaxNonBoughtSeeds data settings crop >= 1.0
    then Some (fun amounts harvests -> trySeedCost data settings seedPrice (FarmCrop crop) profits amounts harvests |> Option.get)
    else None

  let farmCropProfitPerHarvestCalc profits amounts = Array.map2Reduce (+) Qualities.dot profits amounts

  let forageCropProfitPerHarvestCalc profits amount = profits |> Array.sumBy (Qualities.dot amount)

  let private cropItemProfits data settings seed items = items |> Array.map (Price.itemMaxNormalizedPriceByQuality data settings seed >> Option.defaultValue Qualities.zero)

  let private farmCropItemProfits data settings crop = FarmCrop.items crop |> cropItemProfits data settings crop.Seed

type ItemUsage =
  | BoughtSeeds
  | SoldItem of ItemId * Quality
  | MadeSeeds of ItemId * Quality
  | MadeForageSeeds of ItemId * Quality
  | SoldForageSeeds
  | UsedForageSeeds

open YALPS
open YALPS.Operators

// This linear programming solution is perhaps only needed for the case when
// both forage seeds and the seedmaker are selected and unlocked.
// Since this solution also covers and solves all other cases for net profit as well,
// it is reused for simplicity and reducing code size.
  let forageCropNetProfitPerHarvestSolution
  data
  settings
  (crop: ForageCrop)
  seedTarget
  seedPrice
  sellForageSeeds
  useForageSeeds
  (profits: _ Qualities array)
  (amounts: _ Qualities)
  =
  let inline (@) a b = a + ("@" + b)

  let validQualities = Quality.all |> Array.filter (fun q -> amounts[q] > 0.0)
  let forageSeedAmounts = crop.Items |> Array.map (fun item -> string item @ "Forage Seeds")

  let constraints = Array.concat [|
    // x seeds are made
      [| "Seeds" === seedTarget |]

    // cannot use more than the harvested amount for each (item, quality)
    Array.allPairs crop.Items validQualities |> Array.map (fun (item, quality) ->
      string item @ string quality <== amounts[quality])

      // all items set aside to create forage seeds are all used to do so
    forageSeedAmounts |> Array.map (fun amount -> amount === 0.0)
  |]

  let variables = ResizeArray ()

  match seedPrice with
  | Some price when seedTarget > 0.0 -> variables.Add (BoughtSeeds, [| "Profit", -float price; "Seeds", 1.0 |])
  | _ -> ()

  for i = 0 to crop.Items.Length - 1 do
    let item = crop.Items[i]
    let profits = profits[i]
    let forageSeedAmount = forageSeedAmounts[i]
    for quality in validQualities do
      let oneItem = string item @ string quality, 1.0
      let itemQuality = item, quality
      variables.Add (SoldItem itemQuality, [| "Profit", profits[quality]; oneItem |])
      variables.Add (MadeForageSeeds itemQuality, [| forageSeedAmount, -1.0; oneItem |])

    if seedTarget > 0.0 && Selected.unlockedUseSeedsFromSeedMaker data settings (ForageCrop crop) then
      let item = crop.Items[0]
      let amount = Processor.seedMakerExpectedAmount crop.Seed
      for quality in validQualities do
        variables.Add (MadeSeeds (item, quality), [| "Seeds", amount; string item @ string quality, 1.0 |])

  let oneOfEachItem = forageSeedAmounts |> Array.map (fun amount -> amount, 1.0)

  if sellForageSeeds then
      variables.Add (SoldForageSeeds, oneOfEachItem |> Array.append [| "Profit", float (ForageCrop.forageSeedsPerCraft * Game.seedItemSellPrice data settings.Game crop.Seed) |])

  if seedTarget > 0.0 && useForageSeeds then
    variables.Add (UsedForageSeeds, oneOfEachItem |> Array.append [| "Seeds", float ForageCrop.forageSeedsPerCraft |])

  let solution = Solver.solve <| Model.create Maximize "Profit" constraints variables
  assert (solution.status = Infeasible || solution.status = Optimal)
  solution

  let private forageCropNetProfitPerHarvest data settings (crop: ForageCrop) seedPrice =
    let profits = crop.Items |> cropItemProfits data settings crop.Seed
    let amounts = Game.forageCropItemAmountByQuality settings.Game crop
  let sellForageSeeds, useForageSeeds = Selected.unlockedForageSeedsSellAndUse settings crop
    if not (sellForageSeeds || useForageSeeds) then
      trySeedCost data settings seedPrice (ForageCrop crop) profits [| amounts |] 1u |> Option.map (fun cost ->
        forageCropProfitPerHarvestCalc profits amounts - cost)
    else
      let solution = forageCropNetProfitPerHarvestSolution data settings crop 1.0 seedPrice sellForageSeeds useForageSeeds profits amounts
  assert (solution.status = Optimal)
  Some solution.result

  let private forageCropIgnoreSeedsProfitPerHarvest data settings (crop: ForageCrop) =
    let profits = crop.Items |> cropItemProfits data settings crop.Seed
    let amounts = Game.forageCropItemAmountByQuality settings.Game crop
  if Selected.unlockedForageSeedsSellAndUse settings crop |> fst then
      let solution = forageCropNetProfitPerHarvestSolution data settings crop 0.0 None true false profits amounts
    assert (solution.status = Optimal)
    solution.result
  else
      forageCropProfitPerHarvestCalc profits amounts

  let private farmCropIgnoreSeedsProfitPerHarvest data settings crop =
    let profits = farmCropItemProfits data settings crop
    fun fertilizer ->
      Game.farmCropItemAmountsByQuality settings.Game fertilizer crop
      |> farmCropProfitPerHarvestCalc profits

  let private cropIgnoreSeedsProfitPerHarvest data settings crop =
    match crop with
    | FarmCrop crop -> farmCropIgnoreSeedsProfitPerHarvest data settings crop
    | ForageCrop crop -> konst (forageCropIgnoreSeedsProfitPerHarvest data settings crop)

  let private cropProfitAndSeedPrice data settings crop =
    Price.seedMinPrice data settings (Crop.seed crop) |> Option.map (fun seedPrice ->
      let profitPerHarvest = cropIgnoreSeedsProfitPerHarvest data settings crop
      fun fertilizer -> profitPerHarvest fertilizer, seedPrice)

  let private cropNonRegrowStockpileSeedsProfitPerHarvest data settings crop =
    let seedPrice = Price.seedMinPrice data settings (Crop.seed crop)
  match crop with
  | FarmCrop crop ->
      let profits = farmCropItemProfits data settings crop
      farmCropTrySeedCost data settings seedPrice crop profits |> Option.map (fun seedCost fertilizer ->
      let amounts = Game.farmCropItemAmountsByQuality settings.Game fertilizer crop
        farmCropProfitPerHarvestCalc profits amounts - seedCost amounts 1u)
  | ForageCrop crop ->
      forageCropNetProfitPerHarvest data settings crop seedPrice |> Option.map konst

  let private cropRegrowStockpileSeedsProfitData data settings (crop: FarmCrop) =
    let seedPrice = Price.seedMinPrice data settings crop.Seed
    let profits = farmCropItemProfits data settings crop
    farmCropTrySeedCost data settings seedPrice crop profits |> Option.map (fun _ fertilizer ->
      let amounts = Game.farmCropItemAmountsByQuality settings.Game fertilizer crop
    let profit = farmCropProfitPerHarvestCalc profits amounts
      let costsAndLimits = seedOpportunityCostsAndSeedsPerHarvest data settings seedPrice (FarmCrop crop) profits amounts
      match Array.tryHead costsAndLimits with
      | None -> Option.get seedPrice |> constantRegrowData profit
      | Some (cost, limit) -> {
        ProfitPerHarvest = profit
        HarvestsForMinCost = 1.0 / limit |> ceil |> nat
      MinCost = cost
        Cost = seedCostCalc seedPrice costsAndLimits >> Option.get
      })

  let cropNonRegrowProfitPerHarvest data settings crop =
    match settings.Profit.SeedStrategy with
    | IgnoreSeeds -> Some (cropIgnoreSeedsProfitPerHarvest data settings crop)
    | StockpileSeeds -> cropNonRegrowStockpileSeedsProfitPerHarvest data settings crop
    | BuyFirstSeed -> cropProfitAndSeedPrice data settings crop |> Option.map (fun profitAndPrice fertilizer ->
      let profit, cost = profitAndPrice fertilizer
      profit - float cost)

  let cropRegrowProfitData data settings (crop: FarmCrop) =
  match settings.Profit.SeedStrategy with
    | IgnoreSeeds ->
      let profit = farmCropIgnoreSeedsProfitPerHarvest data settings crop
      Some (fun fertilizer -> constantRegrowData (profit fertilizer) 0u)
    | StockpileSeeds -> cropRegrowStockpileSeedsProfitData data settings crop
    | BuyFirstSeed -> cropProfitAndSeedPrice data settings (FarmCrop crop) |> Option.map (fun profitAndPrice fertilizer ->
      let profit, cost = profitAndPrice fertilizer
      constantRegrowData profit cost)

  let private cropIgnoreSeedsProfitData data settings crop =
    let profit = cropIgnoreSeedsProfitPerHarvest data settings crop
    fun fertilizer harvests -> profit fertilizer * float harvests

  let private cropStockpileSeedsProfitData data settings crop =
    let seedPrice = Price.seedMinPrice data settings (Crop.seed crop)
  match crop with
  | FarmCrop crop ->
      let profits = farmCropItemProfits data settings crop
      farmCropTrySeedCost data settings seedPrice crop profits |> Option.map (fun seedCost fertilizer harvests ->
      let amounts = Game.farmCropItemAmountsByQuality settings.Game fertilizer crop
      let profit = farmCropProfitPerHarvestCalc profits amounts
      let harvestForSeed, numSeeds =
        if crop.RegrowTime.IsSome
        then harvests, 1u
        else 1u, harvests
        let cost = seedCost amounts harvestForSeed
        profit * float harvests - cost * float numSeeds)
  | ForageCrop crop ->
      forageCropNetProfitPerHarvest data settings crop seedPrice |> Option.map (fun net _ harvests ->
        net * float harvests)

  let private cropBuyFirstSeedProfitData data settings crop seedPrice =
    match crop with
    | FarmCrop { RegrowTime = Some _ } -> cropIgnoreSeedsProfitData data settings crop
    | FarmCrop crop ->
      let profits = farmCropItemProfits data settings crop
      let seedCost = trySeedCost data settings (Some seedPrice) (FarmCrop crop) profits
        fun fertilizer harvests ->
          let amounts = Game.farmCropItemAmountsByQuality settings.Game fertilizer crop
          let profit = farmCropProfitPerHarvestCalc profits amounts
        let cost = seedCost amounts 1u |> Option.get
        profit * float harvests - cost * float (harvests - 1u)
    | ForageCrop crop ->
      let profit = forageCropIgnoreSeedsProfitPerHarvest data settings crop
      let net = forageCropNetProfitPerHarvest data settings crop (Some seedPrice) |> Option.get
      fun _ harvests -> profit + net * float (harvests - 1u)

  let cropProfitDataCalc mapOutput data settings timeNormalization crop =
    let seedPriceAndProfit =
  match settings.Profit.SeedStrategy with
      | IgnoreSeeds -> Some (0u, cropIgnoreSeedsProfitData data settings crop)
      | StockpileSeeds -> cropStockpileSeedsProfitData data settings crop |> Option.map (fun profit -> 0u, profit)
      | BuyFirstSeed -> Price.seedMinPrice data settings (Crop.seed crop) |> Option.map (fun price -> price, cropBuyFirstSeedProfitData data settings crop price)

    let data fertilizer =
      let growthSpan = bestGrowthSpan settings.Game fertilizer crop
      let fertCost = fertilizerCostOpt data settings (Fertilizer.Opt.name fertilizer)
      match growthSpan, seedPriceAndProfit, fertCost with
      | Some span, Some (seedPrice, profit), Some fertCost ->
        let fertilizerCost = float fertCost * fertilizerBought settings crop span.Harvests
        let divisor = timeNormalizationDivisor span crop timeNormalization
        let profit = profit fertilizer span.Harvests
        Ok (profit, float seedPrice + fertilizerCost, divisor)
      | _ ->
        ((if fertCost.IsNone then IR.NoFertilizerPrice else IR.None)
        ||| (if growthSpan.IsNone then IR.NotEnoughDays else IR.None)
        ||| (if seedPriceAndProfit.IsNone then IR.NotEnoughSeeds else IR.None))
        |> Error

    data >> mapOutput

let cropProfit = Profit.cropProfitDataCalc (Result.map (fun (profit, investment, timeNorm) ->
  (profit - investment) / timeNorm))

let cropROI = Profit.cropProfitDataCalc (Result.bind (fun (profit, investment, timeNorm) ->
    if investment = 0.0
  then Error IR.NoInvestment
  else Ok ((profit - investment) / investment * 100.0 / timeNorm)))

module internal XP =
  let cropNonRegrowXpPerHarvest data settings crop =
    if not <| canMakeEnoughSeeds data settings crop
    then None
    else Some (Game.xpPerHarvest data settings.Game crop)

  let cropRegrowXpData data settings (crop: FarmCrop) =
    let crop = FarmCrop crop
    if not <| canMakeEnoughSeeds data settings crop then None else Some {
      ProfitPerHarvest = Game.xpPerHarvest data settings.Game crop
      HarvestsForMinCost = 1u
      MinCost = 0.0
      Cost = konst 0.0
    }

let cropXP data settings timeNorm crop =
  let enoughSeeds = canMakeEnoughSeeds data settings crop
  fun fertilizer ->
    let hasFertPrice = fertilizerCostOpt data settings (Fertilizer.Opt.name fertilizer) |> Option.isSome
    match bestGrowthSpan settings.Game fertilizer crop with
    | Some span when hasFertPrice && enoughSeeds ->
      let xpPerHarvest = Game.xpPerHarvest data settings.Game crop
      let xp = float span.Harvests * xpPerHarvest
      let divisor = timeNormalizationDivisor span crop timeNorm
      Ok (xp / divisor)
    | span ->
      ((if not hasFertPrice then IR.NoFertilizerPrice else IR.None)
      ||| (if span.IsNone then IR.NotEnoughDays else IR.None)
      ||| (if not enoughSeeds then IR.NotEnoughSeeds else IR.None))
      |> Error






type HarvestsData = {
  GrowthSpan: GrowthSpan
  SeedPrice: (CustomChoice<Vendor, unit> * nat) option
  FertilizerPrice: (CustomChoice<Vendor, unit> * nat) option option
  SellAs: (CustomChoice<Product option, nat * bool> * float) Qualities option array
  SeedsBought: float
  FertilizerBought: float
  SoldAmounts: float Qualities array
  IntoSeedAmounts: (ItemId * float Qualities) array
  ForageSeedsSold: float
  ForageSeedsUsed: float
  NetProfit: float option
  TimeNormalization: float
} with
  member inline this.Investment buyFirstSeed =
    let fertPrice = this.FertilizerPrice |> Option.defaultOrMap (Some 0u) (Option.map snd)
    let seedPrice = if buyFirstSeed then this.SeedPrice |> Option.map snd else (Some 0u)
    Option.map2 (+) fertPrice seedPrice
  member inline this.ROI investment =
    this.NetProfit |> Option.bind (fun total ->
      if investment = 0u
      then None
      else Some ((total - float investment) / float investment * 100.0))

let private seedData
  data
  settings
  seedPrice
  (seed: SeedId)
  (items: ItemId array)
  (profits: float Qualities array)
  (amounts: float Qualities array)
  =
  let amountsUsed = ResizeArray ()

  let d = ResizeArray ()
  let seedPrice = seedPrice |> Option.defaultOrMap System.Double.PositiveInfinity float
  let addCostAndLimit item seedAmount =
    let profits = profits[item]
    let amounts = amounts[item]
    for i = 0 to Quality.highest do
      let cost = profits[enum i] / seedAmount
      let amount = amounts[enum i]
      if cost < seedPrice && amount > 0.0 then
        d.Add {|
          Index = amountsUsed.Count
          Quality = enum<Quality> i
          Cost = cost
          Amount = amount
          AmountUsedPerSeed = seedAmount
        |}
    amountsUsed.Add (item, Array.zeroCreate Quality.count)

  let seedItemIndex =
    if settings.Selected.UseHarvestedSeeds.Contains seed
    then items |> Array.findIndex (fun item -> nat item = nat seed) |> Some
    else None
  match seedItemIndex with
  | None -> ()
  | Some i -> addCostAndLimit i 1.0

  let useSeedMaker = Processor.seedMaker |> Game.processorUnlocked data settings.Game.Skills && settings.Selected.UseSeedMaker.Contains seed
  if useSeedMaker then addCostAndLimit 0 (Processor.seedMakerExpectedAmount (items[0] * 1u<_>))

  let data = resizeToArray d
  data |> Array.sortInPlaceWith (compareBy (fun data -> data.Cost))

  let mutable seedsLeft = 1.0
  let mutable i = 0
  while i < data.Length && seedsLeft > 0.0 do
    let data = data[i]
    let seedsMade = (data.Amount * data.AmountUsedPerSeed) |> min seedsLeft
    let _, usage = amountsUsed[data.Index]
    usage[int data.Quality] <- seedsMade / data.AmountUsedPerSeed
    seedsLeft <- seedsLeft - seedsMade
    i <- i + 1

  let sold = Array.copy amounts
  for item, usage in amountsUsed do
    let amount = amounts[item]
    let usage = Qualities.wrap usage
    sold[item] <- Qualities.map2 (-) amount usage
  let usage = amountsUsed |> resizeToArray |> Array.map (fun (item, usage) -> items[item], Qualities.wrap usage)

  {|
    Sold = sold
    IntoSeeds = usage
    SeedsBought = seedsLeft
  |}

open YALPS
open Profit

let cropProfitDataStockpileSeeds data settings timeNormalization crop fertilizer =
  match bestGrowthSpan settings.Game fertilizer crop with
  | None -> None
  | Some span ->
    let seed = Crop.seed crop
    let harvests = span.Harvests

    let seedPrice' = Price.seedMinVendorAndPrice data settings seed
    let seedPrice = seedPrice' |> Option.map snd

    let items = Crop.items crop
    let sellAs = items |> Array.map (Price.itemMaxSellAsAndNormalizedPrice data settings seed)
    let profits =
      sellAs |> Array.map (function
        | Some sellAs ->
          Qualities.init (fun q -> snd sellAs[q])
        | None -> Qualities.zerof)
    let amounts = Game.cropItemAmountsByQuality settings.Game fertilizer crop
    let fertCost' =
      if settings.Profit.PayForFertilizer
      then fertilizer |> Option.map (Fertilizer.name >> Price.fertilizerMinVendorAndPrice data settings)
      else None
    let fertCost = fertCost' |> Option.defaultOrMap (Some 0u) (Option.map snd)
    let fertilizerBought = fertilizerBought settings crop harvests
    let fertCost = fertCost |> Option.map (float >> (*) fertilizerBought)

    let soldAmounts, seedAmounts, seedsBought, forageSeedsSold, forageSeedsUsed, net =
      match crop with
      | ForageCrop c when Selected.unlockedForageSeedsSellAndUse settings c |> (fun (sell, useSeed) -> sell || useSeed) ->
        let sellForageSeeds, useForageSeeds = Selected.unlockedForageSeedsSellAndUse settings c
        let seedMaker = not useForageSeeds && Processor.seedMaker |> Game.processorUnlocked data settings.Game.Skills && settings.Selected.UseSeedMaker.Contains seed
        let maxSeeds =
          if seedPrice.IsSome || useForageSeeds
          then 1.0
          else min 1.0 (Qualities.sum amounts[0] * if seedMaker then Processor.seedMakerExpectedAmount seed else 0.0)

        let totalAmounts = amounts[0] |> Qualities.mult (float harvests)

        let solution = forageCropNetProfitPerHarvestSolution data settings c (maxSeeds * float harvests) seedPrice sellForageSeeds useForageSeeds profits totalAmounts
        assert (solution.status = Optimal)
        let getUsage (solution: YALPS.Solution<ItemUsage>) case =
          solution.variables
          |> Array.tryFind (fst >> (=) case)
          |> Option.defaultOrMap 0.0 snd

        let soldAmounts =
          let sold =
            solution.variables
              |> Array.choose (function
                | (SoldItem (item, quality), amount) -> Some (item, (quality, amount))
                | _ -> None)
              |> Array.groupBy fst
              |> Array.map (fun (item, usage) ->
                let amounts = Array.zeroCreate Quality.count
                for _, (quality, amount) in usage do
                  amounts[int quality] <- amount
                item, Qualities.wrap amounts)
              |> Table.ofSeq
          c.Items |> Array.map (fun item -> sold.TryFind item |> Option.defaultValue Qualities.zero)

        let seedAmounts =
          solution.variables
          |> Array.choose (function
            | (MadeSeeds (item, quality), amount) -> Some (item, (quality, amount))
            | _ -> None)
          |> Array.groupBy fst
          |> Array.map (fun (item, usage) ->
            let amounts = Array.zeroCreate Quality.count
            for _, (quality, amount) in usage do
              amounts[int quality] <- amount
            item, Qualities.wrap amounts)

        let seedsBought =
          if seedPrice.IsSome
          then getUsage solution BoughtSeeds
          else (1.0 - maxSeeds) * float harvests

        let net =
          Option.map2 (fun fertCost seedPrice ->
            solution.result - fertCost - float seedPrice)
            fertCost
            (if seedsBought = 0.0 then Some 0u else seedPrice)

        soldAmounts, seedAmounts, seedsBought,
        getUsage solution SoldForageSeeds * float ForageCrop.forageSeedsPerCraft,
        getUsage solution UsedForageSeeds * float ForageCrop.forageSeedsPerCraft,
        net
      | _ ->
        let totalAmounts = amounts |> Array.map (Qualities.mult (float harvests))
        let seedData = seedData data settings seedPrice seed items profits (if Crop.regrows crop then totalAmounts else amounts)
        let seedsBought, soldAmounts, seedAmounts =
          if Crop.regrows crop then
            seedData.SeedsBought, seedData.Sold, seedData.IntoSeeds
          else
            let scale = Qualities.mult (float harvests)
            seedData.SeedsBought * float harvests,
            seedData.Sold |> Array.map scale,
            seedData.IntoSeeds |> Array.map (fun (item, amounts) -> item, scale amounts)
        let net =
          Option.map2 (fun fertCost seedPrice ->
            farmCropProfitPerHarvestCalc profits soldAmounts
            - fertCost
            - seedsBought * float seedPrice)
            fertCost
            (if seedsBought = 0.0 then Some 0u else seedPrice)

        soldAmounts, seedAmounts, seedsBought, 0.0, 0.0, net

    Some {
      GrowthSpan = span
      NetProfit = net
      TimeNormalization = timeNormalizationDivisor span crop timeNormalization
      SoldAmounts = soldAmounts
      IntoSeedAmounts = seedAmounts
      SeedsBought = seedsBought
      FertilizerBought = fertilizerBought
      ForageSeedsSold = forageSeedsSold
      ForageSeedsUsed = forageSeedsUsed
      SeedPrice = seedPrice'
      FertilizerPrice = fertCost'
      SellAs = sellAs
    }

let cropProfitDataBuyFirstSeed data settings timeNormalization crop fertilizer =
  match bestGrowthSpan settings.Game fertilizer crop with
  | None -> None
  | Some span ->
    let seed = Crop.seed crop
    let harvests = span.Harvests

    let seedPrice' = Price.seedMinVendorAndPrice data settings seed
    let seedPrice = seedPrice' |> Option.map snd

    let items = Crop.items crop
    let sellAs = items |> Array.map (Price.itemMaxSellAsAndNormalizedPrice data settings seed)
    let profits =
      sellAs |> Array.map (function
        | Some sellAs ->
          Qualities.init (fun q -> snd sellAs[q])
        | None -> Qualities.zero)
    let amounts = Game.cropItemAmountsByQuality settings.Game fertilizer crop
    let fertCost' =
      if settings.Profit.PayForFertilizer
      then fertilizer |> Option.map (Fertilizer.name >> Price.fertilizerMinVendorAndPrice data settings)
      else None
    let fertCost = fertCost' |> Option.defaultOrMap (Some 0u) (Option.map snd)
    let fertBought = fertilizerBought settings crop harvests
    let fertCost = fertCost |> Option.map (float >> (*) fertBought)

    let soldAmounts, seedAmounts, seedsBought, forageSeedsSold, forageSeedsUsed, net =
      match crop with
      | ForageCrop c when Selected.unlockedForageSeedsSellAndUse settings c |> (fun (sell, useSeed) -> sell || useSeed) ->
        let sellForageSeeds, useForageSeeds = Selected.unlockedForageSeedsSellAndUse settings c
        let seedMaker = not useForageSeeds && Processor.seedMaker |> Game.processorUnlocked data settings.Game.Skills && settings.Selected.UseSeedMaker.Contains seed
        let maxSeeds =
          if seedPrice.IsSome
          then 1.0
          else min 1.0 (Qualities.sum amounts[0] * if useForageSeeds then float ForageCrop.forageSeedsPerCraft elif seedMaker then Processor.seedMakerExpectedAmount seed else 0.0)

        let h1 = float (harvests - 1u)
        let totalAmounts = amounts[0] |> Qualities.mult h1

        let solution = forageCropNetProfitPerHarvestSolution data settings c (maxSeeds * h1) seedPrice sellForageSeeds useForageSeeds profits totalAmounts
        assert (solution.status = Optimal)
        let getUsage (solution: YALPS.Solution<ItemUsage>) case =
          solution.variables
          |> Array.tryFind (fst >> (=) case)
          |> Option.defaultOrMap 0.0 snd

        let soldAmounts =
          let sold =
            solution.variables
              |> Array.choose (function
                | (SoldItem (item, quality), amount) -> Some (item, (quality, amount))
                | _ -> None)
              |> Array.groupBy fst
              |> Array.map (fun (item, usage) ->
                let amounts = Array.zeroCreate Quality.count
                for _, (quality, amount) in usage do
                  amounts[int quality] <- amount
                item, Qualities.wrap amounts)
              |> Table.ofSeq
          c.Items |> Array.map (fun item -> sold.TryFind item |> Option.defaultValue Qualities.zero)

        let seedAmounts =
          solution.variables
          |> Array.choose (function
            | (MadeSeeds (item, quality), amount) -> Some (item, (quality, amount))
            | _ -> None)
          |> Array.groupBy fst
          |> Array.map (fun (item, usage) ->
            let amounts = Array.zeroCreate Quality.count
            for _, (quality, amount) in usage do
              amounts[int quality] <- amount
            item, Qualities.wrap amounts)

        let seedsBought =
          if seedPrice.IsSome
          then getUsage solution BoughtSeeds + 1.0
          else (1.0 - maxSeeds) * h1 + 1.0

        let net =
          Option.map2 (fun fertCost seedPrice ->
            solution.result - fertCost - float seedPrice)
            fertCost
            seedPrice

        soldAmounts, seedAmounts, seedsBought,
        getUsage solution SoldForageSeeds * float ForageCrop.forageSeedsPerCraft,
        getUsage solution UsedForageSeeds * float ForageCrop.forageSeedsPerCraft,
        net
      | _ ->
        let totalAmounts = amounts |> Array.map (Qualities.mult (float harvests))
        let seedsBought, soldAmounts, seedAmounts =
          if Crop.regrows crop then
            1.0, totalAmounts, [| |]
          else
            let seedData = seedData data settings seedPrice seed items profits (if Crop.regrows crop then totalAmounts else amounts)
            let h1 = float (harvests - 1u)
            seedData.SeedsBought * h1 + 1.0,
            Array.map2
              (Qualities.map2 (fun sold amount -> sold * h1 + amount))
              seedData.Sold
              amounts,
            seedData.IntoSeeds |> Array.map (fun (item, amounts) -> item, amounts |> Qualities.mult h1)
        let net =
          Option.map2 (fun fertCost seedPrice ->
            farmCropProfitPerHarvestCalc profits soldAmounts
            - fertCost
            - seedsBought * float seedPrice)
            fertCost
            seedPrice

        soldAmounts, seedAmounts, seedsBought, 0.0, 0.0, net

    Some {
      GrowthSpan = span
      NetProfit = net
      TimeNormalization = timeNormalizationDivisor span crop timeNormalization
      SoldAmounts = soldAmounts
      IntoSeedAmounts = seedAmounts
      SeedsBought = seedsBought
      FertilizerBought = fertBought
      ForageSeedsSold = forageSeedsSold
      ForageSeedsUsed = forageSeedsUsed
      SeedPrice = seedPrice'
      FertilizerPrice = fertCost'
      SellAs = sellAs
    }

let cropProfitDataIgnoreSeeds data settings timeNormalization crop fertilizer =
  match bestGrowthSpan settings.Game fertilizer crop with
  | None -> None
  | Some span ->
    let seed = Crop.seed crop
    let harvests = span.Harvests

    let items = Crop.items crop
    let sellAs = items |> Array.map (Price.itemMaxSellAsAndNormalizedPrice data settings seed)
    let profits =
      sellAs |> Array.map (function
        | Some sellAs ->
          Qualities.init (fun q -> snd sellAs[q])
        | None -> Qualities.zero)
    let amounts = Game.cropItemAmountsByQuality settings.Game fertilizer crop
    let fertCost' =
      if settings.Profit.PayForFertilizer
      then fertilizer |> Option.map (Fertilizer.name >> Price.fertilizerMinVendorAndPrice data settings)
      else None
    let fertCost = fertCost' |> Option.defaultOrMap (Some 0u) (Option.map snd)
    let fertBought = fertilizerBought settings crop harvests
    let fertCost = fertCost |> Option.map (float >> (*) fertBought)

    let net, soldAmounts, forageSeedsSold =
      match crop with
      | ForageCrop c when Selected.unlockedForageSeedsSellAndUse settings c |> (fun (sell, useSeed) -> sell || useSeed) ->
        let sellForageSeeds, useForageSeeds = Selected.unlockedForageSeedsSellAndUse settings c
        let soldAmounts = amounts[0] |> Qualities.mult (float harvests)
        let solution = forageCropNetProfitPerHarvestSolution data settings c 0.0 None sellForageSeeds useForageSeeds profits soldAmounts
        assert (solution.status = Optimal)
        let getUsage (solution: YALPS.Solution<ItemUsage>) case =
          solution.variables
          |> Array.tryFind (fst >> (=) case)
          |> Option.defaultOrMap 0.0 snd

        let soldAmounts =
          let sold =
            solution.variables
              |> Array.choose (function
                | (SoldItem (item, quality), amount) -> Some (item, (quality, amount))
                | _ -> None)
              |> Array.groupBy fst
              |> Array.map (fun (item, usage) ->
                let amounts = Array.zeroCreate Quality.count
                for _, (quality, amount) in usage do
                  amounts[int quality] <- amount
                item, Qualities.wrap amounts)
              |> Table.ofSeq
          c.Items |> Array.map (fun item -> sold.TryFind item |> Option.defaultValue Qualities.zero)

        let net = fertCost |> Option.map (fun fertCost -> solution.result - fertBought * float fertCost)

        net,
        soldAmounts,
        getUsage solution SoldForageSeeds * float ForageCrop.forageSeedsPerCraft
      | _ ->
        let soldAmounts = amounts |> Array.map (Qualities.mult (float harvests))
        fertCost |> Option.map (fun fertCost -> farmCropProfitPerHarvestCalc profits soldAmounts - fertBought * float fertCost),
        soldAmounts,
        0.0

    Some {
      GrowthSpan = span
      NetProfit = net
      TimeNormalization = timeNormalizationDivisor span crop timeNormalization
      SoldAmounts = soldAmounts
      IntoSeedAmounts = [| |]
      SeedsBought = 0.0
      FertilizerBought = fertBought
      ForageSeedsSold = forageSeedsSold
      ForageSeedsUsed = 0.0
      SeedPrice = None
      FertilizerPrice = fertCost'
      SellAs = sellAs
    }

let cropProfitData data settings timeNormalization crop fertilizer =
  match settings.Profit.SeedStrategy with
  | IgnoreSeeds -> cropProfitDataIgnoreSeeds data settings timeNormalization crop fertilizer
  | StockpileSeeds -> cropProfitDataStockpileSeeds data settings timeNormalization crop fertilizer
  | BuyFirstSeed -> cropProfitDataBuyFirstSeed data settings timeNormalization crop fertilizer

let cropXpData data settings timeNorm crop fertilizer =
  let hasFertPrice = fertilizerCostOpt data settings (Fertilizer.Opt.name fertilizer) |> Option.isSome
  let enoughSeeds = canMakeEnoughSeeds data settings crop

  match bestGrowthSpan settings.Game fertilizer crop with
  | Some span when hasFertPrice && enoughSeeds ->
    let xpPerItem = Game.xpPerItem data crop
    let xpItemsPerHarvest = Game.xpItemsPerHarvest settings.Game crop
    Ok {|
      XpPerItem = xpPerItem
      ItemsPerHarvest = xpItemsPerHarvest
      Harvests = span.Harvests
      XpPerHarvest = xpItemsPerHarvest * float xpPerItem
      TimeNormalization = timeNormalizationDivisor span crop timeNorm
    |}
  | span ->
    ((if not hasFertPrice then IR.NoFertilizerPrice else IR.None)
    ||| (if span.IsNone then IR.NotEnoughDays else IR.None)
    ||| (if not enoughSeeds then IR.NotEnoughSeeds else IR.None))
    |> Error
