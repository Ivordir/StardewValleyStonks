module StardewValleyStonks.WebApp.Query

open StardewValleyStonks

type GrowthSpan = {
  StartSeason: Season
  EndSeason: Season
  TotalDays: nat
  GrowthTime: nat
  Harvests: nat
}

let private consecutiveInSeasonDays startDate endDate seasons =
  let nthSeason = Date.seasonSpan startDate endDate
  let days = Date.daySpan startDate endDate

  let rec nextSeason start finish totalDays list =
    if finish = nthSeason.Length then
      if totalDays = 0u then list else ((nthSeason[start], nthSeason[finish - 1]), totalDays) :: list
    elif seasons |> Seasons.contains nthSeason[finish] then
      nextSeason start (finish + 1) (totalDays + days[finish]) list
    else
      nextSeason (finish + 1) (finish + 1) 0u (if totalDays = 0u then list else ((nthSeason[start], nthSeason[finish - 1]), totalDays) :: list)

  let consecutiveDays = nextSeason 0 0 0u []
  // 4 seasons can be splitted into at most 2 groups of consecutive seasons
  assert (consecutiveDays.Length <= 2)
  Array.ofList consecutiveDays

/// Returns the GrowthSpan for this crop that has the most harvests,
/// and, in the case of a tie, the date span with the least days.
let bestGrowthSpan vars fertilizer crop =
  let consecutiveDays =
    if vars.Location = Farm
    then crop |> Crop.seasons |> consecutiveInSeasonDays vars.StartDate vars.EndDate
    else [| (vars.StartDate.Season, vars.EndDate.Season), Date.totalDays vars.StartDate vars.EndDate |]

  if Array.isEmpty consecutiveDays then None else

  let time = Game.growthTime vars fertilizer crop

  let ((startSeason, endSeason), totalDays), harvests =
    consecutiveDays |> Array.sortInPlaceBy snd // take least days if tie
    consecutiveDays |> Array.mapReduce (maxBy snd) (fun (span, days) ->
      (span, days), Growth.maxHarvests (Crop.regrowTime crop) time days)

  if harvests = 0u then None else Some {
    StartSeason = startSeason
    EndSeason = endSeason
    TotalDays = totalDays
    GrowthTime = time
    Harvests = harvests
  }

let timeNormalizationDivisor growthSpan crop = function
  | TotalPeriod -> 1.0
  | PerDay -> Growth.daysNeededFor (Crop.regrowTime crop) growthSpan.GrowthTime growthSpan.Harvests |> float
  | PerSeason -> float growthSpan.TotalDays / float Date.daysInSeason

let replacedFertilizerPerHarvest settings crop =
  if settings.Profit.ReplaceLostFertilizer
  then Game.fertilizerLossProb settings.Game crop
  else 0.0

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
  crop |> Crop.items |> Array.mapReduce max comparisonValue

let cropItemsHighestRawPrice (data: GameData) vars crop quality =
  crop |> itemsHighest (fun item -> Game.itemPrice vars (Crop.isForage crop) data.Items[item] quality)

let cropItemsHighestProductPriceFrom data vars crop quality processor =
  crop |> itemsHighest (fun item ->
    GameData.product data item processor |> Option.map (Game.productPrice data vars quality))

let cropItemsHighestProductNormalizedPriceFrom data vars crop quality processor =
  crop |> itemsHighest (fun item ->
    GameData.product data item processor |> Option.map (Game.productNormalizedPrice data vars quality))

let cropItemsHighestCustomPrice data crop quality =
  crop |> itemsHighest (fun item ->
    data.CustomSellPrices.Values.TryFind (Crop.seed crop, item) |> Option.map (customSellPriceValue quality))


module Selected =
  let private mapSelected (table: Table<_,_>) selected = selected |> Seq.map table.Find

  let private mapSelectedPrices (prices: Table<_,_>) selected =
    selected |> Seq.map (fun vendor -> vendor, prices[vendor])

  let fertilizers data settings = settings.Selected.Fertilizers |> mapSelected data.Fertilizers

  let fertilizersOpt data settings =
    fertilizers data settings
    |> Seq.map Some
    |> Seq.append [ if settings.Selected.NoFertilizer then None ]

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

  let unlockedProducts data settings seed item =
    let selected = settings.Selected.Products[seed, item]
    GameData.products data item |> Array.filter (fun product ->
      product |> Game.productUnlocked data settings.Game
      && selected.Contains (Product.processor product))

  let unlockedUseSeedsFromSeedMaker data settings crop =
    Processor.seedMaker |> Game.processorUnlocked data settings.Game
    && settings.Selected.UseSeedMaker.Contains (Crop.seed crop)
    && Crop.canGetOwnSeedsFromSeedMaker crop

  let unlockedForageSeedsSellAndUse settings crop =
    if ForageCrop.seedRecipeUnlocked settings.Game.Skills crop then
      settings.Selected.SellForageSeeds.Contains crop.Seed,
      settings.Selected.UseForageSeeds.Contains crop.Seed
    else
      false, false

  let unlockedForageSeedsAnyUsage settings crop =
    let sellForageSeeds, useForageSeeds = unlockedForageSeedsSellAndUse settings crop
    sellForageSeeds || useForageSeeds

  let internal useHarvestedSeeds settings (seed: SeedId) (item: ItemId) =
    nat seed = nat item && settings.Selected.UseHarvestedSeeds.Contains seed


module Price =
  let private minVendorAndPrice (selectedPrices: GameData -> Settings -> _ -> _) data settings customPrices key =
    let price =
      let prices = selectedPrices data settings key |> Array.ofSeq
      if Array.isEmpty prices then None else
      let vendor, price = Array.minBy snd prices
      Some (NonCustom vendor, price)

    let custom =
      customPrices
      |> Selection.selectedValue key
      |> Option.map (fun price -> Custom (), price)

    Option.merge (minBy snd) price custom

  let private minPrice selectedPriceValues data settings customPrices key =
    let prices = selectedPriceValues data settings key |> Array.ofSeq
    if Array.isEmpty prices
    then None
    else Some (Array.min prices)
    |> Option.min (customPrices |> Selection.selectedValue key)

  let fertilizerMinVendorAndPrice data settings fertilizer =
    minVendorAndPrice Selected.fertilizerVendorsAndPrices data settings settings.Selected.CustomFertilizerPrices fertilizer

  let fertilizerMinPrice data settings fertilizer =
    minPrice Selected.fertilizerPrices data settings settings.Selected.CustomFertilizerPrices fertilizer

  let seedMinVendorAndPrice data settings seed =
    minVendorAndPrice Selected.seedVendorsAndPriceValues data settings settings.Selected.CustomSeedPrices seed

  let seedMinPrice data settings seed =
    minPrice Selected.seedPriceValues data settings settings.Selected.CustomSeedPrices seed

  let private itemMaxProductPriceBy projection data settings seed item (quality: Quality) =
    let products = Selected.unlockedProducts data settings seed item
    if Array.isEmpty products then None else
    products |> Array.mapReduce max (projection data settings.Game quality) |> Some

  let itemMaxProductPrice data settings seed item quality = itemMaxProductPriceBy Game.productPrice data settings seed item quality
  let itemMaxProductPriceNormalized data settings seed item quality = itemMaxProductPriceBy Game.productNormalizedPrice data settings seed item quality

  let private itemMaxProductPriceByQualityBy projection data settings seed item =
    let prices = Selected.unlockedProducts data settings seed item |> Array.map (projection data settings.Game)
    if Array.isEmpty prices then None else
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

  let itemMaxNormalizedPrice data settings seed item quality =
    itemSelectedRawPriceValue data settings seed item quality
    |> Option.map float
    |> Option.max (itemMaxProductPriceNormalized data settings seed item quality)
    |> Option.max (selectedCustomSellPriceValue settings.Selected seed item quality |> Option.map float)

  let itemMaxNormalizedPriceByQuality data settings seed item =
    itemSelectedRawPriceValueByQuality data settings seed item
    |> Option.map (Qualities.map float)
    |> Option.merge (Qualities.map2 max) (itemMaxProductNormalizedPriceByQuality data settings seed item)
    |> Option.merge (Qualities.map2 max) (selectedCustomSellPriceValueByQuality settings.Selected seed item |> Option.map (Qualities.map float))

  let private itemMaxProductAndPriceByQualityBy projection data settings seed item =
    let products = Selected.unlockedProducts data settings seed item |> Array.map (fun product ->
      product, projection data settings.Game product)

    if Array.isEmpty products then None else
    Qualities.init (fun quality ->
      products |> Array.mapReduce (maxBy snd) (fun (product, prices: _ Qualities) -> product, prices[quality]))
    |> Some

  let itemMaxProductAndPriceByQuality data settings seed item =
    itemMaxProductAndPriceByQualityBy Game.productPriceByQuality data settings seed item

  let itemMaxProductAndNormalizedPriceByQuality data settings seed item =
    itemMaxProductAndPriceByQualityBy Game.productNormalizedPriceByQuality data settings seed item

  let itemMaxSellAsAndNormalizedPrice data settings seed item =
    let rawPrices =
      itemSelectedRawPriceValueByQuality data settings seed item
      |> Option.map (Qualities.map (fun price -> None, float price))

    let customPrices =
      settings.Selected.CustomSellPrices
      |> Selection.selectedValue (seed, item)
      |> Option.map (fun custom -> customSellPriceValueByQuality custom |> Qualities.map (fun price -> Some (Custom custom), float price))

    let productPrices =
      itemMaxProductAndNormalizedPriceByQuality data settings seed item
      |> Option.map (Qualities.map (fun (product, profit) -> Some (NonCustom product), profit))

    let sellAs = Array.choose id [|
      rawPrices
      customPrices
      productPrices
    |]

    if Array.isEmpty sellAs then None else
    Qualities.init (fun quality -> sellAs |> Array.mapReduce (maxBy snd) (Qualities.item quality)) |> Some


let fertilizerCost data settings fertilizer =
  if settings.Profit.PayForFertilizer
  then Price.fertilizerMinPrice data settings fertilizer
  else Some 0u

let fertilizerCostOpt data settings = Option.defaultOrMap (Some 0u) (fertilizerCost data settings)

[<System.Flags>]
type InvalidReasons =
  | None              = 0b0000
  | NotEnoughDays     = 0b0001
  | NoFertilizerPrice = 0b0010
  | NotEnoughSeeds    = 0b0100
  | NoInvestment      = 0b1000
  | All               = 0b1111

type private IR = InvalidReasons

let private invalidReasons hasOneHarvest hasFertilierPrice hasEnoughSeeds =
  ((if hasOneHarvest then IR.None else IR.NotEnoughDays)
  ||| (if hasFertilierPrice then IR.None else IR.NoFertilizerPrice)
  ||| (if hasEnoughSeeds then IR.None else IR.NotEnoughSeeds))
  |> Error

// Assume that if a crop regrows and any item = seed, then the harvested amount for that item is >= 1.0.
// This is always true for the main item, and assume that the extra item amount is >= 1.0 if it is the seed.
// Therefore, if a regrow crop has a seed source, then it can always make at least one seed (if there is at least one harvest).
let private farmCropMaxNonBoughtSeeds data settings crop =
  let seedsFromMainItem =
    let mainItemAmount = Game.farmCropMainItemAmount settings.Game crop
    if Selected.unlockedUseSeedsFromSeedMaker data settings (FarmCrop crop) then mainItemAmount * Processor.seedMakerExpectedQuantity crop.Seed
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
    then Processor.seedMakerExpectedQuantity crop.Seed
    else 0.0

  let mainItemAmount = Game.forageCropMainItemAmount settings.Game crop
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

  let private seedOpportunityCostsAndSeedsPerHarvest data settings seedPrice crop (prices: _ Qualities array) (amounts: _ Qualities array) =
    let seed = Crop.seed crop
    let seedCostsAndAmounts = ResizeArray ()
    let seedPriceValue = seedPrice |> Option.defaultOrMap System.Double.PositiveInfinity float
    let addCostAndAmount item seedsPerAmount =
      let prices = prices[item]
      let amounts = amounts[item]
      for i = 0 to Quality.highest do
        let cost = prices[enum i] / seedsPerAmount
        let seeds = amounts[enum i] * seedsPerAmount
        if cost < seedPriceValue && seeds > 0.0 then
          seedCostsAndAmounts.Add (cost, seeds)

    if Selected.unlockedUseSeedsFromSeedMaker data settings crop then
      addCostAndAmount 0 (Processor.seedMakerExpectedQuantity seed)

    crop
    |> Crop.items
    |> Array.tryFindIndex (Selected.useHarvestedSeeds settings seed)
    |> Option.iter (fun i -> addCostAndAmount i 1.0)

    seedCostsAndAmounts.Sort (compareBy fst)
    resizeToArray seedCostsAndAmounts

  let private seedCostCalc seedPrice (opportunityCostsAndSeedsPerHarvest: _ array) (harvests: nat) =
    let mutable seedsLeft = 1.0
    let mutable totalCost = 0.0
    let mutable i = 0

    while i < opportunityCostsAndSeedsPerHarvest.Length && seedsLeft > 0.0 do
      let cost, seedsPerHarvest = opportunityCostsAndSeedsPerHarvest[i]
      let seedsMade = min seedsLeft (seedsPerHarvest * float harvests)
      totalCost <- totalCost + cost * seedsMade
      seedsLeft <- seedsLeft - seedsMade
      i <- i + 1

    match seedPrice with
    | Some price -> Some (totalCost + float price * seedsLeft)
    | None when seedsLeft = 0.0 -> Some totalCost
    | None -> None

  let private trySeedCost data settings seedPrice crop prices amounts harvests =
    let costs = seedOpportunityCostsAndSeedsPerHarvest data settings seedPrice crop prices amounts
    seedCostCalc seedPrice costs harvests

  let private farmCropTrySeedCost data settings seedPrice crop prices =
    if Option.isSome seedPrice || farmCropMaxNonBoughtSeeds data settings crop >= 1.0
    then Some (fun amounts harvests -> trySeedCost data settings seedPrice (FarmCrop crop) prices amounts harvests |> Option.get)
    else None

  let profitCalc prices amounts = Array.map2Reduce (+) Qualities.dot prices amounts

  let private forageCropProfitPerHarvestCalc prices amount = prices |> Array.sumBy (Qualities.dot amount)

  let private cropItemNormalizedPricesByQuality data settings seed items =
    items |> Array.map (Price.itemMaxNormalizedPriceByQuality data settings seed >> Option.defaultValue Qualities.zero)

  let private farmCropItemNormalizedPricesByQuality data settings crop =
    crop |> FarmCrop.items |> cropItemNormalizedPricesByQuality data settings crop.Seed

  type Variable =
    | BoughtSeeds
    | SoldItem of ItemId * Quality
    | MadeSeeds of ItemId * Quality
    | UsedToCraftForageSeeds of ItemId * Quality
    | SoldForageSeeds
    | UsedForageSeeds

  open YALPS
  open YALPS.Operators

  // This linear programming solution is perhaps only needed for the case when
  // both forage seeds and the seedmaker are selected and unlocked.
  // Since this solution also covers and solves all other cases for net profit as well,
  // it is reused for simplicity and reducing code size.
  let forageCropNetProfitSolution data settings crop seedTarget seedPrice (prices: _ Qualities array) (amounts: _ Qualities) =
    let inline (@) a b = a + ("@" + b)

    let sellForageSeeds, useForageSeeds = Selected.unlockedForageSeedsSellAndUse settings crop

    let validQualities = Quality.all |> Array.filter (fun q -> amounts[q] > 0.0)

    // This is necessary, since any quality of an item can be used to craft forage seeds.
    // The only restriction is that one of each item is needed/consumed per craft.
    let allocatedToForageSeeds = crop.Items |> Array.map (fun item -> string item @ "Forage Seeds")

    let constraints = Array.concat [|
      // x seeds are made
      [| "Seeds" === seedTarget |]

      // cannot use more than the harvested amount for each (item, quality)
      Array.allPairs crop.Items validQualities |> Array.map (fun (item, quality) ->
        string item @ string quality <== amounts[quality])

      // all items set aside to craft forage seeds are all used to do so
      allocatedToForageSeeds |> Array.map (fun amount -> amount === 0.0)
    |]

    let variables = ResizeArray ()

    match seedPrice with
    | Some price when seedTarget > 0.0 -> variables.Add (BoughtSeeds, [| "Profit", -float price; "Seeds", 1.0 |])
    | _ -> ()

    for i = 0 to crop.Items.Length - 1 do
      let item = crop.Items[i]
      let prices = prices[i]
      let forageSeedsAllocation = allocatedToForageSeeds[i]
      for quality in validQualities do
        let oneItem = string item @ string quality, 1.0
        let itemQuality = item, quality
        variables.Add (SoldItem itemQuality, [| "Profit", prices[quality]; oneItem |])
        variables.Add (UsedToCraftForageSeeds itemQuality, [| forageSeedsAllocation, 1.0; oneItem |])

    if seedTarget > 0.0 && Selected.unlockedUseSeedsFromSeedMaker data settings (ForageCrop crop) then
      let item = crop.Items[0]
      let amount = Processor.seedMakerExpectedQuantity crop.Seed
      for quality in validQualities do
        variables.Add (MadeSeeds (item, quality), [| "Seeds", amount; string item @ string quality, 1.0 |])

    let oneOfEachItemUsed = allocatedToForageSeeds |> Array.map (fun amount -> amount, -1.0)

    if sellForageSeeds then
      variables.Add (SoldForageSeeds, oneOfEachItemUsed |> Array.append [| "Profit", float (ForageCrop.forageSeedsPerCraft * Game.seedItemSellPrice data settings.Game crop.Seed) |])

    if seedTarget > 0.0 && useForageSeeds then
      variables.Add (UsedForageSeeds, oneOfEachItemUsed |> Array.append [| "Seeds", float ForageCrop.forageSeedsPerCraft |])

    let solution = Solver.solve <| Model.create Maximize "Profit" constraints variables
    assert (solution.status = Optimal)
    solution

  let private forageCropNetProfitPerHarvest data settings (crop: ForageCrop) seedPrice =
    let prices = crop.Items |> cropItemNormalizedPricesByQuality data settings crop.Seed
    let amounts = Game.forageCropItemAmountByQuality settings.Game crop
    if Selected.unlockedForageSeedsAnyUsage settings crop then
      let solution = forageCropNetProfitSolution data settings crop 1.0 seedPrice prices amounts
      Some solution.result
    else
      trySeedCost data settings seedPrice (ForageCrop crop) prices [| amounts |] 1u |> Option.map (fun cost ->
        forageCropProfitPerHarvestCalc prices amounts - cost)

  let private forageCropIgnoreSeedsProfitPerHarvest data settings (crop: ForageCrop) =
    let prices = crop.Items |> cropItemNormalizedPricesByQuality data settings crop.Seed
    let amounts = Game.forageCropItemAmountByQuality settings.Game crop
    if Selected.unlockedForageSeedsSellAndUse settings crop |> fst then
      let solution = forageCropNetProfitSolution data settings crop 0.0 None prices amounts
      solution.result
    else
      forageCropProfitPerHarvestCalc prices amounts

  let private farmCropIgnoreSeedsProfitPerHarvest data settings crop =
    let prices = farmCropItemNormalizedPricesByQuality data settings crop
    fun fertilizer ->
      let amounts = Game.farmCropItemAmountsByQuality settings.Game fertilizer crop
      profitCalc prices amounts

  let private cropIgnoreSeedsProfitPerHarvest data settings crop =
    match crop with
    | FarmCrop crop -> farmCropIgnoreSeedsProfitPerHarvest data settings crop
    | ForageCrop crop -> konst (forageCropIgnoreSeedsProfitPerHarvest data settings crop)

  let private cropProfitAndSeedPrice data settings crop =
    Price.seedMinPrice data settings (Crop.seed crop) |> Option.map (fun seedPrice ->
      let profitPerHarvest = cropIgnoreSeedsProfitPerHarvest data settings crop
      fun fertilizer -> profitPerHarvest fertilizer, seedPrice)

  let private nonRegrowCropStockpileSeedsProfitPerHarvest data settings crop =
    let seedPrice = Price.seedMinPrice data settings (Crop.seed crop)
    match crop with
    | FarmCrop crop ->
      let prices = farmCropItemNormalizedPricesByQuality data settings crop
      farmCropTrySeedCost data settings seedPrice crop prices |> Option.map (fun seedCost fertilizer ->
        let amounts = Game.farmCropItemAmountsByQuality settings.Game fertilizer crop
        profitCalc prices amounts - seedCost amounts 1u)
    | ForageCrop crop ->
      forageCropNetProfitPerHarvest data settings crop seedPrice |> Option.map konst

  let private regrowDataStockpileSeeds data settings (crop: FarmCrop) =
    let seedPrice = Price.seedMinPrice data settings crop.Seed
    let prices = farmCropItemNormalizedPricesByQuality data settings crop
    farmCropTrySeedCost data settings seedPrice crop prices |> Option.map (fun _ fertilizer ->
      let amounts = Game.farmCropItemAmountsByQuality settings.Game fertilizer crop
      let profit = profitCalc prices amounts
      let costsAndLimits = seedOpportunityCostsAndSeedsPerHarvest data settings seedPrice (FarmCrop crop) prices amounts
      match Array.tryHead costsAndLimits with
      | None -> Option.get seedPrice |> constantRegrowData profit
      | Some (cost, limit) -> {
        ProfitPerHarvest = profit
        HarvestsForMinCost = 1.0 / limit |> ceil |> nat
        MinCost = cost
        Cost = seedCostCalc seedPrice costsAndLimits >> Option.get
      })

  let nonRegrowCropProfitPerHarvest data settings crop =
    match settings.Profit.SeedStrategy with
    | IgnoreSeeds -> Some (cropIgnoreSeedsProfitPerHarvest data settings crop)
    | StockpileSeeds -> nonRegrowCropStockpileSeedsProfitPerHarvest data settings crop
    | BuyFirstSeed -> cropProfitAndSeedPrice data settings crop |> Option.map (fun profitAndPrice fertilizer ->
      let profit, cost = profitAndPrice fertilizer
      profit - float cost)

  let regrowCropProfitData data settings crop =
    match settings.Profit.SeedStrategy with
    | IgnoreSeeds ->
      let profit = farmCropIgnoreSeedsProfitPerHarvest data settings crop
      Some (fun fertilizer -> constantRegrowData (profit fertilizer) 0u)
    | StockpileSeeds -> regrowDataStockpileSeeds data settings crop
    | BuyFirstSeed -> cropProfitAndSeedPrice data settings (FarmCrop crop) |> Option.map (fun profitAndPrice fertilizer ->
      let profit, seedPrice = profitAndPrice fertilizer
      constantRegrowData profit seedPrice)

  let private ignoreSeeds data settings crop =
    let profit = cropIgnoreSeedsProfitPerHarvest data settings crop
    fun fertilizer harvests -> profit fertilizer * float harvests

  let private stockpileSeeds data settings crop =
    let seedPrice = Price.seedMinPrice data settings (Crop.seed crop)
    match crop with
    | FarmCrop crop ->
      let prices = farmCropItemNormalizedPricesByQuality data settings crop
      farmCropTrySeedCost data settings seedPrice crop prices |> Option.map (fun seedCost fertilizer harvests ->
        let amounts = Game.farmCropItemAmountsByQuality settings.Game fertilizer crop
        let profit = profitCalc prices amounts
        let harvestForSeed, numSeeds =
          if crop.RegrowTime.IsSome
          then harvests, 1u
          else 1u, harvests
        let cost = seedCost amounts harvestForSeed
        profit * float harvests - cost * float numSeeds)
    | ForageCrop crop ->
      forageCropNetProfitPerHarvest data settings crop seedPrice |> Option.map (fun net _ harvests ->
        net * float harvests)

  let private buyFirstSeed data settings crop seedPrice =
    match crop with
    | FarmCrop { RegrowTime = Some _ } -> ignoreSeeds data settings crop
    | FarmCrop crop ->
      let prices = farmCropItemNormalizedPricesByQuality data settings crop
      let seedCost = trySeedCost data settings (Some seedPrice) (FarmCrop crop) prices
      fun fertilizer harvests ->
        let amounts = Game.farmCropItemAmountsByQuality settings.Game fertilizer crop
        let profit = profitCalc prices amounts
        let cost = seedCost amounts 1u |> Option.get
        profit * float harvests - cost * float (harvests - 1u)
    | ForageCrop crop ->
      let profit = forageCropIgnoreSeedsProfitPerHarvest data settings crop
      let net = forageCropNetProfitPerHarvest data settings crop (Some seedPrice) |> Option.get
      fun _ harvests -> profit + net * float (harvests - 1u)

  let mapData mapping data settings timeNormalization crop =
    let seedPriceAndProfit =
      match settings.Profit.SeedStrategy with
      | IgnoreSeeds -> Some (0u, ignoreSeeds data settings crop)
      | StockpileSeeds -> stockpileSeeds data settings crop |> Option.map (fun profit -> 0u, profit)
      | BuyFirstSeed ->
        Price.seedMinPrice data settings (Crop.seed crop) |> Option.map (fun price ->
          price, buyFirstSeed data settings crop price)

    let data fertilizer =
      let growthSpan = bestGrowthSpan settings.Game fertilizer crop
      let fertCost = fertilizerCostOpt data settings (Fertilizer.Opt.name fertilizer)
      match growthSpan, seedPriceAndProfit, fertCost with
      | Some span, Some (seedPrice, profit), Some fertCost ->
        let fertilizerCost = float fertCost * (1.0 + float (span.Harvests - 1u) * replacedFertilizerPerHarvest settings crop)
        let divisor = timeNormalizationDivisor span crop timeNormalization
        let profit = profit fertilizer span.Harvests
        Ok (profit, float seedPrice + fertilizerCost, divisor)
      | _ -> invalidReasons growthSpan.IsSome fertCost.IsSome seedPriceAndProfit.IsSome

    data >> mapping

let cropProfit = Profit.mapData (Result.map (fun (profit, investment, timeNorm) ->
  (profit - investment) / timeNorm))

let cropROI = Profit.mapData (Result.bind (fun (profit, investment, timeNorm) ->
  if investment = 0.0
  then Error IR.NoInvestment
  else Ok ((profit - investment) / investment * 100.0 / timeNorm)))

module internal XP =
  let nonRegrowCropXpPerHarvest data settings crop =
    if not <| canMakeEnoughSeeds data settings crop
    then None
    else Some (Game.xpPerHarvest data settings.Game crop)

  let regrowCropXpData data settings crop =
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
    | span -> invalidReasons span.IsSome hasFertPrice enoughSeeds

let rankValue = function
  | Gold -> cropProfit
  | ROI -> cropROI
  | XP -> cropXP


type SoldItemSummary = {
  Item: ItemId
  Quality: Quality
  Price: nat
  Quantity: float
  Custom: bool
}

type SoldProductSummary = {
  Product: Product
  Quality: Quality
  Price: nat
  Quantity: float
  InputItemQuantities: ((ItemId * Quality) * float) array
}

type CropProfitSummary = {
  Crop: Crop
  Harvests: nat
  ReplacedFertilizer: float
  SeedPrice: (CustomChoice<Vendor, unit> * nat) option
  UnsoldItems: (ItemId * float Qualities) array
  SoldItems: SoldItemSummary array
  SoldProducts: SoldProductSummary array
  SeedAmounts: (ItemId * float Qualities) array
  SeedsBought: float
  ForageSeedsSold: float
  ForageSeedsUsed: float
  NetProfit: float option
}

type ProfitSummary = {
  Fertilizer: Fertilizer option
  FertilizerPrice: (CustomChoice<Vendor, unit> * nat) option option
  CropSummaries: CropProfitSummary array
  NetProfit: float option
  TimeNormalization: float
} with
  member inline this.Investment buyFirstSeed =
    let fertPrice = this.FertilizerPrice |> Option.defaultOrMap (Some 0u) (Option.map snd)
    let seedPrice = if buyFirstSeed then this.CropSummaries[0].SeedPrice |> Option.map snd else (Some 0u)
    Option.map2 (+) fertPrice seedPrice
  member inline this.ROI investment =
    this.NetProfit |> Option.bind (fun total ->
      if investment = 0u
      then None
      else Some ((total - float investment) / float investment * 100.0))


type CropXpSummary = {
  Crop: Crop
  Harvests: nat
  XpPerItem: nat
  ItemQuantity: float
}

type XpSummary = {
  Fertilizer: Fertilizer option
  CropSummaries: CropXpSummary array
  Xp: float
  TimeNormalization: float
}

module internal ProfitSummary =
  open YALPS
  open Profit

  type private ItemAndSeedSummary = {
    Profit: float option
    SoldAmounts: float Qualities array
    SeedAmounts: (ItemId * float Qualities) array
    SeedsBought: float
    ForageSeedsSold: float
    ForageSeedsUsed: float
  }

  module private ItemAndSeedSummary =
    let zero = {
      Profit = Some 0.0
      SoldAmounts = [||]
      SeedAmounts = [||]
      SeedsBought = 0.0
      ForageSeedsSold = 0.0
      ForageSeedsUsed = 0.0
    }

  let private seedCostAndAmountData data settings crop (seedPrice: nat option) items (prices: _ Qualities array) (amounts: _ Qualities array) =
    let seedCostAndAmountData = ResizeArray ()
    let seedPriceValue = seedPrice |> Option.defaultOrMap System.Double.PositiveInfinity float
    let addCostAndAmount item seedsPerAmount =
      let prices = prices[item]
      let amounts = amounts[item]
      for i = 0 to Quality.highest do
        let quality = enum i
        let cost = prices[quality] / seedsPerAmount
        let amount = amounts[quality]
        if cost < seedPriceValue && amount > 0.0 then
          seedCostAndAmountData.Add {|
            ItemIndex = item
            Quality = quality
            HarvestedAmount = amount
            OpportunityCostPerSeed = cost
            SeedsPerAmount = seedsPerAmount
          |}

    let seed = Crop.seed crop
    if Selected.unlockedUseSeedsFromSeedMaker data settings crop then
      addCostAndAmount 0 (Processor.seedMakerExpectedQuantity seed)

    items
    |> Array.tryFindIndex (Selected.useHarvestedSeeds settings seed)
    |> Option.iter (fun i -> addCostAndAmount i 1.0)

    let seedCostAndAmountData = resizeToArray seedCostAndAmountData
    seedCostAndAmountData |> Array.sortInPlaceWith (compareBy (fun data -> data.OpportunityCostPerSeed))
    seedCostAndAmountData

  let private partitionAmountsForSeeds data settings crop seedPrice prices amounts harvests =
    let items = Crop.items crop
    let seedCostAndAmountData = seedCostAndAmountData data settings crop seedPrice items prices amounts

    let usedAmountsForSeeds = items |> Array.map (fun _ -> Array.zeroCreate Quality.count)
    let mutable seedsLeft = float (if Crop.regrows crop then 1u else harvests)
    let mutable i = 0
    while i < seedCostAndAmountData.Length && seedsLeft > 0.0 do
      let data = seedCostAndAmountData[i]
      let seedsMade = min seedsLeft (data.HarvestedAmount * data.SeedsPerAmount)
      usedAmountsForSeeds[data.ItemIndex][int data.Quality] <- seedsMade / data.SeedsPerAmount
      seedsLeft <- seedsLeft - seedsMade
      i <- i + 1

    let usedAmountsForSeeds = usedAmountsForSeeds |> Array.map Qualities.wrap
    let soldAmounts = (amounts, usedAmountsForSeeds) ||> Array.map2 (Qualities.map2 (-))
    let usedAmountsForSeeds = Array.zip items usedAmountsForSeeds |> Array.filter (fun (_, amountUsed) -> amountUsed <> Qualities.zero)
    soldAmounts, usedAmountsForSeeds, seedsLeft

  let private nonForageItemAndSeedSummary makeSeeds data settings crop seedPrice prices amounts harvests =
    let amounts =
      if harvests = 1u
      then amounts
      else amounts |> Array.map (Qualities.mult (float harvests))

    let soldAmounts, usedAmountsForSeeds, seedsBought =
      if makeSeeds
      then partitionAmountsForSeeds data settings crop seedPrice prices amounts harvests
      else amounts, [||], 0.0

    let profit = profitCalc prices soldAmounts
    let netProfit =
      match seedPrice with
      | Some price -> Some (profit - seedsBought * float price)
      | None when seedsBought = 0.0 -> Some profit
      | None -> None

    { ItemAndSeedSummary.zero with
        Profit = netProfit
        SoldAmounts = soldAmounts
        SeedAmounts = usedAmountsForSeeds
        SeedsBought = seedsBought
    }

  let private itemAmounts (harvests: nat) variableFilter (solution: Variable Solution) =
    solution.variables
    |> Array.choose variableFilter
    |> Array.groupBy fst
    |> Array.map (fun (item, usage) ->
      let amounts = Array.zeroCreate Quality.count
      for _, (quality, amount) in usage do
        amounts[int quality] <- amount * float harvests
      item, Qualities.wrap amounts)

  let private variableAmount variable (harvests: nat) (solution: Variable Solution) =
    let perHarvest =
      solution.variables
      |> Array.tryFind (fst >> (=) variable)
      |> Option.defaultOrMap 0.0 snd

    perHarvest * float harvests

  let private forageCropItemAndSeedSummary makeSeeds data settings crop seedPrice prices amounts harvests =
    let seedTarget =
      if makeSeeds
      then forageCropMaxNonBoughtSeeds data settings crop |> min 1.0
      else 0.0

    let solution = forageCropNetProfitSolution data settings crop seedTarget seedPrice prices amounts

    let soldAmounts = solution |> itemAmounts harvests (function
      | SoldItem (item, quality), amount -> Some (item, (quality, amount))
      | _ -> None)

    let soldAmounts = crop.Items |> Array.map (fun item ->
      soldAmounts
      |> Array.tryFind (fst >> (=) item)
      |> Option.defaultOrMap Qualities.zero snd)

    let seedAmounts = solution |> itemAmounts harvests (function
      | MadeSeeds (item, quality), amount -> Some (item, (quality, amount))
      | _ -> None)

    let seedsBought = solution |> variableAmount BoughtSeeds harvests
    let profit = solution.result * float harvests

    let seedsBought =
      match seedPrice with
      | Some _ -> seedsBought
      | None when seedsBought = 0.0 -> 0.0
      | None -> float harvests * (1.0 - seedTarget)

    let soldForageSeeds = (solution |> variableAmount SoldForageSeeds harvests) * float ForageCrop.forageSeedsPerCraft
    let usedForageSeeds = (solution |> variableAmount UsedForageSeeds harvests) * float ForageCrop.forageSeedsPerCraft

    assert
      let profit' = profitCalc prices soldAmounts + soldForageSeeds * float (Game.seedItemSellPrice data settings.Game crop.Seed)
      abs (profit - profit') < 1e-5

    assert
      not makeSeeds ||
      let seeds = seedsBought + usedForageSeeds + (seedAmounts |> Array.sumBy (snd >> Qualities.sum)) * Processor.seedMakerExpectedQuantity crop.Seed
      abs (seeds - float harvests) < 1e-5

    assert
      let usedForForageSeeds = solution |> itemAmounts harvests (function
        | UsedToCraftForageSeeds (item, quality), amount -> Some (item, (quality, amount))
        | _ -> None)
      let neededForForageSeeds = (soldForageSeeds + usedForageSeeds) / float ForageCrop.forageSeedsPerCraft
      usedForForageSeeds |> Array.forall (fun (_, usedAmounts) ->
        abs (neededForForageSeeds - Qualities.sum usedAmounts) < 1e-5)

    let profit =
      if not makeSeeds || seedTarget = 1.0
      then Some profit
      else None

    {
      Profit = profit
      SoldAmounts = soldAmounts
      SeedAmounts = seedAmounts
      SeedsBought = seedsBought
      ForageSeedsSold = soldForageSeeds
      ForageSeedsUsed = usedForageSeeds
    }

  let private soldItemsAndProducts data settings (items: _ array) (sellAs: _ Qualities option array) (soldAmounts: float Qualities array) =
    let unsoldItems = ResizeArray ()
    let soldItems = ResizeArray ()
    let soldProducts = ResizeArray ()

    for i = 0 to items.Length - 1 do
      let item = items[i]
      let amounts = soldAmounts[i]
      match sellAs[i] with
      | None -> unsoldItems.Add (item, amounts)
      | Some sellAs ->
        for i = 0 to Quality.highest do
          let quality = enum i
          let amount = amounts[quality]
          if amount = 0.0 then () else

          let sellAs, (price: float) = sellAs[quality]
          match sellAs with
          | None | Some (Custom _) ->
            soldItems.Add {
              Item = item
              Quality = quality
              Price = nat price
              Quantity = amount
              Custom = sellAs.IsSome
            }
          | Some (NonCustom product) ->
            let price, productQuality = Game.productPriceAndQuality data settings.Game quality product
            soldProducts.Add (((item, quality), amount), (product, productQuality, price))

    let soldProducts =
      soldProducts
      |> resizeToArray
      |> Array.groupBy snd
      |> Array.map (fun ((product, quality, price), items) ->
        let items = items |> Array.map fst
        let quantity = Product.quantityPerInput product * (items |> Array.sumBy snd)
        {
          Product = product
          Quality = quality
          Price = price
          Quantity = quantity
          InputItemQuantities = items
        })

    resizeToArray unsoldItems,
    resizeToArray soldItems,
    soldProducts

  let private harvestSummary forageItemAndSeedSummary nonForageItemAndSeedSummary data settings (fertilizerPrice: nat option) fertilizer lastCrop crop harvests =
    let seed = Crop.seed crop
    let seedVendorAndPrice = Price.seedMinVendorAndPrice data settings seed
    let seedPrice = seedVendorAndPrice |> Option.map snd
    let replacedFertilizerAmount = float (harvests - if lastCrop then 1u else 0u) * replacedFertilizerPerHarvest settings crop
    let sellAs = crop |> Crop.items |> Array.map (Price.itemMaxSellAsAndNormalizedPrice data settings seed)
    let prices = sellAs |> Array.map (Option.defaultOrMap Qualities.zero (Qualities.map snd))
    let summary =
      match crop with
      | ForageCrop crop when Selected.unlockedForageSeedsAnyUsage settings crop ->
        let amounts = Game.forageCropItemAmountByQuality settings.Game crop
        forageItemAndSeedSummary data settings crop seedPrice prices amounts harvests
      | _ ->
        let amounts = Game.cropItemAmountsByQuality settings.Game fertilizer crop
        nonForageItemAndSeedSummary data settings crop seedPrice prices amounts harvests

    let unsoldItems, soldItems, soldProducts = soldItemsAndProducts data settings (Crop.items crop) sellAs summary.SoldAmounts

    let netProfit =
      match summary.Profit, fertilizerPrice with
      | Some profit, None when replacedFertilizerAmount = 0.0 -> Some profit
      | Some profit, Some cost -> Some (profit - replacedFertilizerAmount * float cost)
      | _ -> None

    {
      Crop = crop
      Harvests = harvests
      ReplacedFertilizer = replacedFertilizerAmount
      SeedPrice = seedVendorAndPrice
      UnsoldItems = unsoldItems
      SoldItems = soldItems
      SoldProducts = soldProducts
      SeedAmounts = summary.SeedAmounts
      SeedsBought = summary.SeedsBought
      ForageSeedsSold = summary.ForageSeedsSold
      ForageSeedsUsed = summary.ForageSeedsUsed
      NetProfit = netProfit
    }

  let ignoreSeeds = harvestSummary (forageCropItemAndSeedSummary false) (nonForageItemAndSeedSummary false)

  let stockpileSeeds = harvestSummary (forageCropItemAndSeedSummary true) (nonForageItemAndSeedSummary true)

  let private buyFirstSeedItemSummary itemAndSeedSummary data settings crop seedPrice prices amounts harvests =
    let firstSummary = itemAndSeedSummary false data settings crop seedPrice prices amounts 1u
    let summary = itemAndSeedSummary true data settings crop seedPrice prices amounts (harvests - 1u)
    let profit = (seedPrice, firstSummary.Profit, summary.Profit) |||> Option.map3 (fun seedPrice firstProfit profit -> firstProfit + profit - float seedPrice)
    {
      Profit = profit
      SoldAmounts = (firstSummary.SoldAmounts, summary.SoldAmounts) ||> Array.map2 (Qualities.map2 (+))
      SeedAmounts = summary.SeedAmounts
      SeedsBought = summary.SeedsBought + 1.0
      ForageSeedsSold = firstSummary.ForageSeedsSold + summary.ForageSeedsSold
      ForageSeedsUsed = summary.ForageSeedsUsed
    }

  let buyFirstSeed =
    harvestSummary
      (buyFirstSeedItemSummary forageCropItemAndSeedSummary)
      (fun data settings crop seedPrice prices amounts harvests ->
        if Crop.regrows crop then
          let summary = nonForageItemAndSeedSummary false data settings crop seedPrice prices amounts harvests
          { summary with
              SeedsBought = 1.0
              Profit = (summary.Profit, seedPrice) ||> Option.map2 (fun profit seedPrice -> profit - float seedPrice)
          }
        else
          buyFirstSeedItemSummary nonForageItemAndSeedSummary data settings crop seedPrice prices amounts harvests)


let private cropXpSummary data settings crop harvests =
  let xpPerItem = Game.xpPerItem data crop
  let xpItemsPerHarvest = Game.xpItemsPerHarvest settings.Game crop
  {
    Crop = crop
    Harvests = harvests
    XpPerItem = xpPerItem
    ItemQuantity = xpItemsPerHarvest * float harvests
  }

module Ranker =
  let profitSummary data settings timeNormalization fertilizer crop =
    let harvestsSummary =
      match settings.Profit.SeedStrategy with
      | IgnoreSeeds -> ProfitSummary.ignoreSeeds
      | StockpileSeeds -> ProfitSummary.stockpileSeeds
      | BuyFirstSeed -> ProfitSummary.buyFirstSeed

    bestGrowthSpan settings.Game fertilizer crop |> Option.map (fun span ->
      let fertilizerVendorAndPrice, fertCost =
        match fertilizer with
        | None -> None, Some 0u
        | Some _ when not settings.Profit.PayForFertilizer -> None, Some 0u
        | Some fertilizer ->
          let vendorAndPrice = fertilizer |> Fertilizer.name |> Price.fertilizerMinVendorAndPrice data settings
          Some vendorAndPrice, vendorAndPrice |> Option.map snd

      let summary = harvestsSummary data settings fertCost fertilizer true crop span.Harvests
      let net = (summary.NetProfit, fertCost) ||> Option.map2 (fun profit fertCost -> profit - float fertCost)

      {
        Fertilizer = fertilizer
        FertilizerPrice = fertilizerVendorAndPrice
        CropSummaries = [| summary |]
        NetProfit = net
        TimeNormalization = timeNormalizationDivisor span crop timeNormalization
      })

  let xpSummary data settings timeNorm fertilizer crop =
    let hasFertPrice = fertilizerCostOpt data settings (Fertilizer.Opt.name fertilizer) |> Option.isSome
    let enoughSeeds = canMakeEnoughSeeds data settings crop

    match bestGrowthSpan settings.Game fertilizer crop with
    | Some span when hasFertPrice && enoughSeeds ->
      let summary = cropXpSummary data settings crop span.Harvests
      Ok {
        Fertilizer = fertilizer
        CropSummaries = [| summary |]
        Xp = float summary.XpPerItem * summary.ItemQuantity
        TimeNormalization = timeNormalizationDivisor span crop timeNorm
      }
    | span -> invalidReasons span.IsSome hasFertPrice enoughSeeds


module Solver =
  let profitSummary data settings fertilizer cropHarvests =
    let fertilizerVendorAndPrice, fertCost =
      match fertilizer with
      | None -> None, Some 0u
      | Some _ when not settings.Profit.PayForFertilizer -> None, Some 0u
      | Some fertilizer ->
        let vendorAndPrice = fertilizer |> Fertilizer.name |> Price.fertilizerMinVendorAndPrice data settings
        Some vendorAndPrice, vendorAndPrice |> Option.map snd

    let harvestSummary =
      match settings.Profit.SeedStrategy with
      | IgnoreSeeds -> ProfitSummary.ignoreSeeds data settings fertCost fertilizer
      | StockpileSeeds -> ProfitSummary.stockpileSeeds data settings fertCost fertilizer
      | BuyFirstSeed ->
        fun lastCrop crop harvests ->
          let summary = ProfitSummary.ignoreSeeds data settings fertCost fertilizer lastCrop crop harvests
          let seedsBought = float (if Crop.regrows crop then 1u else harvests)
          { summary with
              SeedsBought = seedsBought
              NetProfit = (summary.NetProfit, summary.SeedPrice) ||> Option.map2 (fun profit (_, price) -> profit - float price * seedsBought)
          }

    let summaries = cropHarvests |> Array.mapi (fun i (crop, harvests) ->
      harvestSummary (i = cropHarvests.Length - 1) crop harvests)

    {
      Fertilizer = fertilizer
      FertilizerPrice = fertilizerVendorAndPrice
      CropSummaries = summaries
      NetProfit = summaries |> Array.mapReduce (Option.map2 (+)) (fun summary -> summary.NetProfit)
      TimeNormalization = 1.0
    }

  let xpSummary data settings fertilizer cropHarvests =
    let summaries = cropHarvests |> Array.map (fun (crop, harvests) -> cropXpSummary data settings crop harvests)
    let xp = summaries |> Array.sumBy (fun summary -> float summary.XpPerItem * summary.ItemQuantity)
    {
      Fertilizer = fertilizer
      CropSummaries = summaries
      Xp = xp
      TimeNormalization = 1.0
    }
