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

let fertilizerBought model crop harvests =
  1.0 + replacedFertilizerAmount model crop harvests

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

  let seedVendorsAndPrices data settings crop =
    settings.Selected.SeedPrices[crop] |> mapSelectedPrices data.SeedPrices[crop]

  let seedVendorsAndPriceValues data settings crop =
    seedVendorsAndPrices data settings crop |> Seq.map (fun (vendor, price) ->
      vendor, Game.seedPrice data settings.Game crop price)

  let seedPrices data settings crop =
    settings.Selected.SeedPrices[crop] |> mapSelected data.SeedPrices[crop]

  let seedPriceValues data settings crop =
    seedPrices data settings crop |> Seq.map (Game.seedPrice data settings.Game crop)

  let products data settings seed item =
    settings.Selected.Products.[seed, item] |> mapSelected data.Products[item]

  let unlockedProducts data settings seed item =
    products data settings seed item |> Seq.filter (Game.productUnlocked data settings.Game.Skills)


module Price =
  let private minVendor (selectedPrices: GameData -> Settings -> _ -> _) data settings customPrices key =
    let price =
      let prices = selectedPrices data settings key |> Array.ofSeq
      if prices.Length = 0 then None else
      let vendor, price = Array.min prices
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

  // let productOutputQuality settings product quality = product |> Product.outputQuality settings.ModData quality

  // let outputQuality settings product quality =
  //   match product with
  //   | NonCustom None -> quality
  //   | NonCustom (Some product) -> productOutputQuality settings product quality
  //   | Custom (_, preservesQuality) -> if preservesQuality then quality else Normal

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

    let bestProducts =
      itemMaxProductAndNormalizedPriceByQuality data settings seed item
      |> Option.map (Qualities.map (fun (product, profit) -> NonCustom <| Some product, profit))

    let sellAs = Array.choose id [|
      rawPrices
      customPrices
      bestProducts
    |]

    if sellAs.Length = 0 then None else
    Qualities.init (fun quality -> sellAs |> Array.mapReduce (maxBy snd) (Qualities.item quality)) |> Some


let fertilizerCost data settings fertilizer =
  if settings.Profit.PayForFertilizer
  then Price.fertilizerMinPrice data settings fertilizer
  else Some 0u

let fertilizerCostOpt data settings = Option.defaultOrMap (Some 0u) (fertilizerCost data settings)

let seedAmount data settings seed mainItem item =
  if int seed = int item then
    if settings.Selected.UseHarvestedSeeds.Contains seed
    then Some 1.0
    else None
  elif mainItem
    && Processor.seedMaker |> Game.processorUnlocked data settings.Game.Skills
    && settings.Selected.UseSeedMaker.Contains seed
  then
    Some <| Processor.seedMakerAmountWith seed
  else
    None


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
// both forage seeds and the seedmaker are selected+unlocked.
// Since this solution also covers and solves all other cases for net profit as well,
// it is reused for simplicity and reducing code size.
let private forageCropNetProfitPerHarvestForageSeedsSolution
  data
  settings
  (crop: ForageCrop)
  seedTarget
  seedPrice
  (profits: _ Qualities array)
  (amounts: _ Qualities)
  =
  let inline (@) a b = a + ("@" + b)

  let seed = crop.Seed

  let validQualities = Quality.all |> Array.filter (fun q -> amounts[q] > 0.0)
  let forageSeedAmounts = crop.Items |> Array.map (fun item -> string item @ "Forage Seeds")

  let constraints = Array.concat [|
    // x seeds are made
    [| "Seeds" === seedTarget |]

    // all items of each quality are used -- is this needed?
    Array.allPairs crop.Items validQualities |> Array.map (fun (item, quality) ->
      string item @ string quality === amounts[quality])

    // all items set aside to create forage seeds are all used
    forageSeedAmounts |> Array.map (fun amount -> amount === 0.0)
  |]

  let variables = ResizeArray ()

  match seedPrice with
  | None -> ()
  | Some price -> variables.Add (BoughtSeeds, [| "Profit", -float price; "Seeds", 1.0 |])

  for i = 0 to crop.Items.Length - 1 do
    let item = crop.Items[i]
    let profits = profits[i]
    let forageSeedAmount = forageSeedAmounts[i]
    let seedAmount = seedAmount data settings seed (item = crop.Items[0]) item

    for quality in validQualities do
      let oneItem = string item @ string quality, 1.0
      let itemQuality = item, quality

      variables.Add (SoldItem itemQuality, [| "Profit", profits[quality]; oneItem |])
      variables.Add (MadeForageSeeds itemQuality, [| forageSeedAmount, -1.0; oneItem |])
      match seedAmount with
      | None -> ()
      | Some amount -> variables.Add (MadeSeeds itemQuality, [| "Seeds", amount; oneItem |])

  let oneOfEachItem = forageSeedAmounts |> Array.map (fun amount -> amount, 1.0)

  if settings.Selected.SellForageSeeds.Contains crop.Seed then
    variables.Add (SoldForageSeeds, oneOfEachItem |> Array.append [| "Profit", float (ForageCrop.forageSeedsPerCraft * Game.seedItemSellPrice data settings.Game seed) |])

  if settings.Selected.UseForageSeeds.Contains crop.Seed then
    variables.Add (UsedForageSeeds, oneOfEachItem |> Array.append [| "Seeds", float ForageCrop.forageSeedsPerCraft |])

  let solution = Solver.solve <| Model.create Maximize "Profit" constraints variables
  assert (solution.status = Infeasible || solution.status = Optimal)
  solution

let private forageCropNetProfitPerHarvestForageSeeds data settings crop seedTarget seedPrice profits amounts =
  let solution = forageCropNetProfitPerHarvestForageSeedsSolution data settings crop seedTarget seedPrice profits amounts
  if solution.status = Optimal
  then Some solution.result
  else None


type [<System.Flags>] NoProfitReasons =
  | None              = 0b0000
  | NotEnoughDays     = 0b0001
  | NoFertilizerPrice = 0b0010
  | NotEnoughSeeds    = 0b0100
  | NoInvestment      = 0b1000
  | All               = 0b1111

type private NPR = NoProfitReasons

let timeNormalizationDivisor (growthSpan: GrowthSpan) crop = function
  | TotalPeriod -> 1.0
  | PerDay -> Growth.daysUsedWith (Crop.regrowTime crop) growthSpan.GrowthTime growthSpan.Harvests |> float
  | PerSeason -> float growthSpan.Span.TotalDays / float Date.daysInSeason

let seedCostsandLimits data settings (seedPrice: nat option) (seed: SeedId) (items: ItemId array) (profits: _ Qualities array) (amounts: _ Qualities array) =
  let costsAndLimits = ResizeArray ()
  let seedPrice = seedPrice |> Option.defaultOrMap System.Double.PositiveInfinity float
  let inline addCostAndLimit (profits: _ Qualities) (amounts: _ Qualities) seedAmount =
    for i = 0 to Quality.highest do
      let cost = profits[enum i] / seedAmount
      let amount = seedAmount * amounts[enum i]
      if cost < seedPrice && amount > 0.0 then
        costsAndLimits.Add (cost, amount)

  if Processor.seedMaker |> Game.processorUnlocked data settings.Game.Skills && settings.Selected.UseSeedMaker.Contains seed then
    addCostAndLimit profits[0] amounts[0] (Processor.seedMakerAmountWith seed)

  if settings.Selected.UseHarvestedSeeds.Contains seed then
    match items |> Array.tryFindIndex (fun item -> int item = int seed) with
    | None -> ()
    | Some i -> addCostAndLimit profits[i] amounts[i] 1.0

  costsAndLimits.Sort (compareBy fst)
  resizeToArray costsAndLimits


let private seedCostCalc seedPrice (costsAndLimits: _ array) (harvests: nat) =
  let mutable seedsLeft = 1.0
  let mutable totalCost = 0.0
  let mutable i = 0

  while i < costsAndLimits.Length && seedsLeft > 0.0 do
    let cost, seedsPerHarvest = costsAndLimits[i]
    let seedsMade = (seedsPerHarvest * float harvests) |> min seedsLeft
    totalCost <- totalCost + cost * seedsMade
    seedsLeft <- seedsLeft - seedsMade
    i <- i + 1

  if seedsLeft = 0.0 then Some totalCost else
  match seedPrice with
  | Some price ->
    Some (totalCost + float price * seedsLeft)
  | None -> None

let seedCost data settings seedPrice seed items profits amounts harvests = seedCostCalc seedPrice (seedCostsandLimits data settings seedPrice seed items profits amounts) harvests

let private farmCropProfitPerHarvestCalc profits amounts = Array.map2Reduce (+) Qualities.dot profits amounts

let cropItemProfits data settings seed items = items |> Array.map (Price.itemMaxNormalizedPriceByQuality data settings seed >> Option.defaultValue Qualities.zerof)

let canUseSeedMakerForOwnSeeds data settings seed = Processor.seedMaker |> Game.processorUnlocked data settings.Game.Skills && settings.Selected.UseSeedMaker.Contains seed

let canUseForageSeeds settings = function
  | ForageCrop crop -> ForageCrop.seedsRecipeUnlocked settings.Game.Skills crop && settings.Selected.UseForageSeeds.Contains crop.Seed
  | FarmCrop _ -> false

let nonRegrowData data settings crop =
  let seed = Crop.seed crop
  let seedPrice = Price.seedMinPrice data settings seed
  let hasSeedSource =
    seedPrice.IsSome
    || canUseSeedMakerForOwnSeeds data settings seed
    || settings.Selected.UseHarvestedSeeds.Contains seed

  match crop with
  | FarmCrop crop ->
    if not hasSeedSource || crop.RegrowTime.IsSome then None else
    let items = FarmCrop.items crop
    let profits = items |> cropItemProfits data settings crop.Seed
    let seedCost = seedCost data settings seedPrice seed items profits
    Some (fun fertilizer ->
      let amounts = Game.farmCropItemAmounts settings.Game fertilizer crop
      match seedCost amounts 1u with
      | Some cost ->
        let profit = farmCropProfitPerHarvestCalc profits amounts
        Some (profit - cost)
      | None -> None)
  | ForageCrop crop ->
    let seedsUnlocked = ForageCrop.seedsRecipeUnlocked settings.Game.Skills crop
    let useForageSeeds = settings.Selected.UseForageSeeds.Contains seed
    let hasSeedSource = hasSeedSource || (seedsUnlocked && useForageSeeds)
    if not hasSeedSource then None else
    let amounts = Game.forageCropItemAmounts settings.Game crop
    let profits = crop.Items |> cropItemProfits data settings crop.Seed
    let net =
      if seedsUnlocked && (useForageSeeds || settings.Selected.SellForageSeeds.Contains seed) then
        forageCropNetProfitPerHarvestForageSeeds data settings crop 1.0 seedPrice profits amounts
      else
        let profit = profits |> Array.sumBy (Qualities.dot amounts)
        seedCost data settings seedPrice seed crop.Items profits (Array.create crop.Items.Length amounts) 1u
        |> Option.map (fun cost -> profit - cost)
    Some (konst net)

type RegrowData = {
  Profit: float
  GrowthTime: nat
  HarvestsForMinCost: nat
  MinCost: float
  Cost: nat -> float option
}

let regrowSeedData data settings crop =
  let seed = Crop.seed crop
  let seedPrice = Price.seedMinPrice data settings seed
  let hasSeedSource =
    seedPrice.IsSome
    || canUseSeedMakerForOwnSeeds data settings seed
    || settings.Selected.UseHarvestedSeeds.Contains seed

  if not hasSeedSource then None else
  let items = Crop.items crop
  let profits = cropItemProfits data settings seed items
  Some (fun fertilizer ->
    let amounts = Game.cropItemAmounts settings.Game fertilizer crop
    let profit = farmCropProfitPerHarvestCalc profits amounts
    let costsAndLimits = seedCostsandLimits data settings seedPrice seed items profits amounts
    costsAndLimits
    |> Array.tryHead
    |> Option.orElse (seedPrice |> Option.map (fun price -> float price, System.Double.MaxValue))
    |> Option.map (fun (cost, limit) -> {
      Profit = profit
      GrowthTime = Game.growthTime settings.Game fertilizer crop
      HarvestsForMinCost = ceil (1.0 / limit) |> nat
      MinCost = cost
      Cost = seedCostCalc seedPrice costsAndLimits
    }))

let private cropProfitCalc netProfit data settings timeNormalization crop =
  let netProfit = netProfit data settings crop
  fun fertilizer ->
    let growthSpan = bestGrowthSpan settings.Game fertilizer crop
    let fertCost = fertilizerCostOpt data settings (Fertilizer.Opt.name fertilizer)
    match growthSpan, netProfit, fertCost with
    | Some span, Some netProfit, Some fertCost ->
      match netProfit fertilizer span.Harvests with
      | Some netProfit ->
        let fertilizerCost = float fertCost * fertilizerBought settings crop span.Harvests
        let divisor = timeNormalizationDivisor span crop timeNormalization
        Ok ((netProfit - fertilizerCost), divisor)
      | None -> Error NPR.NotEnoughSeeds
    | _ ->
      ((if fertCost.IsNone then NPR.NoFertilizerPrice else NPR.None)
      ||| (if growthSpan.IsNone then NPR.NotEnoughDays else NPR.None)
      ||| (if netProfit.IsNone then NPR.NotEnoughSeeds else NPR.None))
      |> Error

let private cropProfitCalcIgnoreSeeds = cropProfitCalc (fun data settings crop ->
  (match crop with
  | FarmCrop crop ->
    let profits = FarmCrop.items crop |> cropItemProfits data settings crop.Seed
    fun fertilizer harvests ->
      let amounts = Game.farmCropItemAmounts settings.Game fertilizer crop
      Some ((farmCropProfitPerHarvestCalc profits amounts) * float harvests)
  | ForageCrop crop ->
    let seed = crop.Seed
    let profits = crop.Items |> cropItemProfits data settings crop.Seed
    let amounts = Game.forageCropItemAmounts settings.Game crop
    let profit =
      if ForageCrop.seedsRecipeUnlocked settings.Game.Skills crop
        && (settings.Selected.UseForageSeeds.Contains seed || settings.Selected.SellForageSeeds.Contains seed)
      then
        let profit = forageCropNetProfitPerHarvestForageSeeds data settings crop 0.0 None profits amounts
        assert profit.IsSome
        profit |> Option.defaultValue 0.0
      else
        farmCropProfitPerHarvestCalc profits (Array.create crop.Items.Length amounts)
    fun _ harvests -> Some(profit * float harvests))
  |> Some)

let private cropProfitCalcStockpileSeeds = cropProfitCalc (fun data settings crop ->
  let seed = Crop.seed crop
  let seedPrice = Price.seedMinPrice data settings seed
  let hasSeedSource =
    seedPrice.IsSome
    || canUseSeedMakerForOwnSeeds data settings seed
    || settings.Selected.UseHarvestedSeeds.Contains seed

  match crop with
  | FarmCrop crop ->
    if not hasSeedSource then None else
    let items = FarmCrop.items crop
    let profits = items |> cropItemProfits data settings crop.Seed
    let seedCost = seedCost data settings seedPrice seed items profits
    (if crop.RegrowTime.IsSome then
      fun fertilizer harvests ->
        let amounts = Game.farmCropItemAmounts settings.Game fertilizer crop
        let profit = farmCropProfitPerHarvestCalc profits amounts
        match seedCost amounts harvests with
        | Some cost -> Some (profit * float harvests - cost)
        | None -> None
      else
      fun fertilizer harvests ->
        let amounts = Game.farmCropItemAmounts settings.Game fertilizer crop
        match seedCost amounts 1u with
        | Some cost ->
          let profit = farmCropProfitPerHarvestCalc profits amounts
          Some ((profit - cost) * float harvests)
        | None -> None)
    |> Some
  | ForageCrop crop ->
    let seedsUnlocked = ForageCrop.seedsRecipeUnlocked settings.Game.Skills crop
    let useForageSeeds = settings.Selected.UseForageSeeds.Contains seed
    let hasSeedSource = hasSeedSource || (seedsUnlocked && useForageSeeds)
    if not hasSeedSource then None else
    let amounts = Game.forageCropItemAmounts settings.Game crop
    let profits = crop.Items |> cropItemProfits data settings crop.Seed
    let net =
      if seedsUnlocked && (useForageSeeds || settings.Selected.SellForageSeeds.Contains seed) then
        forageCropNetProfitPerHarvestForageSeeds data settings crop 1.0 seedPrice profits amounts
      else
        let profit = profits |> Array.sumBy (Qualities.dot amounts)
        seedCost data settings seedPrice seed crop.Items profits (Array.create crop.Items.Length amounts) 1u
        |> Option.map (fun cost -> profit - cost)
    Some (fun _ harvests -> net |> Option.map ((*) (float harvests))))

// refactor, there will always be a seed cost
let private cropProfitCalcBuyFirstSeed = cropProfitCalc (fun data settings crop ->
  let seed = Crop.seed crop
  match Price.seedMinPrice data settings seed with
  | None -> None
  | Some seedPrice ->
    (match crop with
    | FarmCrop crop ->
      let items = FarmCrop.items crop
      let profits = items |> cropItemProfits data settings crop.Seed
      let seedCost = seedCost data settings (Some seedPrice) crop.Seed items profits
      if crop.RegrowTime.IsSome then
        fun fertilizer harvests ->
          let amounts = Game.farmCropItemAmounts settings.Game fertilizer crop
          let profit = farmCropProfitPerHarvestCalc profits amounts
          Some (profit * float harvests - float seedPrice)
      else
        fun fertilizer harvests ->
          let amounts = Game.farmCropItemAmounts settings.Game fertilizer crop
          match seedCost amounts 1u with
          | Some cost ->
            let profit = farmCropProfitPerHarvestCalc profits amounts
            Some ((profit - cost) * float harvests + cost - float seedPrice)
          | None -> None
    | ForageCrop crop ->
      let amounts = Game.forageCropItemAmounts settings.Game crop
      let profits = crop.Items |> cropItemProfits data settings crop.Seed
      let profit = profits |> Array.sumBy (Qualities.dot amounts)
      let netAndcost =
        if ForageCrop.seedsRecipeUnlocked settings.Game.Skills crop
          && (settings.Selected.UseForageSeeds.Contains seed || settings.Selected.SellForageSeeds.Contains seed)
        then
          forageCropNetProfitPerHarvestForageSeeds data settings crop 1.0 (Some seedPrice) profits amounts
          |> Option.map (fun net -> net, profit - net)
        else
          seedCost data settings (Some seedPrice) seed crop.Items profits (Array.create crop.Items.Length amounts) 1u
          |> Option.map (fun cost -> profit - cost, cost)
      fun _ harvests ->
        match netAndcost with
        | Some (net, cost) ->
          Some (net * float harvests + cost - float seedPrice)
        | None -> None)
    |> Some)

let cropProfit data (settings: Settings) timeNorm crop =
  match settings.Profit.SeedStrategy with
  | IgnoreSeeds -> cropProfitCalcIgnoreSeeds data settings timeNorm crop
  | StockpileSeeds -> cropProfitCalcStockpileSeeds data settings timeNorm crop
  | BuyFirstSeed -> cropProfitCalcBuyFirstSeed data settings timeNorm crop
  >> Result.map (fun (profit, timeNorm) -> profit / timeNorm)

let cropXP data settings timeNorm crop fertilizer =
  let seed = Crop.seed crop
  let hasFertPrice = fertilizerCostOpt data settings (Fertilizer.Opt.name fertilizer) |> Option.isSome
  let enoughSeeds =
    match settings.Profit.SeedStrategy with
    | IgnoreSeeds -> true
    | BuyFirstSeed -> Price.seedMinPrice data settings seed |> Option.isSome
    | StockpileSeeds ->
      Price.seedMinPrice data settings seed |> Option.isSome
      || (match crop with
          | FarmCrop c ->
            canUseSeedMakerForOwnSeeds data settings seed
            || (settings.Selected.UseHarvestedSeeds.Contains seed && (int c.Item = int seed || c.ExtraItem |> Option.exists (fun (item, amount) -> amount >= 1.0 && int item = int seed)))
          | ForageCrop _ -> canUseForageSeeds settings crop) // assume forage crop has >=3 items so that seedmaker does not give enough seeds

  match bestGrowthSpan settings.Game fertilizer crop with
  | Some span when hasFertPrice && enoughSeeds ->
    let xpPerHarvest = Game.xpPerHarvest data settings.Game crop
    let xp = float span.Harvests * xpPerHarvest
    let divisor = timeNormalizationDivisor span crop timeNorm
    Ok (xp / divisor)
  | span ->
    ((if not hasFertPrice then NPR.NoFertilizerPrice else NPR.None)
    ||| (if span.IsNone then NPR.NotEnoughDays else NPR.None)
    ||| (if not enoughSeeds then NPR.NotEnoughSeeds else NPR.None))
    |> Error

let cropXpData data settings timeNorm crop fertilizer =
  let seed = Crop.seed crop
  let hasFertPrice = fertilizerCostOpt data settings (Fertilizer.Opt.name fertilizer) |> Option.isSome
  let enoughSeeds =
    match settings.Profit.SeedStrategy with
    | IgnoreSeeds -> true
    | BuyFirstSeed -> Price.seedMinPrice data settings seed |> Option.isSome
    | StockpileSeeds ->
      Price.seedMinPrice data settings seed |> Option.isSome
      || (match crop with
          | FarmCrop c ->
            canUseSeedMakerForOwnSeeds data settings seed
            || (settings.Selected.UseHarvestedSeeds.Contains seed && (int c.Item = int seed || c.ExtraItem |> Option.exists (fun (item, amount) -> amount >= 1.0 && int item = int seed)))
          | ForageCrop _ -> canUseForageSeeds settings crop) // assume forage crop has >=3 items so that seedmaker does not give enough seeds

  match bestGrowthSpan settings.Game fertilizer crop with
  | Some span when hasFertPrice && enoughSeeds ->
    Ok {|
      xpPerItem = Crop.xpPerItem data.Items.Find crop
      xpPerHarvest = Game.xpPerHarvest data settings.Game crop
      TimeNormalization = timeNormalizationDivisor span crop timeNorm
      Harvests = span.Harvests
    |}
  | span ->
    ((if not hasFertPrice then NPR.NoFertilizerPrice else NPR.None)
    ||| (if span.IsNone then NPR.NotEnoughDays else NPR.None)
    ||| (if not enoughSeeds then NPR.NotEnoughSeeds else NPR.None))
    |> Error

let private cropROIWith metric data (settings: Settings) timeNormalization crop =
  let seedPrice =
    match settings.Profit.SeedStrategy with
    | BuyFirstSeed -> Price.seedMinPrice data settings (Crop.seed crop)
    | _ -> Some 0u
  let metric = metric data settings timeNormalization crop

  fun fertilizer ->
    let fertCost = fertilizerCostOpt data settings (Fertilizer.Opt.name fertilizer)
    match metric fertilizer with
    | Ok (profit, timeNorm) ->
        let investment = Option.get seedPrice + Option.get fertCost
        if investment = 0u
        then Error NPR.NoInvestment
        else Ok((profit - float investment) / float investment * 100.0 / timeNorm)
    | Error e -> Error e


let cropROI data (settings: Settings) timeNormalization crop =
  match settings.Profit.SeedStrategy with
  | IgnoreSeeds -> cropROIWith cropProfitCalcIgnoreSeeds data settings timeNormalization crop
  | StockpileSeeds -> cropROIWith cropProfitCalcStockpileSeeds data settings timeNormalization crop
  | BuyFirstSeed -> cropROIWith cropProfitCalcBuyFirstSeed data settings timeNormalization crop

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
    then items |> Array.findIndex (fun item -> int item = int seed) |> Some
    else None
  match seedItemIndex with
  | None -> ()
  | Some i -> addCostAndLimit i 1.0

  let useSeedMaker = Processor.seedMaker |> Game.processorUnlocked data settings.Game.Skills && settings.Selected.UseSeedMaker.Contains seed
  if useSeedMaker then addCostAndLimit 0 (Processor.seedMakerAmountWith (items[0] * 1u<_>))

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
    let amounts = Game.cropItemAmounts settings.Game fertilizer crop
    let fertCost' =
      if settings.Profit.PayForFertilizer
      then fertilizer |> Option.map (Fertilizer.name >> Price.fertilizerMinVendorAndPrice data settings)
      else None
    let fertCost = fertCost' |> Option.defaultOrMap (Some 0u) (Option.map snd)
    let fertilizerBought = fertilizerBought settings crop harvests
    let fertCost = fertCost |> Option.map (float >> (*) fertilizerBought)

    let soldAmounts, seedAmounts, seedsBought, forageSeedsSold, forageSeedsUsed, net =
      match crop with
      | ForageCrop c when ForageCrop.seedsRecipeUnlocked settings.Game.Skills c
          && (settings.Selected.UseForageSeeds.Contains seed || settings.Selected.SellForageSeeds.Contains seed) ->
        let useForageSeeds = settings.Selected.UseForageSeeds.Contains seed
        let seedMaker = not useForageSeeds && Processor.seedMaker |> Game.processorUnlocked data settings.Game.Skills && settings.Selected.UseSeedMaker.Contains seed
        let maxSeeds =
          if seedPrice.IsSome
          then 1.0
          else min 1.0 (Qualities.sum amounts[0] * if useForageSeeds then float ForageCrop.forageSeedsPerCraft elif seedMaker then Processor.seedMakerAmount else 0.0)

        let totalAmounts = amounts[0] |> Qualities.mult (float harvests)

        let solution = forageCropNetProfitPerHarvestForageSeedsSolution data settings c (maxSeeds * float harvests) seedPrice profits totalAmounts
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
          c.Items |> Array.map (fun item -> sold.TryFind item |> Option.defaultValue Qualities.zerof)

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
        | None -> Qualities.zerof)
    let amounts = Game.cropItemAmounts settings.Game fertilizer crop
    let fertCost' =
      if settings.Profit.PayForFertilizer
      then fertilizer |> Option.map (Fertilizer.name >> Price.fertilizerMinVendorAndPrice data settings)
      else None
    let fertCost = fertCost' |> Option.defaultOrMap (Some 0u) (Option.map snd)
    let fertBought = fertilizerBought settings crop harvests
    let fertCost = fertCost |> Option.map (float >> (*) fertBought)

    let soldAmounts, seedAmounts, seedsBought, forageSeedsSold, forageSeedsUsed, net =
      match crop with
      | ForageCrop c when ForageCrop.seedsRecipeUnlocked settings.Game.Skills c
          && (settings.Selected.UseForageSeeds.Contains seed || settings.Selected.SellForageSeeds.Contains seed) ->
        let useForageSeeds = settings.Selected.UseForageSeeds.Contains seed
        let seedMaker = not useForageSeeds && Processor.seedMaker |> Game.processorUnlocked data settings.Game.Skills && settings.Selected.UseSeedMaker.Contains seed
        let maxSeeds =
          if seedPrice.IsSome
          then 1.0
          else min 1.0 (Qualities.sum amounts[0] * if useForageSeeds then float ForageCrop.forageSeedsPerCraft elif seedMaker then Processor.seedMakerAmount else 0.0)

        let h1 = float (harvests - 1u)
        let totalAmounts = amounts[0] |> Qualities.mult h1

        let solution = forageCropNetProfitPerHarvestForageSeedsSolution data settings c (maxSeeds * h1) seedPrice profits totalAmounts
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
          c.Items |> Array.map (fun item -> sold.TryFind item |> Option.defaultValue Qualities.zerof)

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
        | None -> Qualities.zerof)
    let amounts = Game.cropItemAmounts settings.Game fertilizer crop
    let fertCost' =
      if settings.Profit.PayForFertilizer
      then fertilizer |> Option.map (Fertilizer.name >> Price.fertilizerMinVendorAndPrice data settings)
      else None
    let fertCost = fertCost' |> Option.defaultOrMap (Some 0u) (Option.map snd)
    let fertBought = fertilizerBought settings crop harvests
    let fertCost = fertCost |> Option.map (float >> (*) fertBought)

    let net, soldAmounts, forageSeedsSold =
      match crop with
      | ForageCrop c when ForageCrop.seedsRecipeUnlocked settings.Game.Skills c
          && (settings.Selected.UseForageSeeds.Contains seed || settings.Selected.SellForageSeeds.Contains seed) ->
        let soldAmounts = amounts[0] |> Qualities.mult (float harvests)
        let solution = forageCropNetProfitPerHarvestForageSeedsSolution data settings c 0.0 None profits soldAmounts
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
          c.Items |> Array.map (fun item -> sold.TryFind item |> Option.defaultValue Qualities.zerof)

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

let cropProfitData data (settings: Settings) timeNormalization crop fertilizer =
  match settings.Profit.SeedStrategy with
  | IgnoreSeeds -> cropProfitDataIgnoreSeeds data settings timeNormalization crop fertilizer
  | StockpileSeeds -> cropProfitDataStockpileSeeds data settings timeNormalization crop fertilizer
  | BuyFirstSeed -> cropProfitDataBuyFirstSeed data settings timeNormalization crop fertilizer
