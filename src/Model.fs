namespace StardewValleyStonks

type Location =
  | Farm
  | Greenhouse
  | [<CompiledName ("Ginger Island")>] GingerIsland

module Location = let all = unitUnionCases<Location>


type TimeNormalization =
  | [<CompiledName ("Total")>] TotalPeriod
  | [<CompiledName ("Per Day")>] PerDay
  | [<CompiledName ("Per Season")>] PerSeason

module TimeNormalization = let all = unitUnionCases<TimeNormalization>


type SeedStrategy =
  | [<CompiledName ("Buy First Seed")>] BuyFirstSeed
  | [<CompiledName ("Stockpile Seeds")>] StockpileSeeds
  | [<CompiledName ("Ignore Seeds")>] IgnoreSeeds

module SeedStrategy = let all = unitUnionCases<SeedStrategy>

// Assume for now that SeedMaker is the only processor which converts items into seeds.
type GameData = {
  Fertilizers: Table<FertilizerName, Fertilizer>
  FertilizerPrices: Table<FertilizerName, Table<Vendor, nat>>
  FertilizerVendors: Vendor array
  Crops: Table<SeedId, Crop>
  FarmCrops: Table<SeedId, FarmCrop>
  ForageCrops: Table<SeedId, ForageCrop>
  SeedPrices: Table<SeedId, Table<Vendor, SeedPrice>>
  SeedVendors: Vendor array
  Items: Table<ItemId, Item>
  Products: Table<ItemId, Table<Processor, Product>>
  Processors: Processor array
  ProcessorUnlockLevel: Table<Processor, nat>
}

module GameData =
  let canGetOwnSeedsFromSeedMaker crop data =
    match data.Products[Crop.mainItem crop].TryFind Processor.seedMaker with
    | Some (SeedsFromSeedMaker item) when item = Crop.seedItem crop -> true
    | _ -> false

type Selection<'a, 'b when 'a: comparison> = {
  Values: Map<'a, 'b>
  Selected: 'a Set
}

module Selection =
  let empty = {
    Values = Map.empty
    Selected = Set.empty
  }

  let selectedValue key selection =
    if selection.Selected.Contains key
    then Some selection.Values[key]
    else None

  let allSelected selection =
    selection.Values.Keys |> Seq.forall selection.Selected.Contains


type Model = {
  Data: GameData // The immutable game data the Model acts as a filter over

  SelectedCrops: SeedId Set
  SelectedSeedPrices: Map<SeedId, Vendor Set>

  AllowNoFertilizer: bool
  SelectedFertilizers: FertilizerName Set
  SelectedFertilizerPrices: Map<FertilizerName, Vendor Set>

  SellRawItems: (SeedId * ItemId) Set
  SelectedProducts: Map<SeedId * ItemId, Processor Set>
  SellForageSeeds: SeedId Set

  UseRawSeeds: SeedId Set
  UseSeedMaker: SeedId Set
  UseForageSeeds: SeedId Set

  CustomSeedPrices: Selection<SeedId, nat>
  CustomFertilizerPrices: Selection<FertilizerName, nat>
  CustomSellPrices: Selection<SeedId * ItemId, nat * bool>

  Skills: Skills
  Multipliers: Multipliers
  CropAmount: CropAmountSettings
  ModData: ModData
  JojaMembership: bool
  Irrigated: bool

  StartDate: Date
  EndDate: Date
  Location: Location

  SeedStrategy: SeedStrategy
  PayForFertilizer: bool
  ReplaceLostFertilizer: bool
}

type CustomChoice<'a, 'b> =
  | NonCustom of 'a
  | Custom of 'b

module Model =
  let inline getItem model item = model.Data.Items[item]
  let inline getSeedItem model (seed: SeedId) = getItem model (seed * 1<_>)
  let inline getProduct model item processor = model.Data.Products[item].[processor]
  let inline getFertilizer model fertilizer = model.Data.Fertilizers[fertilizer]
  let getFertilizerOpt model fertilizer =
    fertilizer |> Option.map (getFertilizer model)
  let inline getCrop model crop = model.Data.Crops[crop]

  let inline forageCrops model = model.Data.ForageCrops.Values
  let inline forageCropIds model = model.Data.ForageCrops.Keys

  let processorUnlocked model processor =
    model.Data.ProcessorUnlockLevel.TryFind processor |> Option.forall (flip Skills.farmingLevelMet model.Skills)
  let productUnlocked model = Product.processor >> processorUnlocked model

  let growthMultiplier model growth =
    (if Skills.agriculturistActive model.Skills then 0.1 else 0.0)
    + if model.Irrigated && growth.Paddy then 0.25 else 0.0

  let growthSpeed model fertilizer growth =
    Fertilizer.Opt.speed fertilizer + growthMultiplier model growth

  let reducedGrowthStagesAndTime model fertilizer growth =
    growthSpeed model fertilizer growth |> Growth.stagesAndTime growth

  let growthTime model fertilizer growth = reducedGrowthStagesAndTime model fertilizer growth |> snd

  let cropGrowthMultiplier model = Crop.growth >> growthMultiplier model
  let cropGrowthTime model fertilizer = Crop.growth >> growthTime model fertilizer
  let cropReducedStagesAndTime model fertilizer = Crop.growth >> reducedGrowthStagesAndTime model fertilizer

  let growthSpans model crop =
    if model.Location = Farm
    then Crop.seasons crop
    else Seasons.All
    |> Date.spans model.StartDate model.EndDate

  let harvests model crop fertilizer =
    let growth = Crop.growth crop
    let harvests = Growth.consecutiveHarvests (growthSpans model crop) (growthSpeed model fertilizer growth) growth
    Array.natSum harvests

  let giantCropsPossible model = model.Location <> Greenhouse

  let giantCropProb model = if giantCropsPossible model then CropAmount.giantCropProb model.CropAmount else 0.0

  let farmCropFertilizerLossProb model (crop: FarmCrop) =
    if giantCropsPossible model && crop.Amount.Giant
    then Fertilizer.lossProbability * CropAmount.giantCropProb model.CropAmount
    else 0.0

  let lostFertilizerPerHarvest model = function
    | FarmCrop c -> farmCropFertilizerLossProb model c
    | ForageCrop _ -> Fertilizer.lossProbability

  let selectedFertilizers model = model.SelectedFertilizers |> Seq.map (getFertilizer model)
  let selectedFertilizersOpt model =
    selectedFertilizers model
    |> Seq.map Some
    |> Seq.append (if model.AllowNoFertilizer then [ None ] else [])

  let selectedFertilizerVendorPrices model fertilizer =
    model.SelectedFertilizerPrices[fertilizer] |> Seq.map (fun vendor -> vendor, model.Data.FertilizerPrices[fertilizer].[vendor])

  let selectedFertilizerPrices model fertilizer =
    model.SelectedFertilizerPrices[fertilizer] |> Seq.map model.Data.FertilizerPrices[fertilizer].Find

  let lowestFertilizerPrice model fertilizer =
    selectedFertilizerPrices model fertilizer
    |> Seq.tryMin
    |> Option.min (model.CustomFertilizerPrices |> Selection.selectedValue fertilizer)

  let lowestFertilizerCost model fertilizer =
    if model.PayForFertilizer
    then lowestFertilizerPrice model fertilizer
    else Some 0u
  let lowestFertilizerCostOpt model = Option.defaultOrMap (Some 0u) (lowestFertilizerCost model)

  let replacementFertilizerAmount model crop (harvests: nat) =
    if model.ReplaceLostFertilizer
    then float harvests * lostFertilizerPerHarvest model crop
    else 0.0

  let fertilizerUsed model crop harvests =
    1.0 + replacementFertilizerAmount model crop harvests

  // let fertilizerCost model crop fertilizer harvests =
  //   lowestFertilizerCost model fertilizer |> Option.map (float >> (*) (fertilizerUsed model crop harvests))


  let selectedCrops model = model.SelectedCrops |> Seq.map (getCrop model)

  let seedPrice model seed = function
    | FixedPrice (_, price) -> price
    | ScalingPrice (vendor, price) ->
      let price =
        match price with
        | Some price -> price
        | None -> 2u * (getSeedItem model seed |> Item.sellPrice)
      if vendor = Vendor.joja
      then price |> withMultiplier (model.Multipliers.ProfitMargin * if model.JojaMembership then 1.0 else 1.25)
      else price |> withMultiplier model.Multipliers.ProfitMargin |> max 1u

  let selectedVendorSeedPrices model crop =
    let seedPrices = model.Data.SeedPrices[crop]
    model.SelectedSeedPrices[crop] |> Seq.map (fun vendor -> vendor, seedPrices[vendor])
  let selectedVendorSeedPriceValues model crop =
    selectedVendorSeedPrices model crop |> Seq.map (fun (vendor, price) -> vendor, seedPrice model crop price)

  let selectedSeedPrices model crop = model.SelectedSeedPrices[crop] |> Seq.map model.Data.SeedPrices[crop].Find
  let selectedSeedPriceValues model crop = selectedSeedPrices model crop |> Seq.map (seedPrice model crop)

  let seedPriceFrom model vendor crop = model.Data.SeedPrices[crop].TryFind vendor
  let seedPriceValueFrom model vendor crop = seedPriceFrom model vendor crop |> Option.map (seedPrice model crop)

  let lowestSeedPrice model seed =
    selectedSeedPriceValues model seed
    |> Seq.tryMin
    |> Option.min (model.CustomSeedPrices |> Selection.selectedValue seed)

  let selectedProducts model seed item = model.SelectedProducts.[seed, item] |> Seq.map (getProduct model item)

  let seasons model = Date.seasonsBetween model.StartDate model.EndDate

  let cropInSeasonFrom model seasons crop =
    model.Location <> Farm || crop |> Crop.growsInSeasons seasons

  let cropInSeason model = cropInSeasonFrom model (seasons model)

  let inSeasonCrops model =
    model.Data.Crops.Values |> Seq.filter (cropInSeason model)

  let selectedInSeasonCrops model =
    selectedCrops model |> Seq.filter (cropInSeason model)


  open type Quality

  let itemPrice model item quality = Item.price model.Skills model.Multipliers item quality
  let itemPrices model item = Item.prices model.Skills model.Multipliers item
  let itemForagePrice model item quality = Item.Forage.price model.Skills model.Multipliers item quality
  let itemForagePrices model item = Item.Forage.prices model.Skills model.Multipliers item

  let productPrice model item quality product = Product.price (getItem model) model.Skills model.Multipliers model.ModData item quality product
  let productPrices model item product = Product.prices (getItem model) model.Skills model.Multipliers model.ModData item product

  let productProfit model item quality product = Product.profit (getItem model) model.Skills model.Multipliers model.ModData item quality product
  let productProfits model item product = Product.profits (getItem model) model.Skills model.Multipliers model.ModData item product

  let productOutputQuality model product quality = product |> Product.outputQuality model.ModData quality

  let outputQuality model product quality =
    match product with
    | NonCustom None -> quality
    | NonCustom (Some product) -> productOutputQuality model product quality
    | Custom (_, preservesQuality) -> if preservesQuality then quality else Normal

  let selectedProductsCalc mapping model seed item =
    selectedProducts model seed item |> Seq.choose (fun product ->
      if productUnlocked model product
      then Some <| mapping product
      else None)

  let bestSelectedProducts model seed item =
    let selected = selectedProducts model seed item |> Seq.filter (productUnlocked model) |> Array.ofSeq
    if selected.Length = 0 then None else
    let profits = selected |> Array.map (productProfits model (getItem model item))
    Array.init Quality.count (fun quality ->
      let mutable maxI = 0
      let mutable maxProfit = -1.0
      for i = 0 to profits.Length - 1 do
        let profit = profits[i].[quality]
        if profit > maxProfit then
          maxI <- i
          maxProfit <- profit
      selected[maxI])
    |> Some

  let bestSelectedProductsAndProfit model seed item =
    let selected = selectedProducts model seed item |> Seq.filter (productUnlocked model) |> Array.ofSeq
    if selected.Length = 0 then None else
    let profits = selected |> Array.map (productProfits model (getItem model item))
    Array.init Quality.count (fun quality ->
      let mutable maxI = 0
      let mutable maxProfit = -1.0
      for i = 0 to profits.Length - 1 do
        let profit = profits[i].[quality]
        if profit > maxProfit then
          maxI <- i
          maxProfit <- profit
      selected[maxI], maxProfit)
    |> Some

  let private itemBestProductCalc comparisonValue model seed item quality =
    selectedProductsCalc (comparisonValue model (getItem model item) quality) model seed item |> Seq.tryMin

  let private itemBestProductCalcs comparisonValue model seed item =
    let products = selectedProductsCalc (comparisonValue model (getItem model item)) model seed item |> Array.ofSeq
    if products.Length = 0 then None else
    Some <| Qualities.init (fun quality -> products |> Array.mapReduce max (Qualities.item quality))

  let itemBestProductPrice model seed item quality = itemBestProductCalc productPrice model seed item quality
  let itemBestProductProfit model seed item quality = itemBestProductCalc productProfit model seed item quality

  let itemBestProductPrices model seed item = itemBestProductCalcs productPrices model seed item
  let itemBestProductProfits model seed item = itemBestProductCalcs productProfits model seed item

  let customSellPrice model seed item (quality: Quality) =
    model.CustomSellPrices
    |> Selection.selectedValue (seed, item)
    |> Option.map (fun (price, preserveQuality) ->
      if preserveQuality
      then price |> withMultiplier Qualities.multipliers[quality]
      else price)

  let customSellPrices model seed item =
    model.CustomSellPrices
    |> Selection.selectedValue (seed, item)
    |> Option.map (fun (price, preserveQuality) ->
      if preserveQuality
      then Qualities.multipliers |> Qualities.map (flip withMultiplier price >> float)
      else price |> float |> Qualities.create)

  let itemBestPrice model seed item quality =
    if model.SellRawItems.Contains (seed, item)
    then Some <| itemPrice model (getItem model item) quality
    else None
    |> Option.max (itemBestProductPrice model seed item quality)
    |> Option.max (customSellPrice model seed item quality)

  let itemBestPrices model seed item =
    if model.SellRawItems.Contains (seed, item)
    then Some <| itemPrices model (getItem model item)
    else None
    |> Option.reduce (Qualities.map2 max) (itemBestProductPrices model seed item)
    |> Option.reduce (Qualities.map2 max) (customSellPrices model seed item)

  let itemBestProfit model seed item quality =
    if model.SellRawItems.Contains (seed, item)
    then Some (float <| itemPrice model (getItem model item) quality)
    else None
    |> Option.max (itemBestProductProfit model seed item quality)
    |> Option.max (customSellPrice model seed item quality |> Option.map float)

  let itemBestProfits model seed item =
    if model.SellRawItems.Contains (seed, item)
    then Some <| itemPrices model (getItem model item)
    else None
    |> Option.reduce (Qualities.map2 max) (itemBestProductProfits model seed item)
    |> Option.reduce (Qualities.map2 max) (customSellPrices model seed item)

  // refactor / correct forage prices

  let seedLowestPriceBuyFrom model seed =
    let price =
      selectedVendorSeedPriceValues model seed
      |> Seq.tryMinBy snd
      |> Option.map (fun (v, p) -> NonCustom v, p)

    let custom =
      model.CustomSeedPrices
      |> Selection.selectedValue seed
      |> Option.map (tuple2 (Custom ()))

    Option.reduce (minBy snd) price custom


  let fertilizerLowestPriceBuyFrom model fertilizer =
    let price =
      selectedFertilizerVendorPrices model fertilizer
      |> Seq.tryMin
      |> Option.map (fun (v, p) -> NonCustom v, p)

    let custom =
      model.CustomFertilizerPrices
      |> Selection.selectedValue fertilizer
      |> Option.map (tuple2 (Custom ()))

    Option.reduce (minBy snd) price custom

  let itemBestProfitsSellAs model seed item =
    let rawPrices =
      if model.SellRawItems.Contains (seed, item) then
        let prices = itemPrices model (getItem model item)
        Quality.all
        |> Array.map (fun quality -> NonCustom None, prices[quality])
        |> Some
        // Some (NonCustom None, itemPrices model (getItem model item))
      else
        None

    let customPrices =
      model.CustomSellPrices
      |> Selection.selectedValue (seed, item)
      |> Option.map (fun ((price, preservesQuality) as custom) ->
        if preservesQuality then
          Quality.all |> Array.map (fun quality ->
            Custom custom, price |> withMultiplier Qualities.multipliers[quality] |> float)
        else
          Array.create Quality.count (Custom custom, float price))

    let bestProducts =
      bestSelectedProductsAndProfit model seed item
      |> Option.map (Array.map (fun (product, profit) -> NonCustom <| Some product, profit))
      // bestSelectedProducts model seed item |> Option.map (fun products ->
      //   let item = getItem model item
      //   Quality.all |> Block.map' (fun quality ->
      //     let product = products[int quality]
      //     NonCustom <| Some product,
      //     {|
      //       Price = productPrice model item quality product
      //       Amount = Product.amountPerItem product
      //       Quality = if Product.processor product |> Processor.preservesQuality model.ModData then quality else Normal
      //     |}))

    let sellAs = Array.choose id [|
      rawPrices
      customPrices
      bestProducts
    |]

    if sellAs.Length = 0 then None else
    Quality.all
    |> Array.map (fun quality ->
      sellAs
      |> Array.map (Array.item (int quality))
      |> Array.maxBy snd)
    |> Some

  let itemForageBestPrice model seed item quality =
    if model.SellRawItems.Contains (seed, item)
    then Some <| itemForagePrice model (getItem model item) quality
    else None
    |> Option.max (itemBestProductPrice model seed item quality)
    |> Option.max (customSellPrice model seed item quality)

  let itemForageBestPrices model seed item =
    if model.SellRawItems.Contains (seed, item)
    then Some <| itemForagePrices model (getItem model item)
    else None
    |> Option.reduce (Qualities.map2 max) (itemBestProductPrices model seed item)
    |> Option.reduce (Qualities.map2 max) (customSellPrices model seed item)

  let itemForageBestProfit model seed item quality =
    if model.SellRawItems.Contains (seed, item)
    then Some (float <| itemForagePrice model (getItem model item) quality)
    else None
    |> Option.max (itemBestProductProfit model seed item quality)
    |> Option.max (customSellPrice model seed item quality |> Option.map float)

  let itemForageBestProfits model seed item =
    if model.SellRawItems.Contains (seed, item)
    then Some <| itemForagePrices model (getItem model item)
    else None
    |> Option.reduce (Qualities.map2 max) (itemBestProductProfits model seed item)
    |> Option.reduce (Qualities.map2 max) (customSellPrices model seed item)

  let private cropBestItemCalc comparisonValue crop =
    Crop.items crop |> Array.mapReduce max comparisonValue

  let cropBestItemPriceFrom model crop quality =
   crop |> cropBestItemCalc (fun item ->
    (if Crop.isForage crop
    then itemPrice
    else itemForagePrice)
      model (getItem model item) quality)

  let cropBestProductPriceFrom model crop quality processor =
    crop |> cropBestItemCalc (fun item ->
      model.Data.Products[item].TryFind processor |> Option.map (productPrice model (getItem model item) quality))

  let cropBestCustomPrice model crop (quality: Quality) =
    crop |> cropBestItemCalc (fun item ->
      model.CustomSellPrices.Values.TryFind (Crop.seed crop, item) |> Option.map (fun (price, q) ->
        if q
        then price |> withMultiplier Qualities.multipliers[quality]
        else price))


  let seedAmount model seed mainItem item =
    if int seed = int item then
      if model.UseRawSeeds.Contains seed
      then Some 1.0
      else None
    elif mainItem
      && Processor.seedMaker |> processorUnlocked model
      && model.UseSeedMaker.Contains seed
    then
      Some <| Processor.seedMakerAmount (seed * 1<_>)
    else
      None



  let farmingAmounts model amount farmingQualities =
    if giantCropsPossible model
    then CropAmount.farmingGiantAmounts model.Skills model.CropAmount amount farmingQualities
    else CropAmount.farmingAmounts model.Skills model.CropAmount amount farmingQualities

  let farmingAmounts' model amount fertilizer =
    let qualities = Skills.farmingQualitiesWith fertilizer model.Skills
    if giantCropsPossible model
    then CropAmount.farmingGiantAmounts model.Skills model.CropAmount amount qualities
    else CropAmount.farmingAmounts model.Skills model.CropAmount amount qualities

  let foragingAmounts model (crop: ForageCrop) =
    Skills.foragingAmounts model.Skills |> Qualities.map (fun a -> a / float crop.Items.Length)


  let private seedItemPrice model seed = itemPrice model (getSeedItem model seed) Normal

  let private forageSeedsProfit model seed = float (ForageCrop.forageSeedsPerCraft * seedItemPrice model seed)

  let private forageRawCropProfit profits i = profits |> Array.sumBy (Qualities.itemi i)

  // Needs tests, e.g. compared to using brute force or lp methods
  let private forageCropProfitPerHarvestIgnoreSeedsCalc model crop profits (amounts: Qualities) =
    if ForageCrop.seedsRecipeUnlocked model.Skills crop
      && model.SellForageSeeds.Contains crop.Growth.Seed
    then
      let forageSeedsProfit = forageSeedsProfit model crop.Growth.Seed

      // For all raw items and products: profitPerItem(low quality item/product) <= profitPerItem(high quality item/product)
      // Starting with the lowest quality, make forage seeds until the sum of the profits of each individual rawCrop is greater than the forage seeds profit.

      let mutable profit = 0.0
      let mutable i = 0

      while i < Quality.count && forageRawCropProfit profits i <= forageSeedsProfit do
        profit <- profit + forageSeedsProfit * amounts[i]
        i <- i + 1

      while i < Quality.count do
        assert (forageRawCropProfit profits i > forageSeedsProfit)
        profit <- profit + (forageRawCropProfit profits i) * amounts[i]
        i <- i + 1

      profit
    else
      profits |> Array.sumBy (Qualities.dot amounts)


  let private forageCropProfitPerHarvestIgnoreSeeds model crop =
    let amounts = foragingAmounts model crop
    let profits =
      crop.Items |> Array.map (itemForageBestProfits model crop.Growth.Seed >> Option.defaultValue Qualities.zero)
    forageCropProfitPerHarvestIgnoreSeedsCalc model crop profits amounts

  type ItemUsage =
    | BoughtSeeds
    | SoldItem of ItemId * Quality
    | MadeSeeds of ItemId * Quality
    | MadeForageSeeds of ItemId * Quality
    | SoldForageSeeds
    | UsedForageSeeds

  open YALPS
  open YALPS.Operators
  let private forageCropNetProfitPerHarvestForageSeedsSolution model (crop: ForageCrop) seedPrice (profits: Qualities array) (amounts: Qualities) =
    // This linear programming solution is perhaps only needed for the case when
    // both forage seeds and the seedmaker are selected+unlocked.
    // Since this solution also covers and solves all other cases for net profit as well,
    // it is reused for simplicity and reducing code size.

    let inline (@) a b = a + ("@" + b)

    let validQualities = Quality.all |> Array.filter (fun q -> amounts[q] > 0.0)
    let forageSeedAmounts = crop.Items |> Array.map (fun item -> string item @ "Forage Seeds")

    let constraints = Array.concat [|
      // one seed is created and stockpiled
      [| "Seeds" === 1.0 |]

      // all items of each quality are used -- is this needed?
      Array.allPairs crop.Items validQualities
      |> Array.map (fun (item, quality) -> string item @ string quality === amounts[quality])

      // all items set aside to create forage seeds are all used
      forageSeedAmounts |> Array.map (fun amount -> amount === 0.0)
    |]

    let variables = ResizeArray ()

    match seedPrice with
    | Some price -> variables.Add (BoughtSeeds, [| "Profit", -float price; "Seeds", 1.0 |])
    | None -> ()

    for item, profits, forageSeedAmount in Seq.zip3 crop.Items profits forageSeedAmounts do
      let seedAmount = seedAmount model crop.Growth.Seed (item = crop.Items[0]) item

      for quality in validQualities do
        let oneItem = string item @ string quality, 1.0
        let itemQuality = item, quality

        variables.Add (SoldItem itemQuality, [| "Profit", profits[quality]; oneItem |])
        variables.Add (MadeForageSeeds itemQuality, [| forageSeedAmount, -1.0; oneItem |])
        match seedAmount with
        | Some amount -> variables.Add (MadeSeeds itemQuality, [| "Seeds", amount; oneItem |])
        | None -> ()

    let oneOfEachItem = forageSeedAmounts |> Array.map (fun amount -> amount, 1.0)

    if model.SellForageSeeds.Contains crop.Growth.Seed then
      variables.Add (SoldForageSeeds, [| "Profit", forageSeedsProfit model crop.Growth.Seed; yield! oneOfEachItem |])

    if model.UseForageSeeds.Contains crop.Growth.Seed then
      variables.Add (UsedForageSeeds, [| "Seeds", float ForageCrop.forageSeedsPerCraft; yield! oneOfEachItem |])

    Solver.solve <| Model.create Maximize "Profit" constraints variables

  let forageCropNetProfitPerHarvestForageSeeds model crop seedPrice profits amounts =
    let solution = forageCropNetProfitPerHarvestForageSeedsSolution model crop seedPrice profits amounts
    if solution.status = Optimal
    then Some solution.result
    else None



  type [<System.Flags>] NoProfitReasons =
    | None              = 0b000
    | NoFertilizerPrice = 0b001
    | NoSeeds           = 0b010
    | NotEnoughDays     = 0b100
    | All               = 0b111

  type private NPR = NoProfitReasons

  let consecutiveHarvests' growthTime totalDays growth = Growth.consecutiveHarvestsWith growthTime totalDays growth |> Array.filter ((<>) 0u)

  let timeNormalizationDivisor dateSpans growthTime harvests growth = function
    | TotalPeriod -> 1.0
    | PerDay -> harvests |> Array.natSumBy (fun harvests -> Growth.daysUsedWith growthTime harvests growth) |> float
    | PerSeason -> float (dateSpans |> Array.natSumBy DateSpan.totalDays) / float Date.daysInSeason

  let seedCostsandLimits model (seedPrice: nat option) (seed: SeedId) (items: ItemId array) (profits: Qualities array) (amounts: Qualities array) =
    let costsAndLimits = ResizeArray ()
    let seedPrice = seedPrice |> Option.defaultOrMap System.Double.PositiveInfinity float
    let inline addCostAndLimit (profits: Qualities) (amounts: Qualities) seedAmount =
      for i = 0 to Quality.highest do
        let cost = profits[i] / seedAmount
        let amount = seedAmount * amounts[i]
        if cost < seedPrice && amount > 0.0 then
          costsAndLimits.Add (cost, amount)

    if Processor.seedMaker |> processorUnlocked model && model.UseSeedMaker.Contains seed then
      addCostAndLimit profits[0] amounts[0] (Processor.seedMakerAmount (seed * 1<_>))

    if model.UseRawSeeds.Contains seed then
      match items |> Array.tryFindIndex (fun item -> int item = int seed) with
      | Some i -> addCostAndLimit profits[i] amounts[i] 1.0
      | None -> ()

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

  let seedCost model seedPrice seed items profits amounts harvests = seedCostCalc seedPrice (seedCostsandLimits model seedPrice seed items profits amounts) harvests

  let private farmCropItemAmounts model (crop: FarmCrop) =
    let extraItemAmount = crop.ExtraItem |> Option.map (snd >> Qualities.normalSingleton)
    fun fertilizer ->
      let amounts = farmingAmounts' model crop.Amount fertilizer
      match extraItemAmount with
      | Some extra -> [| amounts; extra |]
      | None -> [| amounts |]

  let cropItemAmounts model = function
    | FarmCrop c -> farmCropItemAmounts model c
    | ForageCrop c ->
      foragingAmounts model c |> Array.create c.Items.Length |> konst

  let private farmCropProfitPerHarvestCalc profits amounts = Array.map2 Qualities.dot profits amounts |> Array.sum

  let cropItemProfits model seed items = items |> Array.map (itemBestProfits model seed >> Option.defaultValue Qualities.zero)

  let canUseSeedMakerForOwnSeeds model seed = Processor.seedMaker |> processorUnlocked model && model.UseSeedMaker.Contains seed

  let canUseForageSeeds model = function
    | ForageCrop crop -> ForageCrop.seedsRecipeUnlocked model.Skills crop && model.UseForageSeeds.Contains crop.Growth.Seed
    | FarmCrop _ -> false

  let nonRegrowData model seed =
    let crop = getCrop model seed
    let seedPrice = lowestSeedPrice model seed
    let hasSeedSource =
      seedPrice.IsSome
      || canUseSeedMakerForOwnSeeds model seed
      || model.UseRawSeeds.Contains seed

    match crop with
    | FarmCrop crop ->
      if not hasSeedSource then konst None else
      let items = FarmCrop.items crop
      let profits = items |> cropItemProfits model crop.Growth.Seed
      let amounts = farmCropItemAmounts model crop
      let seedCost = seedCost model seedPrice seed items profits
      if crop.Growth.RegrowTime.IsSome then konst None else
      fun fertilizer ->
        let amounts = amounts fertilizer
        match seedCost amounts 1u with
        | Some cost ->
          let profit = farmCropProfitPerHarvestCalc profits amounts
          Some (profit - cost)
        | None -> None
    | ForageCrop crop ->
      let seedsUnlocked = ForageCrop.seedsRecipeUnlocked model.Skills crop
      let useForageSeeds = model.UseForageSeeds.Contains seed
      let hasSeedSource = hasSeedSource || (seedsUnlocked && useForageSeeds)
      if not hasSeedSource then konst None else
      let amounts = foragingAmounts model crop
      let profits = crop.Items |> cropItemProfits model crop.Growth.Seed
      let net =
        if seedsUnlocked && (useForageSeeds || model.SellForageSeeds.Contains seed) then
          forageCropNetProfitPerHarvestForageSeeds model crop seedPrice profits amounts
        else
          let profit = profits |> Array.sumBy (Qualities.dot amounts)
          seedCost model seedPrice seed crop.Items profits (Array.create crop.Items.Length amounts) 1u
          |> Option.map (fun cost -> profit - cost)
      konst net

  let regrowSeedData model seed =
    let seedPrice = lowestSeedPrice model seed
    let crop = getCrop model seed
    let items = Crop.items crop
    let profits = cropItemProfits model seed items
    let amounts = cropItemAmounts model crop
    fun fertilizer ->
      let amounts = amounts fertilizer
      let profit = farmCropProfitPerHarvestCalc profits amounts
      let costsAndLimits = seedCostsandLimits model seedPrice seed items profits amounts
      costsAndLimits
      |> Array.tryHead
      |> Option.orElse (seedPrice |> Option.map (fun price -> float price, System.Double.MaxValue))
      |> Option.map (fun (cost, limit) -> {|
        Profit = profit
        HarvestsForMinCost = ceil (1.0 / limit) |> nat
        MinCost = cost
        Cost = seedCostCalc seedPrice costsAndLimits
      |})

  let cropProfitCalcNoForageSeeds netProfit model timeNormalization crop =
    let netProfit = netProfit model crop
    let growth = Crop.growth crop
    let growthSpans = growthSpans model crop
    fun fertilizer ->
      let fertCost = lowestFertilizerCostOpt model (Fertilizer.Opt.name fertilizer)
      let growthTime = cropGrowthTime model fertilizer crop
      let harvests = consecutiveHarvests' growthTime growthSpans growth
      match netProfit, fertCost with
      | Some netProfit, Some fertCost when harvests.Length > 0 ->
        match netProfit fertilizer harvests with
        | Some netProfit ->
          let fertilizerCost = float fertCost * (harvests |> Array.sumBy (fertilizerUsed model crop))
          let divisor = timeNormalizationDivisor growthSpans growthTime harvests growth timeNormalization
          Ok ((netProfit - fertilizerCost) / divisor)
        | None -> Error NPR.NoSeeds // unable to make enough seeds
      | _ ->
        ((if fertCost.IsNone then NPR.NoFertilizerPrice else NPR.None)
        ||| (if harvests.Length = 0 then NPR.NotEnoughDays else NPR.None)
        ||| (if netProfit.IsNone then NPR.NoSeeds else NPR.None))
        |> Error

  let cropProfitCalcNoForageSeedsIgnoreSeeds =
    cropProfitCalcNoForageSeeds
      (fun model crop ->
        (match crop with
        | FarmCrop crop ->
          let profits = FarmCrop.items crop |> cropItemProfits model crop.Growth.Seed
          let amounts = farmCropItemAmounts model crop
          fun fertilizer harvests ->
            Some ((amounts fertilizer |> farmCropProfitPerHarvestCalc profits) * (float <| Array.natSum harvests))
        | ForageCrop crop ->
          let profitPerHarvest = forageCropProfitPerHarvestIgnoreSeeds model crop
          fun _ harvests -> Some (profitPerHarvest * float (Array.natSum harvests)))
        |> Some)

  let cropProfitCalcNoForageSeedsStockpileSeeds =
    cropProfitCalcNoForageSeeds
      (fun model crop ->
        let seed = Crop.seed crop
        let seedPrice = lowestSeedPrice model seed
        let hasSeedSource =
          seedPrice.IsSome
          || canUseSeedMakerForOwnSeeds model seed
          || model.UseRawSeeds.Contains seed

        match crop with
        | FarmCrop crop ->
          if not hasSeedSource then None else
          let items = FarmCrop.items crop
          let profits = items |> cropItemProfits model crop.Growth.Seed
          let amounts = farmCropItemAmounts model crop
          let seedCost = seedCost model seedPrice seed items profits
          (if crop.Growth.RegrowTime.IsSome then
            fun fertilizer harvests ->
              let amounts = amounts fertilizer
              let profit = farmCropProfitPerHarvestCalc profits amounts
              harvests |> Array.mapReduce (Option.map2 (+)) (fun harvests ->
                match seedCost amounts harvests with
                | Some cost -> Some (profit * float harvests - cost)
                | None -> None)
           else
            fun fertilizer harvests ->
              let amounts = amounts fertilizer
              match seedCost amounts 1u with
              | Some cost ->
                let profit = farmCropProfitPerHarvestCalc profits amounts
                Some ((profit - cost) * float (Array.natSum harvests))
              | None -> None)
          |> Some
        | ForageCrop crop ->
          let seedsUnlocked = ForageCrop.seedsRecipeUnlocked model.Skills crop
          let useForageSeeds = model.UseForageSeeds.Contains seed
          let hasSeedSource = hasSeedSource || (seedsUnlocked && useForageSeeds)
          if not hasSeedSource then None else
          let amounts = foragingAmounts model crop
          let profits = crop.Items |> cropItemProfits model crop.Growth.Seed
          let net =
            if seedsUnlocked && (useForageSeeds || model.SellForageSeeds.Contains seed) then
              forageCropNetProfitPerHarvestForageSeeds model crop seedPrice profits amounts
            else
              let profit = profits |> Array.sumBy (Qualities.dot amounts)
              seedCost model seedPrice seed crop.Items profits (Array.create crop.Items.Length amounts) 1u
              |> Option.map (fun cost -> profit - cost)
          Some (fun _ harvests -> net |> Option.map ((*) (float <| Array.natSum harvests))))

  // refactor, there will always be a seed cost
  let cropProfitCalcNoForageSeedsBuyFirstSeed = cropProfitCalcNoForageSeeds (fun model crop ->
    let seed = Crop.seed crop
    match lowestSeedPrice model seed with
    | None -> None
    | Some seedPrice ->
      (match crop with
      | FarmCrop crop ->
        let items = FarmCrop.items crop
        let profits = items |> cropItemProfits model crop.Growth.Seed
        let amounts = farmCropItemAmounts model crop
        let seedCost = seedCost model (Some seedPrice) crop.Growth.Seed items profits
        if crop.Growth.RegrowTime.IsSome then
          fun fertilizer (harvests: nat array) ->
            let amounts = amounts fertilizer
            let profit = farmCropProfitPerHarvestCalc profits amounts
            let last = harvests.Length - 1
            let lastProfit = profit * float harvests[last] - float seedPrice
            harvests[..(last - 1)]
            |> Array.fold (fun sum harvests ->
              seedCost amounts harvests
              |> Option.map (fun cost -> profit * float harvests - cost)
              |> Option.map2 (+) sum)
              (Some 0.0)
            |> Option.map ((+) lastProfit)
        else
          fun fertilizer harvests ->
            let amounts = amounts fertilizer
            match seedCost amounts 1u with
            | Some cost ->
              let profit = farmCropProfitPerHarvestCalc profits amounts
              Some ((profit - cost) * float (Array.natSum harvests) + cost - float seedPrice)
            | None -> None
      | ForageCrop crop ->
        let amounts = foragingAmounts model crop
        let profits = crop.Items |> cropItemProfits model crop.Growth.Seed
        let profit = profits |> Array.sumBy (Qualities.dot amounts)
        let netAndcost =
          if ForageCrop.seedsRecipeUnlocked model.Skills crop
            && (model.UseForageSeeds.Contains seed || model.SellForageSeeds.Contains seed)
          then
            forageCropNetProfitPerHarvestForageSeeds model crop (Some seedPrice) profits amounts
            |> Option.map (fun net -> net, profit - net)
          else
            seedCost model (Some seedPrice) seed crop.Items profits (Array.create crop.Items.Length amounts) 1u
            |> Option.map (fun cost -> profit - cost, cost)
        fun _ harvests ->
          match netAndcost with
          | Some (net, cost) ->
            Some (net * float (Array.natSum harvests) + cost - float seedPrice)
          | None -> None)
      |> Some)

  let cropProfit model timeNorm crop =
    match model.SeedStrategy with
    | IgnoreSeeds -> cropProfitCalcNoForageSeedsIgnoreSeeds model timeNorm crop
    | StockpileSeeds -> cropProfitCalcNoForageSeedsStockpileSeeds model timeNorm crop
    | BuyFirstSeed -> cropProfitCalcNoForageSeedsBuyFirstSeed model timeNorm crop

  let cropXP model timeNorm crop =
    let growth = Crop.growth crop
    let growthSpans = growthSpans model crop

    let xpPerHarvest = Crop.xpPerHarvest (Skills.botanistActive model.Skills) (getItem model) crop

    fun fertilizer ->
      let fertCost = lowestFertilizerCostOpt model (Fertilizer.Opt.name fertilizer)
      let growthTime = growthTime model fertilizer growth
      let harvests = consecutiveHarvests' growthTime growthSpans growth
      if harvests.Length > 0 && fertCost.IsSome then
        let xp = float (Array.natSum harvests) * xpPerHarvest
        let divisor = timeNormalizationDivisor growthSpans growthTime harvests growth timeNorm
        Ok (xp / divisor)
      else
        ((if fertCost.IsNone then NPR.NoFertilizerPrice else NPR.None)
        ||| (if harvests.Length = 0 then NPR.NotEnoughDays else NPR.None))
        |> Error


  let cropROISimple model timeNormalization crop =
    let growth = Crop.growth crop
    let seed = growth.Seed
    let seedPrice = lowestSeedPrice model seed
    let growthSpans = growthSpans model crop

    let netProfit =
      match seedPrice with
      | None | Some 0u -> None
      | Some seedPrice ->
        (match crop with
        | FarmCrop crop ->
          let items = FarmCrop.items crop
          let profits = items |> cropItemProfits model seed
          let amounts = farmCropItemAmounts model crop
          let seedCost = seedCost model (Some seedPrice) seed items profits
          if growth.RegrowTime.IsSome then
            fun fertilizer (harvests: nat array) ->
              let amounts = amounts fertilizer
              let profit = farmCropProfitPerHarvestCalc profits amounts
              let last = harvests.Length - 1
              let lastProfit = profit * float harvests[last]
              let profit =
                harvests[..(last - 1)] |> Array.sumBy (fun harvests ->
                  let cost = seedCost amounts harvests |> Option.get
                  profit * float harvests - cost)
              profit + lastProfit
          else
            fun fertilizer harvests ->
              let amounts = amounts fertilizer
              let cost = seedCost amounts 1u |> Option.get
              let profit = farmCropProfitPerHarvestCalc profits amounts
              (profit - cost) * float (Array.natSum harvests) + cost
        | ForageCrop crop ->
          let amounts = foragingAmounts model crop
          let profits = crop.Items |> cropItemProfits model seed
          let profit = profits |> Array.sumBy (Qualities.dot amounts)
          let net, cost =
            if ForageCrop.seedsRecipeUnlocked model.Skills crop
              && (model.UseForageSeeds.Contains seed || model.SellForageSeeds.Contains seed)
            then
              let net = forageCropNetProfitPerHarvestForageSeeds model crop (Some seedPrice) profits amounts |> Option.get
              net, profit - net
            else
              let cost = seedCost model (Some seedPrice) seed crop.Items profits (Array.create crop.Items.Length amounts) 1u |> Option.get
              profit - cost, cost
          fun _ harvests ->
            net * float (Array.natSum harvests) + cost)
        |> Some

    fun fertilizer ->
      let fertCost = lowestFertilizerCostOpt model (Fertilizer.Opt.name fertilizer)
      let growthTime = growthTime model fertilizer growth
      let harvests = consecutiveHarvests' growthTime growthSpans growth
      match seedPrice, fertCost with
      | Some seedPrice, Some fertCost when harvests.Length > 0 && seedPrice > 0u ->
        let profit = (Option.get netProfit) fertilizer harvests
        let fertilizerCost = float fertCost * (harvests |> Array.sumBy (fertilizerUsed model crop))
        let netProfit = profit - fertilizerCost
        let investment = float seedPrice + float fertCost
        let divisor = timeNormalizationDivisor growthSpans growthTime harvests growth timeNormalization
        Ok (((netProfit - investment) / investment) / divisor)
      | _ ->
        ((if fertCost.IsNone then NPR.NoFertilizerPrice else NPR.None)
        ||| (if harvests.Length = 0 then NPR.NotEnoughDays else NPR.None)
        ||| (if netProfit.IsNone then NPR.NoSeeds else NPR.None))
        |> Error

  type DateSpanHarvestData = {
    DateSpan: DateSpan
    Harvests: nat
    SeedsBought: float
    FertilizerBought: float
    SoldAmounts: Qualities array
    IntoSeedAmounts: (ItemId * Qualities) array
    ForageSeedsMade: float
    NetProfit: float option
  }

  type HarvestsData = {
    TotalNetProfit: float option
    TimeNormalization: float
    SeedPrice: nat option
    FertilizerPrice: nat option
    SellAs: (CustomChoice<Product option, nat * bool> * float) array option array
    ConsecutiveHarvests: DateSpanHarvestData array
  }

  let private seedData
    model
    seedPrice
    (seed: SeedId)
    (items: ItemId array)
    (profits: Qualities array)
    (amounts: Qualities array)
    (harvests: nat)
    =
    let amountsUsed = ResizeArray ()

    let data = ResizeArray ()
    let seedPrice = seedPrice |> Option.defaultOrMap System.Double.PositiveInfinity float
    let addCostAndLimit item seedAmount =
      let profits = profits[item]
      let amounts = amounts[item]
      for i = 0 to Quality.highest do
        let cost = profits[i] / seedAmount
        let amount = amounts[i]
        if cost < seedPrice && amount > 0.0 then
          data.Add {|
            Index = amountsUsed.Count
            Quality = enum<Quality> i
            Cost = cost
            AmountUsedPerSeed = seedAmount
            MaxSeedsPerHarvest = amount * seedAmount
          |}
      amountsUsed.Add (item, Array.zeroCreate Quality.count)

    let seedItemIndex =
      if model.UseRawSeeds.Contains seed
      then items |> Array.findIndex (fun item -> int item = int seed) |> Some
      else None
    match seedItemIndex with
    | Some i -> addCostAndLimit i 1.0
    | None -> ()

    let useSeedMaker = Processor.seedMaker |> processorUnlocked model && model.UseSeedMaker.Contains seed
    if useSeedMaker then addCostAndLimit 0 (Processor.seedMakerAmount items[0])

    let data = resizeToArray data
    data |> Array.sortInPlaceWith (compareBy (fun data -> data.Cost))

    let mutable seedsLeft = 1.0
    let mutable i = 0
    while i < data.Length && seedsLeft > 0.0 do
      let data = data[i]
      let seedsMade = (data.MaxSeedsPerHarvest * float harvests) |> min seedsLeft
      let _, usage = amountsUsed[data.Index]
      usage[int data.Quality] <- seedsMade / data.AmountUsedPerSeed / float harvests
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

    // if seedsLeft = 0.0 then Some totalCost else
    // match seedPrice with
    // | Some price ->
    //   Some (totalCost + float price * seedsLeft)
    // | None -> None


  // needs to react based on seedMode
  let cropProfitData model timeNormalization crop fertilizer =
    let growth = Crop.growth crop
    let seed = growth.Seed
    let growthSpans = growthSpans model crop
    let growthTime = Growth.time growth (growthSpeed model fertilizer growth)
    let harvestData = Growth.consecutiveHarvestsData growthSpans (growthSpeed model fertilizer growth) growth

    let regrowTime = Crop.regrowTime crop
    let seedPrice = lowestSeedPrice model seed

    let items = Crop.items crop
    let sellAs = items |> Array.map (itemBestProfitsSellAs model seed)
    // let profits = cropItemProfits model seed items
    let profits =
      sellAs |> Array.map (function
        | Some sellAs ->
          Qualities.initi (fun i -> snd sellAs[i])
        | None -> Qualities.zero)
    let amounts = cropItemAmounts model crop

    let fertCost = lowestFertilizerCostOpt model (Fertilizer.Opt.name fertilizer)
    let amounts = amounts fertilizer
    let data = growthSpans |> Array.choose (fun span ->
      let harvests = Growth.harvestsWith growthTime span.TotalDays growth
      if harvests = 0u then None else
      let seedData = seedData model seedPrice seed items profits amounts (if regrowTime.IsSome then harvests else 1u)
      let fertilizerBought = fertilizerUsed model crop harvests
      let fertCost = fertCost |> Option.map (float >> (*) fertilizerBought)
      let seedCost =
        if seedData.SeedsBought = 0.0
        then Some 0.0
        else seedPrice |> Option.map (float >> (*) (seedData.SeedsBought * if regrowTime.IsSome then 1.0 else float harvests))
      let net =
        Option.map2 (fun fertCost seedCost -> float harvests * farmCropProfitPerHarvestCalc profits amounts - fertCost - seedCost)
          fertCost
          seedCost
      Some {
        DateSpan = span
        Harvests = harvests
        SoldAmounts = seedData.Sold
        IntoSeedAmounts = seedData.IntoSeeds
        FertilizerBought = fertilizerBought
        SeedsBought = seedData.SeedsBought
        ForageSeedsMade = 0.0
        NetProfit = net
      } )

    {
      TotalNetProfit =
        if data.Length = 0
        then None
        else data |> Array.mapReduce Option.sum (fun data -> data.NetProfit)
      TimeNormalization = timeNormalizationDivisor growthSpans harvestData.GrowthTime harvestData.Harvests growth timeNormalization
      SeedPrice = seedPrice
      FertilizerPrice = fertCost
      SellAs = sellAs
      ConsecutiveHarvests = data
    }
