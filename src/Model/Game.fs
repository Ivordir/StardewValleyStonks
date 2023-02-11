namespace StardewValleyStonks

// Bulk data extracted from the game's xnb files.
type ExtractedData = {
  Items: Item array
  Products: Table<ItemId, ProcessedItem array>
  FarmCrops: FarmCrop array
  ForageCrops: ForageCrop array
}

// Data found / inputted manually by digging through game code.
type SupplementalData = {
  Fertilizers: Fertilizer array
  FertilizerPrices: Table<FertilizerName, Table<Vendor, nat>>
  GenerateSeedPrices: Table<Vendor, SeedId array>
  SeedPrices: Table<SeedId, SeedPrice array>
  ProcessorUnlockLevel: Table<Processor, nat>
}

// The model type for all of Stardew Valley's relevant game data/content. (I.e., this does not include save game data.)
type GameData = {
  Fertilizers: Table<FertilizerName, Fertilizer>
  FertilizerPrices: Table<FertilizerName, Table<Vendor, nat>>
  Crops: Table<SeedId, Crop>
  FarmCrops: Table<SeedId, FarmCrop>
  ForageCrops: Table<SeedId, ForageCrop>
  SeedPrices: Table<SeedId, Table<Vendor, SeedPrice>>
  Seed: Table<ItemId, SeedId>
  Items: Table<ItemId, Item>
  Products: Table<ItemId, Table<Processor, ProcessedItem>>
  ProcessorUnlockLevel: Table<Processor, nat>
}
// Assume that the seedMaker is the only processor which converts items into seeds.

[<RequireQualifiedAccess>]
module GameData =
  let seedItemPairsFrom crops =
    crops
    |> Seq.map (fun crop ->
      let seed = Crop.seed crop
      Crop.items crop |> Array.map (fun item -> seed, item))
    |> Array.concat

  let seedItemPairs data = seedItemPairsFrom data.Crops.Values

  let private implicitProduct data item processor =
    let itemId = item.Id
    match Item.category item, processor with
    | Fruit, ProcessorName "Preserves Jar" -> Some (Jam itemId)
    | Fruit, ProcessorName "Keg" -> Some (Wine itemId)
    | Vegetable, ProcessorName "Preserves Jar" -> Some (Pickles itemId)
    | Vegetable, ProcessorName "Keg" -> Some (Juice itemId)
    | _, ProcessorName "Seed Maker" -> data.Seed.TryFind itemId |> Option.map (convertUnit >> SeedsFromSeedMaker)
    | _ -> None

  let product data item processor =
    match data.Products.TryFind item |> Option.bind (Table.tryFind processor) with
    | Some product -> Some (Processed product)
    | None -> implicitProduct data data.Items[item] processor

  let products data item =
    let products = data.Products.TryFind item
    let item = data.Items[item]
    let implicit =
      [|
        Processor.preservesJar
        Processor.keg
        Processor.seedMaker
      |]
      |> Array.choose (fun processor ->
        if products |> Option.exists (Table.containsKey processor)
        then None
        else implicitProduct data item processor)

    match products with
    | Some products ->
      products.Values
      |> Seq.map Processed
      |> Array.ofSeq
      |> Array.append implicit
    | None -> implicit

  let fromExtractedAndSupplementalData (extracted: ExtractedData) (supplemental: SupplementalData) =
    let items = extracted.Items |> Table.ofValues Item.id

    let crops =
      extracted.FarmCrops
      |> Array.map FarmCrop
      |> Array.append (extracted.ForageCrops |> Array.map ForageCrop)

    let generatedPrices =
      supplemental.GenerateSeedPrices
      |> Table.toSeq
      |> Seq.collect (fun (vendor, seeds) ->
        seeds |> Array.map (fun seed -> seed, ScalingPrice (vendor, None)))
      |> Seq.groupBy fst
      |> Seq.map (fun (seed, prices) -> seed, prices |> Seq.map snd)
      |> Table.ofSeq

    let seedPrices =
      crops
      |> Seq.map Crop.seed
      |> Table.ofKeys (fun seed ->
        supplemental.SeedPrices.TryFind seed
        |> Option.defaultValue Array.empty
        |> Seq.append (generatedPrices.TryFind seed |> Option.defaultValue Seq.empty)
        |> Table.ofValues SeedPrice.vendor)

    let seeds =
      crops
      |> Seq.choose (fun crop ->
        if Crop.canGetOwnSeedsFromSeedMaker crop
        then Some (Crop.mainItem crop, Crop.seed crop)
        else None)
      |> Table.ofSeq

    let products =
      extracted.Products
      |> Table.toSeq
      |> Seq.map (fun (item, products) -> item, products |> Table.ofValues ProcessedItem.processor)
      |> Table.ofSeq

    {
      Fertilizers = supplemental.Fertilizers |> Table.ofValues Fertilizer.name
      FertilizerPrices =
        supplemental.Fertilizers
        |> Seq.map Fertilizer.name
        |> Table.ofKeys (supplemental.FertilizerPrices.TryFind >> Option.defaultWith Table.empty)

      Crops = crops |> Table.ofValues Crop.seed
      FarmCrops = extracted.FarmCrops |> Table.ofValues FarmCrop.seed
      ForageCrops = extracted.ForageCrops |> Table.ofValues ForageCrop.seed
      SeedPrices = seedPrices

      Seed = seeds
      Items = items
      Products = products
      ProcessorUnlockLevel = supplemental.ProcessorUnlockLevel
    }


type Location =
  | Farm
  | Greenhouse
  | [<CompiledName ("Ginger Island")>] GingerIsland

// Settings / parameters about the state of the (save) game.
// Also includes other context such as the location to plant at and mod data.
type GameVariables = {
  Skills: Skills
  Multipliers: Multipliers
  ModData: ModData
  CropAmount: CropAmountSettings
  JojaMembership: bool
  Irrigated: bool
  StartDate: Date
  EndDate: Date
  Location: Location
}

[<RequireQualifiedAccess>]
module GameVariables =
  let common = {
    Skills = Skills.zero
    Multipliers = Multipliers.common
    ModData = ModData.common
    CropAmount = CropAmountSettings.common
    JojaMembership = false
    Irrigated = false
    StartDate = { Season = Season.Spring; Day = Date.firstDay }
    EndDate = { Season = Season.Fall; Day = Date.lastDay }
    Location = Farm
  }


[<RequireQualifiedAccess>]
module Game =
  let seasons vars = Date.seasonsBetween vars.StartDate vars.EndDate

  let cropIsInSeason vars crop = vars.Location <> Farm || crop |> Crop.growsInSeasons (seasons vars)

  let inSeasonCrops vars crops =
    if vars.Location = Farm
    then crops |> Seq.filter (Crop.growsInSeasons (seasons vars))
    else crops

  let growthMultiplier vars crop =
    (if vars.Skills |> Skills.professionActive Agriculturist then Multiplier.agriculturist else 0.0)
    + if vars.Irrigated && Crop.paddy crop then Multiplier.irrigated else 0.0

  let growthSpeed vars fertilizer crop = Fertilizer.Opt.speed fertilizer + growthMultiplier vars crop
  let growthTimeAndStages vars fertilizer crop = Crop.growthStagesAndTime (growthSpeed vars fertilizer crop) crop
  let growthTime vars fertilizer crop = growthTimeAndStages vars fertilizer crop |> snd

  let giantCropsPossible location = location <> Greenhouse

  let giantCropProb vars =
    if giantCropsPossible vars.Location
    then CropAmount.giantCropProb vars.CropAmount
    else 0.0

  let farmCropFertilizerLossProb vars crop =
    if crop.Giant && giantCropsPossible vars.Location
    then Fertilizer.lossProbability * CropAmount.giantCropProb vars.CropAmount
    else 0.0

  let forageCropFertilizerLossProb location =
    if location = GingerIsland
    then Fertilizer.lossProbability
    else 0.0

  let fertilizerLossProb vars = function
    | FarmCrop crop -> farmCropFertilizerLossProb vars crop
    | ForageCrop _ -> forageCropFertilizerLossProb vars.Location

  let farmCropMainItemAmount vars crop =
    if crop.Giant && giantCropsPossible vars.Location
    then CropAmount.expectedGiantAmount vars.Skills vars.CropAmount crop.Amount
    else CropAmount.expectedAmount vars.Skills vars.CropAmount crop.Amount

  let farmCropMainItemAmountByQuality vars fertilizer crop =
    let qualities = Skills.farmCropQualitiesWith fertilizer vars.Skills
    if crop.Giant && giantCropsPossible vars.Location
    then CropAmount.expectedGiantAmountByQuality vars.Skills vars.CropAmount crop.Amount qualities
    else CropAmount.expectedAmountByQuality vars.Skills vars.CropAmount crop.Amount qualities

  let forageCropItemAmountByQuality vars (crop: ForageCrop) =
    Skills.forageCropHarvestAmounts vars.Skills |> Qualities.map (fun a -> a / float crop.Items.Length)

  let farmCropItemAmountsByQuality vars fertilizer crop =
    let amounts = farmCropMainItemAmountByQuality vars fertilizer crop
    match crop.ExtraItem with
    | Some (_, amount) -> [| amounts; Qualities.zero |> Qualities.addNormal amount |]
    | None -> [| amounts |]

  let forageCropMainItemAmount vars (crop: ForageCrop) =
    ForageCrop.xpItemsPerHarvest vars.Skills / float crop.Items.Length

  let cropMainItemAmount vars = function
    | FarmCrop crop -> farmCropMainItemAmount vars crop
    | ForageCrop crop -> forageCropMainItemAmount vars crop

  let cropMainItemAmountByQuality vars fertilizer = function
    | FarmCrop crop -> farmCropMainItemAmountByQuality vars fertilizer crop
    | ForageCrop crop -> forageCropItemAmountByQuality vars crop

  let cropItemAmountsByQuality vars fertilizer = function
    | FarmCrop crop -> farmCropItemAmountsByQuality vars fertilizer crop
    | ForageCrop crop -> forageCropItemAmountByQuality vars crop |> Array.create crop.Items.Length

  let processorUnlocked data vars processor =
    vars.Skills.IgnoreSkillLevelRequirements
    || data.ProcessorUnlockLevel.TryFind processor |> Option.forall (fun l -> l <= vars.Skills.Farming.Level)

  let productUnlocked data vars = Product.processor >> processorUnlocked data vars

  let seedPrice data vars (seed: SeedId) = function
    | FixedPrice (_, price) -> price
    | ScalingPrice (vendor, price) ->
      let price =
        match price with
        | Some price -> price
        | None -> 2u * data.Items[convertUnit seed].SellPrice

      if vendor = Vendor.joja
      then price |> withMultiplier (vars.Multipliers.ProfitMargin * if vars.JojaMembership then 1.0 else 1.25)
      else price |> withMultiplier vars.Multipliers.ProfitMargin |> max 1u

  let itemPrice vars forage item quality = Item.price vars.Skills vars.Multipliers forage item quality

  let itemPriceByQuality vars forage item = Item.priceByQuality vars.Skills vars.Multipliers forage item

  let productPriceAndQuality data vars quality product =
    Product.priceAndQuality data.Items.Find vars.Skills vars.Multipliers vars.ModData quality product

  let productPrice data vars quality product =
    Product.price data.Items.Find vars.Skills vars.Multipliers vars.ModData quality product

  let productPriceByQuality data vars product =
    Product.priceByQuality data.Items.Find vars.Skills vars.Multipliers vars.ModData product

  let productNormalizedPrice data vars quality product =
    Product.normalizedPrice data.Items.Find vars.Skills vars.Multipliers vars.ModData quality product

  let productNormalizedPriceByQuality data vars product =
    Product.normalizedPriceByQuality data.Items.Find vars.Skills vars.Multipliers vars.ModData product

  let productQuality vars quality product = Product.outputQuality vars.ModData quality product

  let seedItemSellPrice data vars (seed: SeedId) = itemPrice vars false data.Items[convertUnit seed] Quality.Normal

  let xpPerItem data crop = Crop.xpPerItem data.Items.Find crop
  let xpItemsPerHarvest vars crop = Crop.xpItemsPerHarvest (giantCropProb vars) vars.Skills crop
  let xpPerHarvest data vars crop = Crop.xpPerHarvest data.Items.Find (giantCropProb vars) vars.Skills crop
