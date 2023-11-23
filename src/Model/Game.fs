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

// The model type for all of Stardew Valley's relevant game data/content.
// (I.e., this does not include save game data.)
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
  let private implicitProduct data item processor =
    let itemId = item.Id
    match item.Category, processor with
    | Fruit, "Preserves Jar" -> Some (Jam itemId)
    | Fruit, "Keg" -> Some (Wine itemId)
    | Vegetable, "Preserves Jar" -> Some (Pickles itemId)
    | Vegetable, "Keg" -> Some (Juice itemId)
    | _, "Seed Maker" -> data.Seed.TryFind itemId |> Option.map (toItem >> SeedsFromSeedMaker)
    | _ -> None

  let product data processor item =
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

    (implicit, products) ||> Option.fold (fun implicit products ->
      products.Values
      |> Seq.map Processed
      |> Array.ofSeq
      |> Array.append implicit)

  let fromExtractedAndSupplementalData (extracted: ExtractedData) (supplemental: SupplementalData) =
    let items = extracted.Items |> Table.ofValues _.Id

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
        if Crop.canGetOwnSeedsFromSeedMaker items.Find crop
        then Some (Crop.mainItem crop, Crop.seed crop)
        else None)
      |> Table.ofSeq

    let products =
      extracted.Products
      |> Table.toSeq
      |> Seq.map (fun (item, products) -> item, products |> Table.ofValues _.Processor)
      |> Table.ofSeq

    {
      Fertilizers = supplemental.Fertilizers |> Table.ofValues _.Name
      FertilizerPrices =
        supplemental.Fertilizers
        |> Seq.map _.Name
        |> Table.ofKeys (supplemental.FertilizerPrices.TryFind >> Option.defaultWith Table.empty)

      Crops = crops |> Table.ofValues Crop.seed
      FarmCrops = extracted.FarmCrops |> Table.ofValues _.Seed
      ForageCrops = extracted.ForageCrops |> Table.ofValues _.Seed
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

module Location =
  let growsGiantCrops = function
    | Farm -> true
    | Greenhouse | GingerIsland -> false

  let forageCropDestroyFertilizerProb = function
    | GingerIsland -> Fertilizer.destroyProbability
    | Farm | Greenhouse -> 0.0


// Settings / parameters about the state of the (save) game.
// Also includes other context such as the location to plant at and mod data.
type GameVariables = {
  StartDate: Date
  EndDate: Date
  Location: Location
  Skills: Skills
  Multipliers: Multipliers
  CropAmount: CropAmountSettings
  JojaMembership: bool
  Irrigated: bool
  QualityArtisanProducts: bool
}

[<RequireQualifiedAccess>]
module GameVariables =
  let common = {
    StartDate = { Season = Season.Spring; Day = Date.firstDay }
    EndDate = { Season = Season.Winter; Day = Date.lastDay }
    Location = Farm
    Skills = Skills.zero
    Multipliers = Multipliers.common
    CropAmount = CropAmountSettings.common
    JojaMembership = false
    Irrigated = false
    QualityArtisanProducts = false
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

  let giantCropProb vars =
    if Location.growsGiantCrops vars.Location
    then CropAmount.giantCropProb vars.CropAmount
    else 0.0

  let farmCropDestroyFertilizerProb vars crop =
    if crop.Giant && Location.growsGiantCrops vars.Location
    then Fertilizer.destroyProbability * CropAmount.giantCropProb vars.CropAmount
    else 0.0

  let destroyFertilizerProb vars = function
    | FarmCrop crop -> farmCropDestroyFertilizerProb vars crop
    | ForageCrop _ -> Location.forageCropDestroyFertilizerProb vars.Location

  let forageCropSeedRecipeUnlocked vars crop =
    vars.Skills.IgnoreSkillLevelRequirements || ForageCrop.seedRecipeUnlocked vars.Skills crop

  let farmCropMainItemQuantity vars crop =
    if crop.Giant && Location.growsGiantCrops vars.Location
    then CropAmount.expectedGiantQuantity vars.Skills vars.CropAmount crop.Amount
    else CropAmount.expectedQuantity vars.Skills vars.CropAmount crop.Amount

  let farmCropMainItemQuantityByQuality vars fertilizer crop =
    let qualities = Skills.farmCropQualitiesWith fertilizer vars.Skills
    if crop.Giant && Location.growsGiantCrops vars.Location
    then CropAmount.expectedGiantQuantityByQuality vars.Skills vars.CropAmount crop.Amount qualities
    else CropAmount.expectedQuantityByQuality vars.Skills vars.CropAmount crop.Amount qualities

  let forageCropItemQuantityByQuality vars crop =
    Skills.forageCropHarvestQuantities vars.Skills |> Qualities.map (fun a -> a / float crop.Foragables.Length)

  let farmCropItemQuantitiesByQuality vars fertilizer crop =
    let quantity = farmCropMainItemQuantityByQuality vars fertilizer crop
    match crop.ExtraItem with
    | Some (_, extraQuantity) -> [| quantity; Qualities.zero |> Qualities.addNormal extraQuantity |]
    | None -> [| quantity |]

  let forageCropMainItemQuantity vars crop =
    ForageCrop.xpItemsPerHarvest vars.Skills / float crop.Foragables.Length

  let cropMainItemQuantity vars = function
    | FarmCrop crop -> farmCropMainItemQuantity vars crop
    | ForageCrop crop -> forageCropMainItemQuantity vars crop

  let cropMainItemQuantityByQuality vars fertilizer = function
    | FarmCrop crop -> farmCropMainItemQuantityByQuality vars fertilizer crop
    | ForageCrop crop -> forageCropItemQuantityByQuality vars crop

  let cropItemQuantitiesByQuality vars fertilizer = function
    | FarmCrop crop -> farmCropItemQuantitiesByQuality vars fertilizer crop
    | ForageCrop crop -> forageCropItemQuantityByQuality vars crop |> Array.create crop.Foragables.Length

  let processorUnlocked data vars processor =
    vars.Skills.IgnoreSkillLevelRequirements
    || data.ProcessorUnlockLevel.TryFind processor |> Option.forall (fun l -> l <= vars.Skills.Farming.Level)

  let productUnlocked data vars = Product.processor >> processorUnlocked data vars

  let seedPrice data vars seed = function
    | FixedPrice (_, price) -> price
    | ScalingPrice (vendor, price) ->
      let price =
        match price with
        | Some price -> price
        | None -> 2u * data.Items[toItem seed].SellPrice

      if vendor = Vendor.joja
      then price |> withMultiplier (vars.Multipliers.ProfitMargin * if vars.JojaMembership then 1.0 else 1.25)
      else price |> withMultiplier vars.Multipliers.ProfitMargin |> max 1u

  let itemPrice vars forage item quality = Item.price vars.Skills vars.Multipliers forage item quality

  let itemPriceByQuality vars forage item = Item.priceByQuality vars.Skills vars.Multipliers forage item

  let productPriceAndQuality data vars quality product =
    Product.priceAndQuality data.Items.Find vars.Skills vars.Multipliers vars.QualityArtisanProducts quality product

  let productPrice data vars quality product =
    Product.price data.Items.Find vars.Skills vars.Multipliers vars.QualityArtisanProducts quality product

  let productPriceByQuality data vars product =
    Product.priceByQuality data.Items.Find vars.Skills vars.Multipliers vars.QualityArtisanProducts product

  let productNormalizedPrice data vars quality product =
    Product.normalizedPrice data.Items.Find vars.Skills vars.Multipliers vars.QualityArtisanProducts quality product

  let productNormalizedPriceByQuality data vars product =
    Product.normalizedPriceByQuality data.Items.Find vars.Skills vars.Multipliers vars.QualityArtisanProducts product

  let productQuality data vars quality product =
    Product.outputQuality data.Items.Find vars.QualityArtisanProducts quality product

  let seedItemSellPrice data vars seed = itemPrice vars false data.Items[toItem seed] Quality.Normal

  let xpPerItem data crop = Crop.xpPerItem data.Items.Find crop
  let xpItemsPerHarvest vars crop = Crop.xpItemsPerHarvest (giantCropProb vars) vars.Skills crop
  let xpPerHarvest data vars crop = Crop.xpPerHarvest data.Items.Find (giantCropProb vars) vars.Skills crop
