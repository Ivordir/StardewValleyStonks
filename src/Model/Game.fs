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
  Items: Table<ItemId, Item>
  Products: Table<ItemId, Table<Processor, Product>>
  ProcessorUnlockLevel: Table<Processor, nat>
}
// Assume that the seedMaker is the only processor which converts items into seeds.

module GameData =
  let seedItemPairs data =
    data.Crops
    |> Table.toSeq
    |> Seq.map (fun (seed, crop) ->
      Crop.items crop |> Array.map (fun item -> seed, item))
    |> Array.concat

  let cropCanGetOwnSeedsFromSeedMaker crop data =
    crop |> Crop.canGetOwnSeedsFromSeedMaker data.Items.Find

  let missingItemIds data =
    let crops =
      data.FarmCrops.Values
      |> Seq.map FarmCrop
      |> Seq.append (data.ForageCrops.Values |> Seq.map ForageCrop)
      |> Array.ofSeq

    [|
      crops |> Array.collect Crop.items
      crops |> Array.map Crop.seedItem
      data.Products.Values
      |> Seq.collect Table.values
      |> Seq.choose Product.item
      |> Array.ofSeq
    |]
    |> Array.concat
    |> Array.filter (data.Items.ContainsKey >> not)

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

    let products =
      let seedMakerItems =
        crops
        |> Seq.choose (fun crop ->
          if Crop.canGetOwnSeedsFromSeedMaker items.Find crop
          then Some (Crop.mainItem crop, SeedsFromSeedMaker (Crop.seedItem crop))
          else None)
        |> Table.ofSeq

      items.Keys |> Table.ofKeys (fun item ->
        let generate =
          match items[item].Category with
          | Vegetable -> [| Pickles; Juice |]
          | Fruit -> [| Jam; Wine |]
          | _ -> [||]

        [|
          extracted.Products.TryFind item |> Option.defaultOrMap Array.empty (Array.map Processed)
          seedMakerItems.TryFind item |> Option.toArray
          generate
        |]
        |> Array.concat
        |> Table.ofValues Product.processor)

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

module [<RequireQualifiedAccess>] GameVariables =
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



module Game =
  let seasons vars = Date.seasonsBetween vars.StartDate vars.EndDate

  let cropIsInSeason vars crop =
    vars.Location <> Farm || crop |> Crop.growsInSeasons (seasons vars)

  let inSeasonCrops vars crops =
    if vars.Location = Farm then
      let seasons = seasons vars
      crops |> Seq.filter (Crop.growsInSeasons seasons)
    else
      crops

  let growthMultiplier vars crop =
    (if vars.Skills |> Skills.professionActive Agriculturist then Multiplier.agriculturist else 0.0)
    + if vars.Irrigated && Crop.paddy crop then Multiplier.irrigated else 0.0

  let growthSpeed vars fertilizer crop =
    Fertilizer.Opt.speed fertilizer + growthMultiplier vars crop

  let growthTimeAndStages vars fertilizer crop =
    Crop.stagesAndTime (growthSpeed vars fertilizer crop) crop

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
    | FarmCrop c -> farmCropFertilizerLossProb vars c
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
    | Some (_, amount) -> [| amounts; Qualities.zero |> Qualities.updateQuality Quality.Normal amount |]
    | None -> [| amounts |]

  let forageCropMainItemAmount vars (crop: ForageCrop) = ForageCrop.xpItemsPerHarvest vars.Skills / float crop.Items.Length

  let cropMainItemAmount vars = function
    | FarmCrop crop -> farmCropMainItemAmount vars crop
    | ForageCrop crop -> forageCropMainItemAmount vars crop

  let cropMainItemAmountByQuality vars fertilizer = function
    | FarmCrop crop -> farmCropMainItemAmountByQuality vars fertilizer crop
    | ForageCrop crop -> forageCropItemAmountByQuality vars crop

  let cropItemAmountsByQuality vars fertilizer = function
    | FarmCrop crop -> farmCropItemAmountsByQuality vars fertilizer crop
    | ForageCrop crop -> forageCropItemAmountByQuality vars crop |> Array.create crop.Items.Length

  let processorUnlocked data skills processor =
    skills.IgnoreSkillLevelRequirements
    || data.ProcessorUnlockLevel.TryFind processor |> Option.forall (fun l -> l <= skills.Farming.Level)

  let productUnlocked data vars = Product.processor >> processorUnlocked data vars

  let seedPrice data vars (seed: SeedId) = function
    | FixedPrice (_, price) -> price
    | ScalingPrice (vendor, price) ->
      let price =
        match price with
        | Some price -> price
        | None -> 2u * (data.Items[seed * 1u<_>] |> Item.sellPrice)
      if vendor = Vendor.joja
      then price |> withMultiplier (vars.Multipliers.ProfitMargin * if vars.JojaMembership then 1.0 else 1.25)
      else price |> withMultiplier vars.Multipliers.ProfitMargin |> max 1u

  let itemPrice vars forage item quality = Item.price vars.Skills vars.Multipliers forage item quality
  let itemPriceByQuality vars forage item = Item.priceByQuality vars.Skills vars.Multipliers forage item

  let productPrice data vars item quality product =
    Product.price data.Items.Find vars.Skills vars.Multipliers vars.ModData item quality product

  let productPriceByQuality data vars item product =
    Product.priceByQuality data.Items.Find vars.Skills vars.Multipliers vars.ModData item product

  let productNormalizedPrice data vars item quality product =
    Product.normalizedPrice data.Items.Find vars.Skills vars.Multipliers vars.ModData item quality product

  let productNormalizedPriceByQuality data vars item product =
    Product.normalizedPriceByQuality data.Items.Find vars.Skills vars.Multipliers vars.ModData item product

  let seedItemSellPrice data vars (seed: SeedId) = itemPrice vars false data.Items[seed * 1u<_>] Quality.Normal

  let xpPerItem data crop =
    Crop.xpPerItem data.Items.Find crop

  let xpItemsPerHarvest vars crop =
    Crop.xpItemsPerHarvest (giantCropProb vars) vars.Skills crop

  let xpPerHarvest data vars crop =
    Crop.xpPerHarvest data.Items.Find (giantCropProb vars) vars.Skills crop
