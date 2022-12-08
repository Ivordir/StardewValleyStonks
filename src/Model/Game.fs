namespace StardewValleyStonks

// Bulk data extrarced from the game's xnb files
type ExtractedData = {
  Items: Item array
  FarmCrops: FarmCrop array
  ForageCrops: ForageCrop array
}

// Data found / inputted manually by digging through game code.
type SupplementalData = {
  Fertilizers: Fertilizer array
  Products: Table<ItemId, Product array>
  ProcessorUnlockLevel: Table<Processor, nat>
  FertilizerPrices: Table<FertilizerName, Table<Vendor, nat>>
  SeedPrices: Table<SeedId, SeedPrice array>
  GenerateSeedPrices: (Vendor * SeedId array) list
}

// The model type for all of Stardew Valley's relevant game data. (This does not include save game data.)
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
// Assume for now that SeedMaker is the only processor which converts items into seeds.

module GameData =
  let seedItemPairs data =
    data.Crops
    |> Table.toSeq
    |> Seq.collect (fun (seed, crop) ->
      Crop.items crop |> Array.map (tuple2 seed))
    |> Array.ofSeq

  let cropCanGetOwnSeedsFromSeedMaker crop data =
    match data.Products[Crop.mainItem crop].TryFind Processor.seedMaker with
    | Some (SeedsFromSeedMaker item) when item = Crop.seedItem crop -> true
    | _ -> false

  let missingItemIds data =
    let crops =
      data.FarmCrops.Values
      |> Seq.map FarmCrop
      |> Seq.append (data.ForageCrops.Values |> Seq.map ForageCrop)
      |> Array.ofSeq

    crops
    |> Seq.collect Crop.items
    |> Seq.append (crops |> Seq.map Crop.seedItem)
    |> Seq.append (data.Products.Values |> Seq.collect Table.values |> Seq.choose Product.item)
    |> Seq.filter (data.Items.ContainsKey >> not)

  let fromExtractedAndSupplementalData (extracted: ExtractedData) (supplemental: SupplementalData) =
    let items = extracted.Items |> Table.ofValues Item.id

    let crops =
      extracted.FarmCrops
      |> Array.map FarmCrop
      |> Array.append (extracted.ForageCrops |> Array.map ForageCrop)

    let seedPrices =
      supplemental.GenerateSeedPrices
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
        |> Seq.append (seedPrices.TryFind seed |> Option.defaultValue Seq.empty)
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
          | _ -> [| |]

        supplemental.Products.TryFind item
        |> Option.defaultValue Array.empty
        |> Array.append (seedMakerItems.TryFind item |> Option.toArray)
        |> Array.append generate
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
  CropAmount: CropAmountSettings
  ModData: ModData
  JojaMembership: bool
  Irrigated: bool
  StartDate: Date
  EndDate: Date
  Location: Location
}

module GameVariables =
  let common = {
    Skills = Skills.zero
    Multipliers = Multipliers.common
    CropAmount = CropAmountSettings.common
    ModData = ModData.common
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

  let farmCropFertilizerLossProb vars (crop: FarmCrop) =
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

  let farmCropHarvestAmounts vars fertilizer (crop: FarmCrop) =
    let qualities = Skills.farmCropQualitiesWith fertilizer vars.Skills
    if crop.Giant && giantCropsPossible vars.Location
    then CropAmount.farmingGiantAmounts vars.Skills vars.CropAmount crop.Amount qualities
    else CropAmount.farmingAmounts vars.Skills vars.CropAmount crop.Amount qualities

  let farmCropItemAmounts vars fertilizer (crop: FarmCrop) =
    let amounts = farmCropHarvestAmounts vars fertilizer crop
    match crop.ExtraItem with
    | Some (_, amount) -> [| amounts; Qualities.zero |> Qualities.updateQuality Quality.Normal amount |]
    | None -> [| amounts |]

  let forageCropItemAmounts vars (crop: ForageCrop) =
    Skills.forageCropHarvestAmounts vars.Skills |> Qualities.map (fun a -> a / float crop.Items.Length)

  let cropItemAmounts vars fertilizer = function
    | FarmCrop c -> farmCropItemAmounts vars fertilizer c
    | ForageCrop c -> forageCropItemAmounts vars c |> Array.create c.Items.Length

  let processorUnlocked data skills processor =
    skills.IgnoreSkillLevelRequirements
    || data.ProcessorUnlockLevel.TryFind processor |> Option.forall (fun l -> l <= skills.Farming.Level)

  let productUnlocked data vars = Product.processor >> processorUnlocked data vars

  let seedPrice data vars seed = function
    | FixedPrice (_, price) -> price
    | ScalingPrice (vendor, price) ->
      let price =
        match price with
        | Some price -> price
        | None -> 2u * (data.Items[seed * 1u<ItemNum/SeedNum>] |> Item.sellPrice)
      if vendor = Vendor.joja
      then price |> withMultiplier (vars.Multipliers.ProfitMargin * if vars.JojaMembership then 1.0 else 1.25)
      else price |> withMultiplier vars.Multipliers.ProfitMargin |> max 1u

  let itemPrice vars item quality = Item.price vars.Skills vars.Multipliers item quality
  let itemPrices vars item = Item.prices vars.Skills vars.Multipliers item
  let itemForagePrice vars item quality = Item.Forage.price vars.Skills vars.Multipliers item quality
  let itemForagePrices vars item = Item.Forage.prices vars.Skills vars.Multipliers item

  let productPrice data vars item quality product =
    Product.price data.Items.Find vars.Skills vars.Multipliers vars.ModData item quality product

  let productPrices data vars item product =
    Product.prices data.Items.Find vars.Skills vars.Multipliers vars.ModData item product

  let productProfit data vars item quality product =
    Product.profit data.Items.Find vars.Skills vars.Multipliers vars.ModData item quality product

  let productProfits data vars item product =
    Product.profits data.Items.Find vars.Skills vars.Multipliers vars.ModData item product

  let seedItemSellPrice data vars (seed: SeedId) = itemPrice vars data.Items[seed * 1u<ItemNum/SeedNum>] Quality.Normal

  let xpPerHarvest data vars crop =
    Crop.xpPerHarvest data.Items.Find (giantCropProb vars) vars.Skills crop