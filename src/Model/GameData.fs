namespace StardewValleyStonks

type ExtractedData = {
  Items: Item array
  FarmCrops: FarmCrop array
  ForageCrops: ForageCrop array
}

type SupplementalData = {
  Fertilizers: Fertilizer array
  Products: Table<ItemId, Product array>
  ProcessorUnlockLevel: Table<Processor, nat>
  FertilizerPrices: Table<FertilizerName, Table<Vendor, nat>>
  SeedPrices: Table<SeedId, SeedPrice array>
  GenerateSeedPrices: (Vendor * SeedId array) list
}

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

  let private sortKeysByHighestCount table =
    table
    |> Table.values
    |> Seq.collect Table.keys
    |> Seq.countBy id
    |> Seq.sortByDescending snd
    |> Seq.map fst
    |> Array.ofSeq

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
      FertilizerVendors = sortKeysByHighestCount supplemental.FertilizerPrices

      Crops = crops |> Table.ofValues Crop.seed
      FarmCrops = extracted.FarmCrops |> Table.ofValues FarmCrop.seed
      ForageCrops = extracted.ForageCrops |> Table.ofValues ForageCrop.seed
      SeedPrices = seedPrices
      SeedVendors = sortKeysByHighestCount seedPrices

      Items = items
      Products = products
      Processors =
        products.Values
        |> Seq.collect Table.keys
        |> Seq.distinct
        |> Seq.sortWith (Option.noneMaxCompareBy supplemental.ProcessorUnlockLevel.TryFind)
        |> Array.ofSeq
      ProcessorUnlockLevel = supplemental.ProcessorUnlockLevel
    }
