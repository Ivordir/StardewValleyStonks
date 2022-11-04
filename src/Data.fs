module StardewValleyStonks.Json

#nowarn "25"

#if FABLE_COMPILER
open Thoth.Json
#else
open Thoth.Json.Net
#endif

let [<Literal>] private SeedPriceVendorField = "Vendor"
let [<Literal>] private SeedPricePriceField = "Price"
let [<Literal>] private SeedPriceTypeField = "Type"
let [<Literal>] private FixedSeedPrice = "Fixed"
let [<Literal>] private ScalingSeedPrice = "Scaling"

let [<Literal>] private KeySelectionSelectField = "Select"
let [<Literal>] private KeySelectionEntriesField = "Entries"

module Encode =
  let index encoder (index: _ Block) = index |> Block.toSeq |> Seq.map encoder |> Encode.seq

  let seq' encoder (seq: _ seq) = seq |> Seq.map encoder |> Encode.seq

  let set encoder (sel: _ Set) = sel |> Seq.map encoder |> Encode.seq

  let table encodeKey encodeValue table =
    Table.toSeq table
    |> Seq.map (fun (k, v) -> encodeKey k, encodeValue v)
    #if !FABLE_COMPILER
    |> List.ofSeq
    #endif
    |> Encode.object

  let vendor (VendorName vendor) = Encode.string vendor

  let fertilizerName (FertName name) = Encode.string name
  let fertilizer = Encode.Auto.generateEncoder<Fertilizer> ()
  let quality = Encode.Auto.generateEncoder<Quality> ()
  let farming = Encode.Auto.generateEncoder<Farming> ()
  let foraging = Encode.Auto.generateEncoder<Foraging> ()

  let itemId (item: ItemId) = nat item |> Encode.uint32
  let processor (ProcessorName name) = Encode.string name

  let product = function
    | Jam -> Encode.string <| nameof Jam
    | Wine -> Encode.string <| nameof Wine
    | Pickles -> Encode.string <| nameof Pickles
    | Juice -> Encode.string <| nameof Juice
    | SeedsFromSeedMaker seed ->
      Encode.object [ nameof SeedsFromSeedMaker, itemId seed ]
    | Processed p ->
      Encode.object [
        nameof p.Item, itemId p.Item
        nameof p.Processor, processor p.Processor
        if p.Ratio <> None then
          nameof p.Ratio, Encode.tuple2 Encode.uint32 Encode.uint32 (Option.get p.Ratio)
      ]

  let category = Encode.Auto.generateEncoder<Category> ()

  let item = Encode.Auto.generateEncoder<Item> ()

  let season (s: Season) = Season.name s |> Encode.string

  // Fable can't parse flag enums, transform into a sequence instead.
  let seasons (seasons: Seasons) =
    Season.all
    |> Block.filter (flip Seasons.contains seasons)
    |> index season

  let date (Date (season, day)) = Encode.tuple2 Encode.string Encode.uint32 (Season.name season, day)

  let seedId (seed: SeedId) = nat seed |> Encode.uint32

  let seedPrice = function
    | FixedPrice (v, p) ->
      Encode.object [
        SeedPriceVendorField, vendor v
        SeedPricePriceField, Encode.uint32 p
        SeedPriceTypeField, Encode.string FixedSeedPrice
      ]
    | ScalingPrice (v, None) ->
      Encode.object [
        SeedPriceVendorField, vendor v
        SeedPriceTypeField, Encode.string ScalingSeedPrice
      ]
    | ScalingPrice (v, Some p) ->
      Encode.object [
        SeedPriceVendorField, vendor v
        SeedPricePriceField, Encode.uint32 p
        SeedPriceTypeField, Encode.string ScalingSeedPrice
      ]

  let private growthDataFields (data: GrowthData) = [
    nameof data.Seasons, seasons data.Seasons
    nameof data.Stages, index Encode.uint32 data.Stages
    if data.Paddy then nameof data.Paddy, Encode.bool true
    nameof data.Seed, seedId data.Seed
  ]

  let growthData = growthDataFields >> Encode.object

  let cropAmount (amount: CropAmount) =
    let single = CropAmount.singleAmount
    Encode.object [
      if amount.MinCropYield <> single.MinCropYield then
        nameof amount.MinCropYield, Encode.uint32 amount.MinCropYield
      if amount.MaxCropYield <> single.MaxCropYield then
        nameof amount.MaxCropYield, Encode.uint32 amount.MaxCropYield
      if amount.ExtraCropChance <> single.ExtraCropChance then
        nameof amount.ExtraCropChance, Encode.float amount.ExtraCropChance
      if amount.CanDouble <> single.CanDouble then
        nameof amount.CanDouble, Encode.bool amount.CanDouble
      if amount.Giant <> single.Giant then
        nameof amount.Giant, Encode.bool amount.Giant
      if amount.FarmLevelsPerYieldIncrease <> single.FarmLevelsPerYieldIncrease then
        nameof amount.FarmLevelsPerYieldIncrease, Encode.uint32 amount.FarmLevelsPerYieldIncrease
      if amount.FarmingQualities <> single.FarmingQualities then
        nameof amount.FarmingQualities, Encode.bool amount.FarmingQualities
    ]

  let farmCrop (crop: FarmCrop) =
    Encode.object [
      yield! growthDataFields crop.Growth
      nameof crop.Item, itemId crop.Item
      if crop.Amount <> CropAmount.singleAmount then
        nameof crop.Amount, cropAmount crop.Amount
      if crop.ExtraItem <> None then
        nameof crop.ExtraItem, Encode.tuple2 itemId Encode.float (Option.get crop.ExtraItem)
    ]

  let forageCrop (crop: ForageCrop) =
    Encode.object [
      yield! growthDataFields crop.Growth
      nameof crop.Items, index itemId crop.Items
      nameof crop.SeedRecipeUnlockLevel, Encode.uint32 crop.SeedRecipeUnlockLevel
    ]


type RawGameData = {
  FarmCrops: FarmCrop array
  ForageCrops: ForageCrop array
  Fertilizers: Fertilizer array
  Items: Table<ItemId, Item>
  Products: Table<ItemId, Product array>
  ProcessorUnlockLevel: Table<Processor, nat>
  FertilizerPrices: Table<FertilizerName, Table<Vendor, nat>>
  SeedPrices: Table<SeedId, SeedPrice array>
  GenerateSeedPrices: (Vendor * SeedId array) list
}

type KeySelection<'key when 'key: comparison> =
  | SelectAllKeys of bool
  | SelectKeys of bool * 'key Set

module Decode =
  module Helpers =
    let validate desc predicate decoder : _ Decoder = fun path value ->
      match decoder path value with
      | Ok y ->
        if predicate y
        then Ok y
        else Error (path, BadPrimitive (desc, value))
      | Error e -> Error e

    let validate' validator decoder : _ Decoder = fun path value ->
      match decoder path value with
      | Ok y ->
        match validator y with
        | Ok y -> Ok y
        | Error e -> Error (path, BadPrimitive (e, value))
      | Error e -> Error e

    let greater desc lowerBound =
      validate $"%s{desc} greater than {lowerBound}" (fun x -> x > lowerBound)
    let greaterOrEqual desc lowerBound =
      validate $"%s{desc} greater than or equal to {lowerBound}" (fun x -> x >= lowerBound)
    let less desc upperBound =
      validate $"%s{desc} less than {upperBound}" (fun x -> x < upperBound)
    let lessOrEqual desc upperBound =
      validate $"%s{desc} less than or equal to {upperBound}" (fun x -> x <= upperBound)
    let inRange desc min max =
      validate $"%s{desc} in the range [{min}, {max}]" (onClosedInterval min max)

    let natLessOrEqual upperBound = lessOrEqual "an integer" upperBound Decode.uint32
    let natGreaterOrEqual lowerBound = greaterOrEqual "an integer" lowerBound Decode.uint32
    let nonZeroNat = greater "an integer" 0u Decode.uint32
    let natInRange min max = inRange "a natural number" min max Decode.uint32

    let nonNegativeFloat = greaterOrEqual "a float" 0.0 Decode.float
    let nonZeroFloat = greater "a float" 0.0 Decode.float
    let floatInRange min max = inRange "a float" min max Decode.float

    let private tryParseInt<[<Measure>] 'u> (str: string): int<'u> option =
      match System.Int32.TryParse str with
      | true, value -> Some (LanguagePrimitives.Int32WithMeasure value)
      | _ -> None

    let parseItemId = tryParseInt<ItemNum>
    let parseSeedId = tryParseInt<SeedNum>

  open Helpers

  let private wrap (wrapper: 'a -> 'b) (decoder: 'a Decoder) = Fable.Core.JsInterop.(!!) decoder : 'b Decoder
  let private intMeasure<[<Measure>] 'u> = Fable.Core.JsInterop.(!!) Decode.uint32 : int<'u> Decoder

  let index (decoder: 'a Decoder) = Decode.array decoder |> wrap Block.wrap

  let set decoder = Decode.array decoder |> Decode.map Set.ofArray

  let private customKeyValuePairsWith mapping keyMapping decodeValue =
    Decode.keyValuePairs decodeValue |> Decode.map (Seq.map (fun (k, v) -> keyMapping k, v) >> mapping)
  let private wrapKeys mapping (keyWrap: string -> 'k) (decodeValue: 'v Decoder) =
    (Fable.Core.JsInterop.(!!) (Decode.keyValuePairs decodeValue) : ('k * 'v) list Decoder) |> Decode.map mapping
  let private parseKeysWith (mapping: _ seq -> _) parseKey decodeValue =
    Decode.keyValuePairs decodeValue |> Decode.andThen (fun kvps ->
      let acc = ResizeArray ()
      let rec parseKeys = function
        | [] -> true
        | (key, value) :: rest ->
          match parseKey key with
          | Some key ->
            acc.Add (key, value)
            parseKeys rest
          | None -> false
      if parseKeys kvps
      then mapping acc |> Decode.succeed
      else fun path value -> Error (path, BadPrimitive ("a valid key", value)))
  let private parseKeys parseKey decodeValue = parseKeysWith id parseKey decodeValue

  let table keyWrap decodeValue = wrapKeys Table.ofSeq keyWrap decodeValue
  let tableParse parseKey decodeValue = parseKeysWith Table.ofSeq parseKey decodeValue
  let tableOfValues getKey decodeValue = Decode.array decodeValue |> Decode.map (Table.ofValues getKey)

  let mapTree keyWrap decodeValue = wrapKeys Map.ofSeq keyWrap decodeValue
  let mapTreeParse parseKey decodeValue = parseKeysWith Map.ofSeq parseKey decodeValue
  let mapOfValues getKey decodeValue = Decode.array decodeValue |> Decode.map (Map.ofValues getKey)

  let fertilizerName = Decode.string |> wrap FertName

  let fertilizer =
    let u = Unchecked.defaultof<Fertilizer>
    Decode.object <| fun get -> {
      Name = get.Required.Field (nameof u.Name) fertilizerName
      Quality = get.Optional.Field (nameof u.Quality) Decode.uint32 |> Option.defaultValue 0u
      Speed = get.Optional.Field (nameof u.Speed) nonNegativeFloat |> Option.defaultValue 0.0
    }

  let quality = Decode.Enum.int |> validate "a defined quality" (fun q -> System.Enum.IsDefined (typeof<Quality>, q))

  let skills = Decode.Auto.generateDecoder ()

  let vendor = Decode.Auto.generateDecoder ()
  let processor = Decode.Auto.generateDecoder ()

  let itemId = intMeasure<ItemNum>
  let seedId = intMeasure<SeedNum>

  let product =
    let (Processed processed) = Processed Unchecked.defaultof<_>
    Decode.oneOf [
      Decode.string |> Decode.andThen (function
        | nameof Jam -> Decode.succeed Jam
        | nameof Wine -> Decode.succeed Wine
        | nameof Pickles -> Decode.succeed Pickles
        | nameof Juice -> Decode.succeed Juice
        | _ -> Decode.fail $"Was not '{nameof Jam}', '{nameof Wine}', '{nameof Pickles}', or '{nameof Juice}'.")

      Decode.object <| fun get ->
        SeedsFromSeedMaker (get.Required.Field (nameof SeedsFromSeedMaker) itemId)

      Decode.object <| fun get -> Processed {|
        Item = get.Required.Field (nameof processed.Item) itemId
        Processor = get.Required.Field (nameof processed.Processor) processor
        Ratio = get.Optional.Field (nameof processed.Ratio) (Decode.tuple2 nonZeroNat nonZeroNat)
      |}
    ]

  let item = Decode.Auto.generateDecoder ()

  let season =
    Decode.string |> Decode.andThen (fun str ->
      match System.Enum.TryParse str with
      | true, season ->
        match season with
        | Seasons.Spring
        | Seasons.Summer
        | Seasons.Fall
        | Seasons.Winter -> Season.ofSeasons season |> Decode.succeed
        | _ -> Decode.fail $"Undefined season: {season}"
      | _ -> Decode.fail $"Failed to parse season: {str}")

  // Fable can't parse flag enums, parse Season Block instead.
  let seasons = Decode.array season |> Decode.map Seasons.ofSeq

  let date = Decode.tuple2 season (natInRange Date.firstDay Date.lastDay) |> wrap Date

  let location = Decode.Auto.generateDecoder<Location> ()

  let seedPrice =
    Decode.map3 tuple3
      (Decode.field SeedPriceVendorField vendor)
      (Decode.optional SeedPricePriceField Decode.uint32)
      (Decode.field SeedPriceTypeField Decode.string)
    |> Decode.andThen (fun (vendor, price, kind) -> fun path value ->
      match kind, price with
      | FixedSeedPrice, Some price -> Ok (SeedPrice.createFixed vendor price)
      | FixedSeedPrice, None -> Error (path, BadField ($"an object with a field named '{SeedPricePriceField}'", value))
      | ScalingSeedPrice, Some price -> Ok (SeedPrice.createScalingFrom vendor price)
      | ScalingSeedPrice, None -> Ok (SeedPrice.createScaling vendor)
      | _ -> Error (path, BadPrimitive ($"a valid price type ('{FixedSeedPrice}' or '{ScalingSeedPrice}')", value)))

  let private growthDataFields =
    let u = Unchecked.defaultof<GrowthData>
    fun (get: Decode.IGetters) ->
      let inline field x = get.Required.Field x
      let stages = field (nameof u.Stages) (index nonZeroNat |> validate "growth stage with at least one stage" (Block.isEmpty >> not))
      let total =
        match Some stages with
        | None -> 0u
        | Some ind -> Block.sum ind

      {
        Stages = stages
        TotalTime = total
        RegrowTime = get.Optional.Field (nameof u.RegrowTime) Decode.uint32
        Seasons = field (nameof u.Seasons) seasons
        Paddy = get.Optional.Field (nameof u.Paddy) Decode.bool |> Option.defaultValue false
        Seed = field (nameof u.Seed) seedId
      }

  let growthData: GrowthData Decoder = Decode.object growthDataFields

  let cropAmount =
    let u = Unchecked.defaultof<CropAmount>
    Decode.object <| fun get ->
      let inline field name decoder defVal = get.Optional.Field name decoder |> Option.defaultValue defVal
      let single = CropAmount.singleAmount
      let minYield = field (nameof u.MinCropYield) Decode.uint32 single.MinCropYield
      let maxYield = field (nameof u.MaxCropYield) (nonZeroNat |> validate "a max crop yield greater than the min crop yield" (fun x -> x >= minYield)) single.MaxCropYield
      {
        MinCropYield = minYield
        MaxCropYield = maxYield
        FarmLevelsPerYieldIncrease = field (nameof u.FarmLevelsPerYieldIncrease) Decode.uint32 single.FarmLevelsPerYieldIncrease
        ExtraCropChance = field (nameof u.ExtraCropChance) (floatInRange CropAmount.minExtraCropChance CropAmount.maxExtraCropChance) single.ExtraCropChance
        CanDouble = field (nameof u.CanDouble) Decode.bool single.CanDouble
        Giant = field (nameof u.Giant) Decode.bool single.Giant
        FarmingQualities = field (nameof u.FarmingQualities) Decode.bool single.FarmingQualities
      }

  let farmCrop =
    let u = Unchecked.defaultof<FarmCrop>
    Decode.object <| fun get -> {
      Growth = growthDataFields get
      Item = get.Required.Field (nameof u.Item) itemId
      Amount = get.Optional.Field (nameof u.Amount) cropAmount |> Option.defaultValue CropAmount.singleAmount
      ExtraItem = get.Optional.Field (nameof u.ExtraItem) (Decode.tuple2 itemId nonZeroFloat)
    }

  let forageCrop =
    let u = Unchecked.defaultof<ForageCrop>
    Decode.object <| fun get -> {
      Growth = growthDataFields get
      Items = get.Required.Field (nameof u.Items) (index itemId |> validate "at least one item" (Block.isEmpty >> not))
      SeedRecipeUnlockLevel = get.Required.Field (nameof u.SeedRecipeUnlockLevel) Decode.uint32
    }
    |> validate "a forage crop with no regrow time (not yet supported)" (ForageCrop.growth >> Growth.regrowTime >> Option.isNone)

  let cropAmountSettings =
    let u = Unchecked.defaultof<CropAmountSettings>
    Decode.object <| fun get ->
      let inline field name = get.Required.Field name
      {
        SpecialCharm = field (nameof u.SpecialCharm) Decode.bool
        GiantChecksPerTile = field (nameof u.GiantChecksPerTile) (floatInRange CropAmount.minGiantCropChecks CropAmount.maxGiantCropChecks)
        ShavingToolLevel = field (nameof u.ShavingToolLevel) (Decode.option <| natLessOrEqual CropAmount.maxShavingToolLevel)
        LuckBuff = field (nameof u.LuckBuff) Decode.uint32
      }

  let multipliers =
    let u = Unchecked.defaultof<Multipliers>
    Decode.object <| fun get ->
      let inline field name = get.Required.Field name
      {
        ProfitMargin = field (nameof u.ProfitMargin) (floatInRange 0.25 1.0 |> validate "a multiple of 0.25" (fun x -> x % 0.25 = 0.0))
        BearsKnowledge = field (nameof u.BearsKnowledge) Decode.bool
        ForagedFruitTillerOverrides = field (nameof u.ForagedFruitTillerOverrides) (set itemId)
      }

  let modData =
    let u = Unchecked.defaultof<ModData>
    Decode.object <| fun get ->
      let inline field name = get.Required.Field name
      {
        QualityProducts = field (nameof u.QualityProducts) Decode.bool
        QualityProcessors = field (nameof u.QualityProcessors) (set processor)
      }

  let seedMode = Decode.Auto.generateDecoder ()

  let rawGameData =
    let u = Unchecked.defaultof<RawGameData>
    Decode.object (fun get ->
      let inline field name = get.Required.Field name
      {
        FarmCrops = field (nameof u.FarmCrops) (Decode.array farmCrop)
        ForageCrops = field (nameof u.ForageCrops) (Decode.array forageCrop)
        Fertilizers = field (nameof u.Fertilizers) (Decode.array fertilizer)
        Items = field (nameof u.Items) (tableOfValues Item.id item)
        Products = field (nameof u.Products) (tableParse parseItemId (Decode.array product))
        ProcessorUnlockLevel = field (nameof u.ProcessorUnlockLevel) (table ProcessorName Decode.uint32)
        FertilizerPrices = field (nameof u.FertilizerPrices) (table FertName (table VendorName Decode.uint32))
        SeedPrices = field (nameof u.SeedPrices) (tableParse parseSeedId (Decode.array seedPrice))
        GenerateSeedPrices = field (nameof u.GenerateSeedPrices) (wrapKeys id VendorName (Decode.array seedId))
      })
    |> validate' (fun data ->
      let crops =
        data.FarmCrops
        |> Array.map FarmCrop
        |> Array.append (data.ForageCrops |> Array.map ForageCrop)

      let missing =
        crops
        |> Seq.collect (Crop.items >> Block.toSeq)
        |> Seq.append (crops |> Seq.map Crop.seedItem)
        |> Seq.append (data.Products.Values |> Seq.concat |> Seq.choose Product.item)
        |> Seq.filter (data.Items.ContainsKey >> not)
        |> Seq.map string
        |> Array.ofSeq
      if missing.Length = 0
      then Ok data
      else Error <| "The following items ids were referenced, but no items with the ids were provided: " + String.concat ", " missing)

  let private sortKeysByHighestCount table =
    table
    |> Table.values
    |> Seq.collect Table.keys
    |> Seq.countBy id
    |> Seq.sortByDescending snd
    |> Seq.map fst
    |> Block.ofSeq

  let gameDataFromRaw rawData =
    let crops =
      rawData.FarmCrops
      |> Array.map FarmCrop
      |> Array.append (rawData.ForageCrops |> Array.map ForageCrop)

    let seedPrices =
      let generated =
        rawData.GenerateSeedPrices
        |> Seq.collect (fun (vendor, seeds) ->
          seeds |> Array.map (flip tuple2 <| SeedPrice.createScaling vendor))
        |> Seq.groupBy fst
        |> Seq.map (fun (seed, prices) -> seed, prices |> Seq.map snd)
        |> Table.ofSeq
      crops
      |> Seq.map Crop.seed
      |> Table.ofKeys (fun seed ->
        rawData.SeedPrices.TryFind seed
        |> Option.defaultValue Array.empty
        |> Seq.append (generated.TryFind seed |> Option.defaultValue Seq.empty)
        |> Table.ofValues SeedPrice.vendor)

    let products =
      let seedMakerItems =
        crops
        |> Seq.choose (fun crop ->
          if Crop.canGetOwnSeedsFromSeedMaker rawData.Items.Find crop
          then Some (Crop.mainItem crop, SeedsFromSeedMaker <| Crop.seedItem crop)
          else None)
        |> Table.ofSeq
      rawData.Items.Keys |> Table.ofKeys (fun item ->
        let generate =
          match rawData.Items[item].Category with
          | Vegetable -> [| Pickles; Juice |]
          | Fruit -> [| Jam; Wine |]
          | _ -> [| |]
        rawData.Products.TryFind item
        |> Option.defaultValue Array.empty
        |> Array.append (seedMakerItems.TryFind item |> Option.toArray)
        |> Array.append generate
        |> Table.ofValues Product.processor)

    {
      Fertilizers = rawData.Fertilizers |> Table.ofValues Fertilizer.name
      FertilizerPrices =
        rawData.Fertilizers
        |> Seq.map Fertilizer.name
        |> Table.ofKeys (rawData.FertilizerPrices.TryFind >> Option.defaultWith Table.empty)
      FertilizerVendors = sortKeysByHighestCount rawData.FertilizerPrices

      Crops = crops |> Table.ofValues Crop.seed
      FarmCrops = rawData.FarmCrops |> Table.ofValues FarmCrop.seed
      ForageCrops = rawData.ForageCrops |> Table.ofValues ForageCrop.seed
      SeedPrices = seedPrices
      SeedVendors = sortKeysByHighestCount seedPrices

      Items = rawData.Items
      Products = products
      Processors =
        products.Values
        |> Seq.collect Table.keys
        |> Seq.distinct
        |> Seq.sortWith (sortWithLastBy None rawData.ProcessorUnlockLevel.TryFind)
        |> Block.ofSeq
      ProcessorUnlockLevel = rawData.ProcessorUnlockLevel
    }

  let select key =
    Decode.oneOf [
      Decode.bool |> Decode.map SelectAllKeys
      Decode.array key |> Decode.map (fun keys -> SelectKeys (true, Set.ofArray keys))
      Decode.map2 (fun select entries ->
        match entries with
        | Some entries -> SelectKeys (select, Set.ofArray entries)
        | None -> SelectAllKeys select)
        (Decode.field KeySelectionSelectField Decode.bool)
        (Decode.optional KeySelectionEntriesField (Decode.array key))
    ]

  let private selectWith allKeys = function
    | SelectAllKeys true -> Set.ofSeq allKeys
    | SelectAllKeys false -> Set.empty
    | SelectKeys (true, keys) -> keys
    | SelectKeys (false, keys) -> Set.ofSeq allKeys - keys

  let private selectFrom allKeys decodeKey = select decodeKey |> Decode.map (selectWith allKeys)

  let private columnSelectSeq selectMissing columnKey selectionKey data =
    table columnKey (select selectionKey) |> Decode.map (fun columnSelections ->
      data |> Seq.map (fun (key, columns) ->
        key,
        columns
        |> Table.keys
        |> Seq.filter (fun column ->
          match columnSelections.TryFind column with
          | None -> selectMissing
          | Some select ->
            match select with
            | SelectAllKeys b -> b
            | SelectKeys (true, keys) -> keys.Contains key
            | SelectKeys (false, keys) -> not <| keys.Contains key)
        |> Set.ofSeq)
      |> Map.ofSeq)

  let inline private columnSelect selectMissing columnKey selectionKey table =
    table |> Table.toSeq |> columnSelectSeq selectMissing columnKey selectionKey

  let selection key values =
    let u = Unchecked.defaultof<Selection<_,_>>
    Decode.map2
      (fun values selected ->
        { Values = values
          Selected = selectWith (Map.keys values) selected })
      (Decode.field (nameof u.Values) values)
      (Decode.field (nameof u.Selected) (select key))

  let model =
    let u = Unchecked.defaultof<Model>
    fun (data: GameData) ->
      let seedItemPairs =
        data.Crops
        |> Table.toSeq
        |> Seq.collect (fun (seed, crop) ->
          Crop.items crop |> Block.map' (fun item -> (seed, item)))
        |> Array.ofSeq

      let forageCrops =
        data.Crops
        |> Table.toSeq
        |> Seq.choose (fun (seed, crop) ->
          if Crop.isForage crop
          then Some seed
          else None)
        |> Array.ofSeq

      Decode.object (fun get ->
        let inline field x = get.Required.Field x
        {
          Data = data

          SelectedCrops = field (nameof u.SelectedCrops) (selectFrom data.Crops.Keys seedId)
          SelectedSeedPrices = field (nameof u.SelectedSeedPrices) (columnSelect true VendorName seedId data.SeedPrices)

          SelectedFertilizers = field (nameof u.SelectedFertilizers) (selectFrom data.Fertilizers.Keys fertilizerName)
          AllowNoFertilizer = field (nameof u.AllowNoFertilizer) Decode.bool
          SelectedFertilizerPrices = field (nameof u.SelectedFertilizerPrices) (columnSelect true VendorName fertilizerName data.FertilizerPrices)

          SellRawItems = field (nameof u.SellRawItems) (selectFrom seedItemPairs (Decode.tuple2 seedId itemId))
          SelectedProducts =
            field
              (nameof u.SelectedProducts)
              (seedItemPairs |> Seq.map (fun (seed, item) -> (seed, item), data.Products[item])
                |> columnSelectSeq true ProcessorName (Decode.tuple2 seedId itemId))
          SellForageSeeds = field (nameof u.SellForageSeeds) (selectFrom forageCrops seedId)

          UseRawSeeds =
            field
              (nameof u.UseRawSeeds)
              (selectFrom
                (seedItemPairs |> Seq.choose (fun (seed, item) ->
                  if int seed = int item
                  then Some seed
                  else None))
                seedId)
          UseSeedMaker =
            field
              (nameof u.UseSeedMaker)
              (selectFrom
                (data.Crops.Values |> Seq.choose (fun crop ->
                  if data |> GameData.canGetOwnSeedsFromSeedMaker crop
                  then Some (Crop.seed crop)
                  else None))
                seedId)
          UseForageSeeds = field (nameof u.UseForageSeeds) (selectFrom forageCrops seedId)

          CustomFertilizerPrices =
            get.Optional.Field
              (nameof u.CustomFertilizerPrices)
              (selection fertilizerName (mapTree FertName Decode.uint32))
            |> Option.defaultValue Selection.empty
          CustomSeedPrices =
            get.Optional.Field
              (nameof u.CustomSeedPrices)
              (selection seedId (mapTreeParse parseSeedId Decode.uint32))
            |> Option.defaultValue Selection.empty
          CustomSellPrices =
            get.Optional.Field
              (nameof u.CustomSellPrices)
              (selection
                (Decode.tuple2 seedId itemId)
                (parseKeys parseSeedId (parseKeys parseItemId (Decode.tuple2 Decode.uint32 Decode.bool))
                  |> Decode.map (Seq.collect (fun (seed, items) -> items |> Seq.map (fun (item, price) -> (seed, item), price)) >> Map.ofSeq)))
            |> Option.defaultValue Selection.empty

          Skills = field (nameof u.Skills) skills
          Multipliers = field (nameof u.Multipliers) multipliers
          CropAmount = field (nameof u.CropAmount) cropAmountSettings
          ModData = field (nameof u.ModData) modData
          JojaMembership = field (nameof u.JojaMembership) Decode.bool
          Irrigated = field (nameof u.Irrigated) Decode.bool

          StartDate = field (nameof u.StartDate) date
          EndDate = field (nameof u.EndDate) date
          Location = field (nameof u.Location) location

          SeedMode = field (nameof u.SeedMode) seedMode
          PayForFertilizer = field (nameof u.PayForFertilizer) Decode.bool
          ReplaceLostFertilizer = field (nameof u.ReplaceLostFertilizer) Decode.bool
        })
      // check all selected items exist?

  let appMode = Decode.Auto.generateDecoder ()

  let tablesort = Decode.Auto.generateDecoder () |> Decode.map SortByColumns

  let settingsTab = Decode.Auto.generateDecoder ()
  let openDetails = Decode.Auto.generateDecoder ()

  let cropFilters =
    let u = Unchecked.defaultof<CropFilters>
    Decode.object <| fun get -> {
      InSeason = get.Required.Field (nameof u.InSeason) Decode.bool
      Seasons = get.Required.Field (nameof u.Seasons) seasons
      Regrows = get.Optional.Field (nameof u.Regrows) Decode.bool
      Giant = get.Optional.Field  (nameof u.Giant) Decode.bool
      Forage = get.Optional.Field  (nameof u.Forage) Decode.bool
    }

  let rankMetric = Decode.Auto.generateDecoder ()
  let timeNormalization = Decode.Auto.generateDecoder ()
  let rankItem = Decode.Auto.generateDecoder ()

  let ranker =
    let u = Unchecked.defaultof<Ranker>
    Decode.object <| fun get ->
      let inline field name = get.Required.Field name
      {
        RankItem = field (nameof u.RankItem) rankItem
        RankMetric = field (nameof u.RankMetric) rankMetric
        TimeNormalization = field (nameof u.TimeNormalization) timeNormalization
        BrushSpan = field (nameof u.BrushSpan) (Decode.tuple2 Decode.uint32 Decode.uint32 |> validate "the end of the brush to be on or after the start of the brush" (uncurry (<=)))
        SelectedCropAndFertilizer = get.Optional.Field (nameof u.SelectedCropAndFertilizer) (Decode.tuple2 (Decode.option seedId) (fertilizerName |> Decode.option |> Decode.option))
      }

  let app =
    let u = Unchecked.defaultof<App>
    fun model ->
      Decode.object <| fun get ->
        let inline field x = get.Required.Field x
        {
          Model = model
          SavedModels = []

          AppMode = field (nameof u.AppMode) appMode
          SettingsTab = field (nameof u.SettingsTab) settingsTab
          OpenDetails = field (nameof u.OpenDetails) (set openDetails)

          FertilizerSort = field (nameof u.FertilizerSort) tablesort
          FertilizerPriceSort = field (nameof u.FertilizerPriceSort) tablesort
          CropSort = field (nameof u.CropSort) tablesort
          ProductSort = field (nameof u.ProductSort) tablesort
          SeedSort = field (nameof u.SeedSort) tablesort
          ProductQuality = field (nameof u.ProductQuality) quality
          ShowNormalizedProductPrices = field (nameof u.ShowNormalizedProductPrices) Decode.bool

          CropFilters = field (nameof u.CropFilters) cropFilters
          Ranker = field (nameof u.Ranker) ranker
        }


open Fable.Core.JsInterop
let inline private load url decoder = importAll url |> Decode.fromValue "$" decoder

let loadGameData () =
  load "../public/Data.json5" Decode.rawGameData
  |> Result.map Decode.gameDataFromRaw

let loadDefaultApp () =
  loadGameData ()
  |> Result.bind (Decode.model >> load "../public/Model.json5")
  |> Result.bind (Decode.app >> load "../public/App.json5")
