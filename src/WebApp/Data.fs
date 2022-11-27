module StardewValleyStonks.WebApp.Data

open StardewValleyStonks
open StardewValleyStonks.Json
open StardewValleyStonks.WebApp
open Fable.Core.JsInterop
#if FABLE_COMPILER
open Thoth.Json
#else
open Thoth.Json.Net
#endif

let [<Literal>] dataVersion = 1u

let private extractedData: JsonValue = importAll "../../public/data/Extracted.json"
let private rawData: JsonValue = importAll "../../public/data/Data.json5"
let private appData: JsonValue = importAll "../../public/data/App.json5"

let private load json decoder =
  json
  |> Decode.fromValue "$" decoder
  |> Result.get


type RawGameData = {
  Fertilizers: Fertilizer array
  Products: Table<ItemId, Product array>
  ProcessorUnlockLevel: Table<Processor, nat>
  FertilizerPrices: Table<FertilizerName, Table<Vendor, nat>>
  SeedPrices: Table<SeedId, SeedPrice array>
  GenerateSeedPrices: (Vendor * SeedId array) list
}

type SelectKeys<'key when 'key: comparison> =
  | SelectAllKeys of bool
  | SelectKeys of bool * 'key Set

module SelectKeys =
  let toSet allKeys = function
    | SelectAllKeys true -> Set.ofSeq allKeys
    | SelectAllKeys false -> Set.empty
    | SelectKeys (true, keys) -> keys
    | SelectKeys (false, keys) -> Set.ofSeq allKeys - keys

  let columnWise data columnSelections =
    let d = data
    d
    |> Table.toSeq
    |> Seq.map (fun (key, row) ->
      key,
      row
      |> Table.keys
      |> Seq.filter (fun column ->
        match columnSelections |> Table.tryFind column with
        | None -> true
        | Some select ->
          match select with
          | SelectAllKeys b -> b
          | SelectKeys (select, keys) -> select = keys.Contains key)
      |> Set.ofSeq)
    |> Map.ofSeq


type KeySelection<'key, 'value when 'key: comparison> = {
  Values: Map<'key, 'value>
  Selected: 'key SelectKeys
}

module KeySelection =
  let toSelection keySelection : Selection<_,_> = {
    Values = keySelection.Values
    Selected = keySelection.Selected |> SelectKeys.toSet (Map.keys keySelection.Values)
  }


type ShorthandSettings = {
  SelectedCrops: SeedId SelectKeys option
  SelectedSeedPrices: Table<Vendor, SeedId SelectKeys> option

  AllowNoFertilizer: bool option
  SelectedFertilizers: FertilizerName SelectKeys option
  SelectedFertilizerPrices: Table<Vendor, FertilizerName SelectKeys> option

  SellRawItems: (SeedId * ItemId) SelectKeys option
  SelectedProducts: Table<Processor, (SeedId * ItemId) SelectKeys> option
  SellForageSeeds: SeedId SelectKeys option

  UseRawSeeds: SeedId SelectKeys option
  UseSeedMaker: SeedId SelectKeys option
  UseForageSeeds: SeedId SelectKeys option

  CustomSeedPrices: KeySelection<SeedId, nat> option
  CustomFertilizerPrices: KeySelection<FertilizerName, nat> option
  CustomSellPrices: KeySelection<SeedId * ItemId, nat * bool> option

  Skills: Skills option
  Multipliers: Multipliers option
  CropAmount: CropAmountSettings option
  ModData: ModData option
  JojaMembership: bool option
  Irrigated: bool option

  StartDate: Date option
  EndDate: Date option
  Location: Location option

  SeedStrategy: SeedStrategy option
  PayForFertilizer: bool option
  ReplaceLostFertilizer: bool option
}


let private decodeRawGameData =
  let u = Unchecked.defaultof<RawGameData>
  Decode.object (fun get ->
    let field name decode = get.Required.Field name decode
    {
      Fertilizers = field (nameof u.Fertilizers) (Decode.array Decode.fertilizer)
      Products = field (nameof u.Products) (Decode.tableParse Decode.parseItemId (Decode.array Decode.product))
      ProcessorUnlockLevel = field (nameof u.ProcessorUnlockLevel) (Decode.table ProcessorName Decode.uint32)
      FertilizerPrices = field (nameof u.FertilizerPrices) (Decode.table id (Decode.table VendorName Decode.uint32))
      SeedPrices = field (nameof u.SeedPrices) (Decode.tableParse Decode.parseSeedId (Decode.array Decode.seedPrice))
      GenerateSeedPrices = field (nameof u.GenerateSeedPrices) (Decode.wrapKeys id VendorName (Decode.array Decode.seedId))
    }
  )

let private sortKeysByHighestCount table =
  table
  |> Table.values
  |> Seq.collect Table.keys
  |> Seq.countBy id
  |> Seq.sortByDescending snd
  |> Seq.map fst
  |> Array.ofSeq

let gameDataFrom (extractedData: ExtractedData) rawData =
  let items = extractedData.Items |> Table.ofValues Item.id

  let crops =
    extractedData.FarmCrops
    |> Array.map FarmCrop
    |> Array.append (extractedData.ForageCrops |> Array.map ForageCrop)

  let seedPrices =
    rawData.GenerateSeedPrices
    |> Seq.collect (fun (vendor, seeds) ->
      seeds |> Array.map (fun seed -> seed, ScalingPrice (vendor, None)))
    |> Seq.groupBy fst
    |> Seq.map (fun (seed, prices) -> seed, prices |> Seq.map snd)
    |> Table.ofSeq

  let seedPrices =
    crops
    |> Seq.map Crop.seed
    |> Table.ofKeys (fun seed ->
      rawData.SeedPrices.TryFind seed
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
    FarmCrops = extractedData.FarmCrops |> Table.ofValues FarmCrop.seed
    ForageCrops = extractedData.ForageCrops |> Table.ofValues ForageCrop.seed
    SeedPrices = seedPrices
    SeedVendors = sortKeysByHighestCount seedPrices

    Items = items
    Products = products
    Processors =
      products.Values
      |> Seq.collect Table.keys
      |> Seq.distinct
      |> Seq.sortWith (sortWithLastBy None rawData.ProcessorUnlockLevel.TryFind)
      |> Array.ofSeq
    ProcessorUnlockLevel = rawData.ProcessorUnlockLevel
  }

let gameData =
  let extractedData = load extractedData Decode.extractedData
  let rawData = load rawData decodeRawGameData
  gameDataFrom extractedData rawData

#if DEBUG
do
  let missingItems = GameData.missingItemIds gameData |> Seq.map string |> Array.ofSeq
  if missingItems.Length > 0 then
    failwith ("The following items ids were referenced, but no items with the ids were provided: " + String.concat ", " missingItems)
#endif

// validate:
// fertilizers
// products
// crops









module private Shorthand =
  module Decode =
    let [<Literal>] private SelectField = "Select"
    let [<Literal>] private EntriesField = "Entries"

    let selectKeys key =
      Decode.oneOf [
        Decode.bool |> Decode.map SelectAllKeys
        Decode.array key |> Decode.map (fun keys -> SelectKeys (true, Set.ofArray keys))
        Decode.map2
          (fun select entries -> SelectKeys (select, Set.ofArray entries))
          (Decode.field SelectField Decode.bool)
          (Decode.field EntriesField (Decode.array key))
      ]

    let keySelection key values =
      let u = Unchecked.defaultof<KeySelection<_,_>>
      Decode.object (fun get -> {
        Values = get.Required.Field (nameof u.Values) values
        Selected = get.Required.Field (nameof u.Selected) (selectKeys key)
      } )

    let shorthandSettings =
      let u = Unchecked.defaultof<ShorthandSettings>
      Decode.object (fun get ->
        let field name decode = get.Optional.Field name decode
        {
          SelectedCrops = field (nameof u.SelectedCrops) (selectKeys Decode.seedId)
          SelectedSeedPrices = field (nameof u.SelectedSeedPrices) (Decode.table VendorName (selectKeys Decode.seedId))

          AllowNoFertilizer = field (nameof u.AllowNoFertilizer) Decode.bool
          SelectedFertilizers = field (nameof u.SelectedFertilizers) (selectKeys Decode.string)
          SelectedFertilizerPrices = field (nameof u.SelectedFertilizerPrices) (Decode.table VendorName (selectKeys Decode.string))

          SellRawItems = field (nameof u.SellRawItems) (selectKeys (Decode.tuple2 Decode.seedId Decode.itemId))
          SelectedProducts = field (nameof u.SelectedProducts) (Decode.table ProcessorName (selectKeys (Decode.tuple2 Decode.seedId Decode.itemId)))
          SellForageSeeds = field (nameof u.SellForageSeeds) (selectKeys Decode.seedId)

          UseRawSeeds = field (nameof u.UseRawSeeds) (selectKeys Decode.seedId)
          UseSeedMaker = field (nameof u.UseSeedMaker) (selectKeys Decode.seedId)
          UseForageSeeds = field (nameof u.UseForageSeeds) (selectKeys Decode.seedId)

          CustomSeedPrices = field (nameof u.CustomSeedPrices) (keySelection Decode.seedId (Decode.mapObjParse Decode.parseSeedId Decode.uint32))
          CustomFertilizerPrices = field (nameof u.CustomFertilizerPrices) (keySelection Decode.string (Decode.mapObj id Decode.uint32))
          CustomSellPrices = field (nameof u.CustomSellPrices) (keySelection (Decode.tuple2 Decode.seedId Decode.itemId) (Decode.Auto.generateDecoder ()))

          Skills = field (nameof u.Skills) (Decode.Auto.generateDecoder ())
          Multipliers = field (nameof u.Multipliers) (Decode.Auto.generateDecoder ())
          CropAmount = field (nameof u.CropAmount) (Decode.Auto.generateDecoder ())
          ModData = field (nameof u.ModData) (Decode.Auto.generateDecoder ())
          JojaMembership = field (nameof u.JojaMembership) Decode.bool
          Irrigated = field (nameof u.Irrigated) Decode.bool

          StartDate = field (nameof u.StartDate) Decode.date
          EndDate = field (nameof u.EndDate) Decode.date
          Location = field (nameof u.Location) (Decode.Auto.generateDecoder ())

          SeedStrategy = field (nameof u.SeedStrategy) (Decode.Auto.generateDecoder ())
          PayForFertilizer = field (nameof u.PayForFertilizer) Decode.bool
          ReplaceLostFertilizer = field (nameof u.ReplaceLostFertilizer) Decode.bool
        }
      )

  // validate:
  // skills
  // multipliers
  // date


  let private selectKeys allKeys selectKeys =
    selectKeys
    |> Option.defaultValue (SelectAllKeys true)
    |> SelectKeys.toSet allKeys

  let private columnSelect data columnSelections =
    columnSelections
    |> Option.defaultWith Table.empty
    |> SelectKeys.columnWise data

  let private keySelection selection = selection |> Option.defaultOrMap Selection.empty KeySelection.toSelection

  let toSettings (short: ShorthandSettings) : Settings =
    let seedItemPairs = GameData.seedItemPairs gameData
    {
      SelectedCrops = short.SelectedCrops |> selectKeys gameData.Crops.Keys
      SelectedSeedPrices = short.SelectedSeedPrices |> columnSelect gameData.SeedPrices

      AllowNoFertilizer = short.AllowNoFertilizer |> Option.defaultValue true
      SelectedFertilizers = short.SelectedFertilizers |> selectKeys gameData.Fertilizers.Keys
      SelectedFertilizerPrices = short.SelectedFertilizerPrices |> columnSelect gameData.FertilizerPrices

      SellRawItems = short.SellRawItems |> selectKeys seedItemPairs
      SelectedProducts =
        columnSelect
          (seedItemPairs |> Table.ofKeys (snd >> gameData.Products.Find))
          short.SelectedProducts
      SellForageSeeds = short.SellForageSeeds |> selectKeys gameData.ForageCrops.Keys

      UseRawSeeds =
        selectKeys
          (seedItemPairs |> Seq.choose (fun (seed, item) ->
            if int seed = int item
            then Some seed
            else None))
          short.UseRawSeeds

      UseSeedMaker =
        selectKeys
          (gameData.Crops
            |> Table.toSeq
            |> Seq.choose (fun (seed, crop) ->
              if gameData |> GameData.cropCanGetOwnSeedsFromSeedMaker crop
              then Some seed
              else None))
          short.UseSeedMaker

      UseForageSeeds = short.UseForageSeeds |> selectKeys gameData.ForageCrops.Keys

      CustomSeedPrices = keySelection short.CustomSeedPrices
      CustomFertilizerPrices = keySelection short.CustomFertilizerPrices
      CustomSellPrices = keySelection short.CustomSellPrices

      Skills = short.Skills |> Option.defaultValue Skills.zero
      Multipliers = short.Multipliers |> Option.defaultValue Multipliers.common
      CropAmount = short.CropAmount |> Option.defaultValue CropAmountSettings.common
      ModData = short.ModData |> Option.defaultValue ModData.common
      JojaMembership = short.Irrigated |> Option.defaultValue false
      Irrigated = short.Irrigated |> Option.defaultValue false

      StartDate = short.StartDate |> Option.defaultValue { Season = Season.Spring; Day = Date.firstDay }
      EndDate = short.EndDate |> Option.defaultValue { Season = Season.Fall; Day = Date.lastDay }
      Location = short.Location |> Option.defaultValue Farm

      SeedStrategy = short.SeedStrategy |> Option.defaultValue BuyFirstSeed
      PayForFertilizer = short.PayForFertilizer |> Option.defaultValue true
      ReplaceLostFertilizer = short.ReplaceLostFertilizer |> Option.defaultValue true
    }






let private settings =
  Extra.empty
  |> Extra.withCustom (fun (_: GameData) -> Encode.nil) (Decode.succeed Unchecked.defaultof<_>)
  |> Extra.withCustom Encode.date Decode.date
  |> Extra.withCustom
    (Encode.mapObj string (Encode.mapSeq Encode.vendor): Map<SeedId, Vendor Set> -> _)
    (Decode.mapObjParse Decode.parseSeedId (Decode.Auto.generateDecoder ()))
  |> Extra.withCustom
    (Encode.mapObj string Encode.uint32: Map<SeedId, nat> -> _)
    (Decode.mapObjParse Decode.parseSeedId Decode.uint32)

let private encodeCropFilters filters = Encode.object [
  nameof filters.Seasons, Encode.seasons filters.Seasons
  nameof filters.InSeason, Encode.bool filters.InSeason
  if filters.Giant.IsSome then
    nameof filters.Giant, Encode.bool (Option.get filters.Giant)
  if filters.Regrows.IsSome then
    nameof filters.Giant, Encode.bool (Option.get filters.Regrows)
  if filters.Forage.IsSome then
    nameof filters.Giant, Encode.bool (Option.get filters.Forage)
]

let private decodeCropFilters =
  let u = Unchecked.defaultof<CropFilters>
  Decode.object (fun get -> {
    Seasons = get.Required.Field (nameof u.Seasons) Decode.seasons
    InSeason = get.Required.Field (nameof u.InSeason) Decode.bool
    Regrows = get.Optional.Field (nameof u.Regrows) Decode.bool
    Giant = get.Optional.Field (nameof u.Giant) Decode.bool
    Forage = get.Optional.Field (nameof u.Forage) Decode.bool
  } )

let private encodeSettings = Encode.Auto.generateEncoder<Settings>(extra=settings)
let private decodeSettings = Decode.Auto.generateDecoder<Settings>(extra=settings)

let private encodeNestedOption =
  Encode.option
    (Encode.tuple2
      (Encode.option Encode.seedId)
      (function
        | None -> Encode.nil
        | Some fert -> Encode.object [ "Some", Encode.option Encode.string fert ]))

let private decodeNestedOption = Decode.succeed Unchecked.defaultof<_>

let private app =
  settings
  |> Extra.withCustom encodeCropFilters decodeCropFilters
  |> Extra.withCustom encodeSettings decodeSettings
  |> Extra.withCustom encodeNestedOption decodeNestedOption
  |> Extra.withCustom (fun (_: GameData) -> Encode.uint32 dataVersion) (Decode.succeed gameData)

let private encodeApp = Encode.Auto.generateEncoder<App>(extra=app)
let private decodeApp = Decode.Auto.generateDecoder<App>(extra=app)


module Encode =
  let settings = encodeSettings
  let cropFilters = encodeCropFilters
  let app = encodeApp


module Decode =
  let settings = decodeSettings
  let cropFilters = decodeCropFilters

  let appWithShortHandSettings =
    let extra =
      app |> Extra.withCustom
        Encode.settings
        (Shorthand.Decode.shorthandSettings |> Decode.map Shorthand.toSettings)

    Decode.Auto.generateDecoder<App>(extra=extra)

  let app: App Decoder =
    let u = Unchecked.defaultof<App>
    Decode.map2
      (fun version app ->
        if version = dataVersion then app else {
          app with
            Settings = app.Settings |> Settings.adapt gameData
            SavedSettings =
              app.SavedSettings |> List.map (fun (name, settings) ->
                name, settings |> Settings.adapt gameData)
        }
      )
      (Decode.field (nameof u.Data) Decode.uint32)
      decodeApp


let defaultApp = lazy (Decode.appWithShortHandSettings |> load appData)
