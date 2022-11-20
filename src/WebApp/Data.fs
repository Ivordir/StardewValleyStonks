module StardewValleyStonks.WebApp.Json

open StardewValleyStonks
open StardewValleyStonks.WebApp
#if FABLE_COMPILER
open Thoth.Json
#else
open Thoth.Json.Net
#endif

let [<Literal>] private KeySelectionSelectField = "Select"
let [<Literal>] private KeySelectionEntriesField = "Entries"

module Encode =
  open StardewValleyStonks.Json.Encode

  let keyValues encodeKey encodeValue seq =
    seq
    |> Seq.map (fun (k, v) -> encodeKey k, encodeValue v)
    #if !FABLE_COMPILER
    |> List.ofSeq
    #endif
    |> Encode.object

  let mapTree encodeKey encodeValue map =
    map |> Map.toSeq |> keyValues encodeKey encodeValue

  let inline table encodeKey encodeValue table =
    table |> Table.toSeq |> keyValues encodeKey encodeValue

  let location = Encode.Auto.generateEncoder<Location> ()

  let seedStrategy = Encode.Auto.generateEncoder<SeedStrategy> ()

  let selection encodeKey keyString encodeValue (selection: Selection<_,_>) =
    Encode.object [
      nameof selection.Values, selection.Values |> mapTree keyString encodeValue
      nameof selection.Selected, selection.Selected |> mapSeq encodeKey
    ]

  let model (model: Model) =
    Encode.object [
      nameof model.SelectedCrops, model.SelectedCrops |> mapSeq seedId
      nameof model.SelectedSeedPrices, model.SelectedSeedPrices |> mapTree string (mapSeq vendor)

      nameof model.AllowNoFertilizer, Encode.bool model.AllowNoFertilizer
      nameof model.SelectedFertilizers, model.SelectedFertilizers |> mapSeq fertilizerName
      nameof model.SelectedFertilizerPrices, model.SelectedFertilizerPrices |> mapTree string (mapSeq vendor)

      nameof model.SellRawItems, model.SellRawItems |> mapSeq (Encode.tuple2 seedId itemId)
      nameof model.SelectedProducts, model.SelectedProducts |> mapTree seedItemIdPair (mapSeq processor)
      nameof model.SellForageSeeds, model.SellForageSeeds |> mapSeq seedId

      nameof model.UseRawSeeds, model.UseRawSeeds |> mapSeq seedId
      nameof model.UseSeedMaker, model.UseSeedMaker |> mapSeq seedId
      nameof model.UseForageSeeds, model.UseForageSeeds |> mapSeq seedId

      nameof model.CustomSeedPrices, model.CustomSeedPrices |> selection seedId string Encode.uint32
      nameof model.CustomFertilizerPrices, model.CustomFertilizerPrices |> selection fertilizerName string Encode.uint32
      nameof model.CustomSellPrices, model.CustomSellPrices |> selection (Encode.tuple2 seedId itemId) seedItemIdPair (Encode.tuple2 Encode.uint32 Encode.bool)

      nameof model.Skills, skills model.Skills
      nameof model.Multipliers, multipliers model.Multipliers
      nameof model.CropAmount, cropAmountSettings model.CropAmount
      nameof model.ModData, modData model.ModData
      nameof model.JojaMembership, Encode.bool model.JojaMembership
      nameof model.Irrigated, Encode.bool model.Irrigated

      nameof model.StartDate, date model.StartDate
      nameof model.EndDate, date model.EndDate
      nameof model.Location, location model.Location

      nameof model.SeedStrategy, seedStrategy model.SeedStrategy
      nameof model.PayForFertilizer, Encode.bool model.PayForFertilizer
      nameof model.ReplaceLostFertilizer, Encode.bool model.ReplaceLostFertilizer
    ]

  let rankItem = Encode.Auto.generateEncoder<RankItem> ()
  let rankMetric = Encode.Auto.generateEncoder<RankMetric> ()
  let timeNormalization = Encode.Auto.generateEncoder<TimeNormalization> ()

  let ranker (ranker: Ranker) =
    Encode.object [
      nameof ranker.RankItem, rankItem ranker.RankItem
      nameof ranker.RankMetric, rankMetric ranker.RankMetric
      nameof ranker.TimeNormalization, timeNormalization ranker.TimeNormalization
      nameof ranker.BrushSpan, Encode.tuple2 Encode.uint32 Encode.uint32 ranker.BrushSpan
      nameof ranker.SelectedCropAndFertilizer,
        Encode.option
          (Encode.tuple2
            (Encode.option seedId)
            (function
              | None -> Encode.nil
              | Some fert -> Encode.object [ "Some", Encode.option fertilizerName fert ]))
          ranker.SelectedCropAndFertilizer
    ]

  let appMode = Encode.Auto.generateEncoder<AppMode> ()
  let settingsTab = Encode.Auto.generateEncoder<SettingsTab> ()
  let openDetails = Encode.Auto.generateEncoder<OpenDetails> ()
  let tableSort (SortByColumns columns) = columns |> mapSeq (Encode.tuple2 Encode.int Encode.bool)

  let cropFilters (filters: CropFilters) =
    Encode.object [
      nameof filters.InSeason, Encode.bool filters.InSeason
      nameof filters.Seasons, seasons filters.Seasons
      nameof filters.Regrows, Encode.option Encode.bool filters.Regrows
      nameof filters.Giant, Encode.option Encode.bool filters.Giant
      nameof filters.Forage, Encode.option Encode.bool filters.Forage
    ]

  let app (app: App) =
    Encode.object [
      nameof app.Model, model app.Model
      nameof app.SavedModels, app.SavedModels |> mapSeq (Encode.tuple2 Encode.string model)

      nameof app.AppMode, appMode app.AppMode
      nameof app.SettingsTab, settingsTab app.SettingsTab
      nameof app.OpenDetails, app.OpenDetails |> mapSeq openDetails

      nameof app.FertilizerSort, tableSort app.FertilizerSort
      nameof app.FertilizerPriceSort, tableSort app.FertilizerPriceSort
      nameof app.CropSort, tableSort app.CropSort
      nameof app.ProductSort, tableSort app.ProductSort
      nameof app.SeedSort, tableSort app.SeedSort
      nameof app.ProductQuality, quality app.ProductQuality
      nameof app.ShowNormalizedProductPrices, Encode.bool app.ShowNormalizedProductPrices

      nameof app.CropFilters, cropFilters app.CropFilters
      nameof app.Ranker, ranker app.Ranker
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
  open StardewValleyStonks.Json.Decode

  let location = Decode.Auto.generateDecoder<Location> ()

  let seedStrategy = Decode.Auto.generateDecoder ()

  let rawGameData: _ -> _ -> _ -> RawGameData Decoder =
    let u = Unchecked.defaultof<RawGameData>
    fun items farmCrops forageCrops ->
      Decode.object (fun get ->
        let field name decode = get.Required.Field name decode
        {
          FarmCrops = farmCrops
          ForageCrops = forageCrops
          Fertilizers = field (nameof u.Fertilizers) (Decode.array fertilizer)
          Items = items
          Products = field (nameof u.Products) (tableParse Helpers.parseItemId (Decode.array product))
          ProcessorUnlockLevel = field (nameof u.ProcessorUnlockLevel) (table ProcessorName Decode.uint32)
          FertilizerPrices = field (nameof u.FertilizerPrices) (table FertName (table VendorName Decode.uint32))
          SeedPrices = field (nameof u.SeedPrices) (tableParse Helpers.parseSeedId (Decode.array seedPrice))
          GenerateSeedPrices = field (nameof u.GenerateSeedPrices) (wrapKeys id VendorName (Decode.array seedId))
        })
      |> Helpers.checkOk (fun data ->
        let crops =
          data.FarmCrops
          |> Array.map FarmCrop
          |> Array.append (data.ForageCrops |> Array.map ForageCrop)

        let missing =
          crops
          |> Seq.collect Crop.items
          |> Seq.append (crops |> Seq.map Crop.seedItem)
          |> Seq.append (data.Products.Values |> Seq.concat |> Seq.choose Product.item)
          |> Seq.filter (data.Items.ContainsKey >> not)
          |> Seq.map string
          |> Array.ofSeq

        if missing.Length = 0
        then Ok data
        else Error ("The following items ids were referenced, but no items with the ids were provided: " + String.concat ", " missing))

  let private sortKeysByHighestCount table =
    table
    |> Table.values
    |> Seq.collect Table.keys
    |> Seq.countBy id
    |> Seq.sortByDescending snd
    |> Seq.map fst
    |> Array.ofSeq

  let gameDataFromRaw rawData =
    let crops =
      rawData.FarmCrops
      |> Array.map FarmCrop
      |> Array.append (rawData.ForageCrops |> Array.map ForageCrop)

    let seedPrices =
      let generated =
        rawData.GenerateSeedPrices
        |> Seq.collect (fun (vendor, seeds) ->
          seeds |> Array.map (fun seed -> seed, ScalingPrice (vendor, None)))
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
          then Some (Crop.mainItem crop, SeedsFromSeedMaker (Crop.seedItem crop))
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
        |> Array.ofSeq
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

  let private columnSelectSeq columnKey selectionKey data =
    table columnKey (select selectionKey) |> Decode.map (fun columnSelections ->
      data |> Seq.map (fun (key, columns) ->
        key,
        columns
        |> Table.keys
        |> Seq.filter (fun column ->
          match columnSelections.TryFind column with
          | None -> true
          | Some select ->
            match select with
            | SelectAllKeys b -> b
            | SelectKeys (true, keys) -> keys.Contains key
            | SelectKeys (false, keys) -> not <| keys.Contains key)
        |> Set.ofSeq)
      |> Map.ofSeq)

  let inline private columnSelect columnKey selectionKey table =
    table |> Table.toSeq |> columnSelectSeq columnKey selectionKey

  let selection key values =
    let u = Unchecked.defaultof<Selection<_,_>>
    Decode.map2
      (fun values selected ->
        { Values = values
          Selected = selectWith (Map.keys values) selected })
      (Decode.field (nameof u.Values) values)
      (Decode.field (nameof u.Selected) (select key))

  let seedItemIdPair (str: string) =
    match str.Split ',' with
    | [| seed; item |] -> Option.map2 tuple2 (Helpers.parseSeedId seed) (Helpers.parseItemId item)
    | _ -> None

  let model: _ -> Model Decoder =
    let u = Unchecked.defaultof<Model>
    fun (data: GameData) ->
      Decode.object (fun get ->
        let field name decode = get.Required.Field name decode
        {
          Data = data

          SelectedCrops = field (nameof u.SelectedCrops) (set seedId)
          SelectedSeedPrices = field (nameof u.SelectedSeedPrices) (mapTreeParse Helpers.parseSeedId (set vendor))

          SelectedFertilizers = field (nameof u.SelectedFertilizers) (set fertilizerName)
          AllowNoFertilizer = field (nameof u.AllowNoFertilizer) Decode.bool
          SelectedFertilizerPrices = field (nameof u.SelectedFertilizerPrices) (mapTree FertName (set vendor))

          SellRawItems = field (nameof u.SellRawItems) (set (Decode.tuple2 seedId itemId))
          SelectedProducts = field (nameof u.SelectedProducts) (mapTreeParse seedItemIdPair (set processor))
          SellForageSeeds = field (nameof u.SellForageSeeds) (set seedId)

          UseRawSeeds = field (nameof u.UseRawSeeds) (set seedId)
          UseSeedMaker = field (nameof u.UseSeedMaker) (set seedId)
          UseForageSeeds = field (nameof u.UseForageSeeds) (set seedId)

          CustomFertilizerPrices = field (nameof u.CustomFertilizerPrices) (selection fertilizerName (mapTree FertName Decode.uint32))
          CustomSeedPrices = field (nameof u.CustomSeedPrices) (selection seedId (mapTreeParse Helpers.parseSeedId Decode.uint32))
          CustomSellPrices =
            field
              (nameof u.CustomSellPrices)
              (selection
                (Decode.tuple2 seedId itemId)
                (mapTreeParse seedItemIdPair (Decode.tuple2 Decode.uint32 Decode.bool)))

          Skills = field (nameof u.Skills) skills
          Multipliers = field (nameof u.Multipliers) multipliers
          CropAmount = field (nameof u.CropAmount) cropAmountSettings
          ModData = field (nameof u.ModData) modData
          JojaMembership = field (nameof u.JojaMembership) Decode.bool
          Irrigated = field (nameof u.Irrigated) Decode.bool

          StartDate = field (nameof u.StartDate) date
          EndDate = field (nameof u.EndDate) date
          Location = field (nameof u.Location) location

          SeedStrategy = field (nameof u.SeedStrategy) seedStrategy
          PayForFertilizer = field (nameof u.PayForFertilizer) Decode.bool
          ReplaceLostFertilizer = field (nameof u.ReplaceLostFertilizer) Decode.bool
        })


  let newModel: _ -> Model Decoder =
    let u = Unchecked.defaultof<Model>
    fun (data: GameData) ->
      let seedItemPairs =
        data.Crops
        |> Table.toSeq
        |> Seq.collect (fun (seed, crop) ->
          Crop.items crop |> Array.map (fun item -> (seed, item)))
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
        let field name decode = get.Required.Field name decode
        {
          Data = data

          SelectedCrops = field (nameof u.SelectedCrops) (selectFrom data.Crops.Keys seedId)
          SelectedSeedPrices = field (nameof u.SelectedSeedPrices) (columnSelect VendorName seedId data.SeedPrices)

          SelectedFertilizers = field (nameof u.SelectedFertilizers) (selectFrom data.Fertilizers.Keys fertilizerName)
          AllowNoFertilizer = field (nameof u.AllowNoFertilizer) Decode.bool
          SelectedFertilizerPrices = field (nameof u.SelectedFertilizerPrices) (columnSelect VendorName fertilizerName data.FertilizerPrices)

          SellRawItems = field (nameof u.SellRawItems) (selectFrom seedItemPairs (Decode.tuple2 seedId itemId))
          SelectedProducts =
            field
              (nameof u.SelectedProducts)
              (seedItemPairs |> Seq.map (fun (seed, item) -> (seed, item), data.Products[item])
                |> columnSelectSeq ProcessorName (Decode.tuple2 seedId itemId))
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
              (selection seedId (mapTreeParse Helpers.parseSeedId Decode.uint32))
            |> Option.defaultValue Selection.empty
          CustomSellPrices =
            get.Optional.Field
              (nameof u.CustomSellPrices)
              (selection
                (Decode.tuple2 seedId itemId)
                (mapTreeParse seedItemIdPair (Decode.tuple2 Decode.uint32 Decode.bool)))
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

          SeedStrategy = field (nameof u.SeedStrategy) seedStrategy
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
    Decode.object (fun get ->
      let field name decode = get.Required.Field name decode
      {
        InSeason = field (nameof u.InSeason) Decode.bool
        Seasons = field (nameof u.Seasons) seasons
        Regrows = field (nameof u.Regrows) (Decode.option Decode.bool)
        Giant = field (nameof u.Giant) (Decode.option Decode.bool)
        Forage = field (nameof u.Forage) (Decode.option Decode.bool)
      }
    )

  let rankMetric = Decode.Auto.generateDecoder ()
  let timeNormalization = Decode.Auto.generateDecoder ()
  let rankItem = Decode.Auto.generateDecoder ()

  let ranker =
    let u = Unchecked.defaultof<Ranker>
    Decode.object (fun get ->
      let field name decode = get.Required.Field name decode
      {
        RankItem = field (nameof u.RankItem) rankItem
        RankMetric = field (nameof u.RankMetric) rankMetric
        TimeNormalization = field (nameof u.TimeNormalization) timeNormalization
        BrushSpan = field (nameof u.BrushSpan) (Decode.tuple2 Decode.uint32 Decode.uint32 |> Helpers.validate "the end of the brush to be on or after the start of the brush" (uncurry (<=)))
        SelectedCropAndFertilizer =
          field
            (nameof u.SelectedCropAndFertilizer)
            (Decode.option
              (Decode.tuple2
                (Decode.option seedId)
                (Decode.oneOf [
                  Decode.nil None
                  Decode.object (fun get -> Some <| get.Required.Field "Some" (Decode.option fertilizerName))
                ])))
      }
    )

  let app: _ -> App Decoder =
    let u = Unchecked.defaultof<App>
    fun defaultModel ->
      Decode.object (fun get ->
        let field name decode = get.Required.Field name decode
        {
          DefaultModel = defaultModel

          Model = field (nameof u.Model) (model defaultModel.Data)
          SavedModels = field (nameof u.SavedModels) (Decode.list (Decode.tuple2 Decode.string (model defaultModel.Data)))

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
      )

  let newApp: _ -> App Decoder =
    let u = Unchecked.defaultof<App>
    fun model ->
      Decode.object (fun get ->
        let field name decode = get.Required.Field name decode
        {
          DefaultModel = model
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
      )


open StardewValleyStonks.Json
open Fable.Core.JsInterop

let inline private load url decoder = importAll url |> Decode.fromValue "$" decoder

let loadGameData () =
  load "../../public/Data/Items.json" (Decode.array Decode.item)
  |> Result.bind (fun items ->
    load
      "../../public/Data/Crops.json"
      (Decode.object (fun get -> {|
        FarmCrops = get.Required.Field "FarmCrops" (Decode.array Decode.farmCrop)
        ForageCrops = get.Required.Field "ForageCrops" (Decode.array Decode.forageCrop)
      |}))
  |> Result.bind (fun crops ->
    load "../../public/Data/Data.json5" (Decode.rawGameData (Table.ofValues Item.id items) crops.FarmCrops crops.ForageCrops)
  |> Result.map Decode.gameDataFromRaw))

let loadDefaultModel () =
  loadGameData () |> Result.bind (Decode.newModel >> load "../../public/Data/Model.json5")

let loadDefaultApp () =
  loadDefaultModel () |> Result.bind (Decode.newApp >> load "../../public/Data/App.json5")
