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

// Major = new schema -> ???
// Minor = new crops, fertilizers, or items on crops -> adapt settings
// Patch = compatible data -> no action needed
let private dataVersion = Version.parse "0.1.0" |> Option.get

let [<Literal>] private localStorageKey = "app"
let [<Literal>] private localStorageBackupKey = "backup"

let private extractedData: JsonValue = importAll "../../public/data/Extracted.json"
let private supplementalData: JsonValue = importAll "../../public/data/Supplemental.json5"
let private appData: JsonValue = importAll "../../public/data/App.json5"

let private load json decoder =
  json
  |> Decode.fromValue "$" decoder
  |> Result.get


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
    data
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

let private gameData =
  GameData.fromExtractedAndSupplementalData
    (load extractedData Decode.extractedData)
    (load supplementalData Decode.supplementalData)

#if DEBUG
do
  let missingItems = GameData.missingItemIds gameData |> Seq.map string |> Array.ofSeq
  if missingItems.Length > 0 then
    failwith ("The following items ids were referenced, but no items with the ids were provided: " + String.concat ", " missingItems)
// validate:
// fertilizers
// products
// crops
#endif



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
        | Some None -> Encode.string ""
        | Some (Some fert) -> Encode.string fert))

let private decodeNestedOption =
  Decode.option
    (Decode.tuple2
      (Decode.option Decode.seedId)
      (Decode.oneOf [
        Decode.nil None
        Decode.string |> Decode.map (function | "" -> Some None | fert -> Some (Some fert))
      ] ))

let private app =
  settings
  |> Extra.withCustom encodeCropFilters decodeCropFilters
  |> Extra.withCustom encodeSettings decodeSettings
  |> Extra.withCustom encodeNestedOption decodeNestedOption
  |> Extra.withCustom (fun (_: GameData) -> Encode.string dataVersion.String) (Decode.succeed gameData)

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
    Decode.map2 tuple2
      (Decode.field
        (nameof u.Data)
        (Decode.string |> Decode.andThen (fun str ->
          match Version.parse str with
          | Some ver -> Decode.succeed ver
          | None -> Decode.fail "invalid data version")))
      decodeApp
    |> Decode.andThen (fun (version, app) ->
        if dataVersion.Major <> version.Major then
          Decode.fail "Unsupported version" //TODO
        elif dataVersion.Minor <> version.Minor then
          Decode.succeed {
            app with
              Settings = app.Settings |> Settings.adapt gameData
              SavedSettings =
                app.SavedSettings |> List.map (fun (name, settings) ->
                  name, settings |> Settings.adapt gameData)
          }
        else
          Decode.succeed app)



let defaultApp = lazy (Decode.appWithShortHandSettings |> load appData)

#if DEBUG
// validate for all settings:
// skills
// multipliers
// date
// cropamount
#endif



let saveAppToLocalStorage app =
  Fable.Core.JS.console.time "encode"
  let data = Encode.toString 0 <| Encode.app app
  Fable.Core.JS.console.timeEnd "encode"
  Browser.WebStorage.localStorage.setItem (localStorageKey, data)


let loadAppFromLocalStorage () =
  let data = Browser.WebStorage.localStorage.getItem localStorageKey
  if isNullOrUndefined data then
    defaultApp.Value
  else
    match Decode.fromString Decode.app data with
    | Ok app -> app
    | Error e ->
      printfn $"Failed to load app from local storage: {e}"
      printfn "Backing up app data..."
      Browser.WebStorage.localStorage.setItem (localStorageBackupKey, data)
      defaultApp.Value



// Rather than depending on an xml parsing library, we use the browser's built-in native xml parsing.
// Stardew valley saves games are simple enough and/or we extract so little data from them
// such that the following manual code is workable...
module private XML =
  open Fable.Core

  let [<Emit "XPathResult.STRING_TYPE">] stringType: obj = jsNative
  let [<Emit "XPathResult.UNORDERED_NODE_ITERATOR_TYPE">] nodeIter: obj = jsNative
  let [<Emit "XPathResult.UNORDERED_NODE_ITERATOR_TYPE">] firstNode: obj = jsNative

  let private getString (doc: Browser.Types.XMLDocument) (node: obj) path : string =
    doc?evaluate(path, node, null, stringType, null)?stringValue

  let stringValue doc node path =
    let value = getString doc node path
    if value = "" then None else Some value

  let private parseWith parser doc node path =
    match parser (getString doc node path) with
    | true, value -> Some value
    | _ -> None

  let boolValue = parseWith System.Boolean.TryParse
  let natValue = parseWith System.UInt32.TryParse
  let floatValue = parseWith System.Double.TryParse

  let private array typePath parser (doc: Browser.Types.XMLDocument) (node: obj) path =
    let arr = ResizeArray ()
    let iter = doc?evaluate(path + "/" + typePath, node, null, nodeIter, null)

    let mutable finished = false
    while not finished do
      let elm: Browser.Types.Element option = iter?iterateNext()
      match elm with
      | Some e ->
        match parser e.textContent with
        | Some value -> arr.Add value
        | None -> ()
      | None ->
        finished <- true

    resizeToArray arr

  let natArray = array "int" (fun str ->
    match System.UInt32.TryParse str with
    | true, value -> Some value
    | _ -> None)

  let stringArray = array "string" (function | "" -> None | str -> Some str)

  let [<Emit "new DOMParser()">] private domParser (): obj = jsNative
  let private xmlParser = domParser ()

  let parse (xml: string): Browser.Types.XMLDocument = xmlParser?parseFromString(xml, "text/xml")

open XML

let loadSettingsFromSaveGame (xml: string) =
  try
    let doc = parse xml

    match doc?evaluate("Farmer", doc, null, firstNode, null) with
    | None -> failwith "Invalid save game: no Farmer was found"
    | Some (farmer: obj) ->

    let professions = natArray doc farmer "professions"
    let eventsSeen = natArray doc farmer "eventsSeen"
    let mailReceived = stringArray doc farmer "mailReceived"

    let missing = ResizeArray ()
    let tryGet name parser path defaultValue =
      match parser doc farmer path with
      | Some value -> value
      | None ->
        missing.Add name
        defaultValue

    let specialCharm = tryGet "Special Charm" boolValue "hasSpecialCharm" false
    let farmingLevel = tryGet "Farming Level" natValue "farmingLevel" 0u |> min Skill.maxLevel
    let foragingLevel = tryGet "Foraging Level" natValue "foragingLevel" 0u |> min Skill.maxLevel
    let day = tryGet "Day" natValue "dayOfMonthForSaveGame" Date.firstDay |> max Date.firstDay |> min Date.lastDay
    let season = tryGet "Season" natValue "seasonForSaveGame" (uint Season.Spring) |> int |> enum
    let profitMargin = tryGet "Difficulty Modifier" floatValue "difficultyModifier" 1.0
    let farmerName = tryGet "Farmer Name" stringValue "name" "Imported Save"
    let settings = {
      defaultApp.Value.Settings with
        Skills = {
          Skills.zero with
            Farming = { Skill.zero with Level = farmingLevel }
            Foraging = { Skill.zero with Level = foragingLevel }
            Professions = Set.ofList [
              if professions |> Array.contains 1u then Tiller
              if professions |> Array.contains 4u then Artisan
              if professions |> Array.contains 5u then Agriculturist
              if professions |> Array.contains 13u then Gatherer
              if professions |> Array.contains 16u then Botanist
            ]
        }
        Multipliers = {
          Multipliers.common with
            ProfitMargin = profitMargin
            BearsKnowledge = eventsSeen |> Array.contains 2120303u
        }
        CropAmount = { CropAmountSettings.common with SpecialCharm = specialCharm }
        StartDate = { Season = season; Day = day }
        JojaMembership = mailReceived |> Array.contains "JojaMember"
    }

    Ok ((farmerName, settings), resizeToArray missing)

  with e -> Error e
