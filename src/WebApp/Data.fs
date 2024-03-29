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

let private extractedData: JsonValue = importDefault "../../public/data/Extracted.json"
let private supplementalData: JsonValue = importDefault "../../public/data/Supplemental.json"

let private load json decoder =
  json
  |> Decode.fromValue "$" decoder
  |> Result.get

let private gameData =
  GameData.fromExtractedAndSupplementalData
    (load extractedData Decode.extractedData)
    (load supplementalData Decode.supplementalData)

assert // no missing item references
  [|
    gameData.Crops.Values |> Seq.collect Crop.items
    gameData.Crops.Values |> Seq.map Crop.seedItem
    gameData.Products.Values
    |> Seq.collect Table.values
    |> Seq.map _.Item
  |]
  |> Seq.concat
  |> Seq.forall gameData.Items.ContainsKey

assert // no negative speeds
  gameData.Fertilizers.Values |> Seq.forall (fun fertilizer -> fertilizer.Speed >= Fertilizer.minSpeed)

assert // all seed items have the Seeds category
  gameData.Crops.Values |> Seq.forall (Crop.seedItem >> gameData.Items.Find >> _.Category >> (=) Seeds)

assert // non-zero growth stages
  gameData.Crops.Values |> Seq.forall (Crop.stages >> Array.contains 0u >> not)

assert // non-zero total growth times
  gameData.Crops.Values |> Seq.forall (Crop.growthTime >> (<>) 0u)

assert // non-zero regrow times
  gameData.Crops.Values |> Seq.forall (Crop.regrowTime >> Option.contains 0u >> not)

assert // supported/valid extra item quantities
  gameData.Crops.Values |> Seq.forall (function
    | FarmCrop crop ->
      crop.ExtraItem |> Option.forall (fun (item, quantity) ->
        if crop.RegrowTime.IsSome && nat item = nat crop.Seed
        then quantity >= 1.0
        else quantity >= FarmCrop.minExtraItemQuantity)
    | ForageCrop _ -> true)

assert // crop amounts have values in the valid ranges
  gameData.Crops.Values |> Seq.forall (function
    | FarmCrop crop ->
      let amount = crop.Amount
      CropAmount.minExtraCropChance <= amount.ExtraCropChance && amount.ExtraCropChance <= CropAmount.maxExtraCropChance
      && CropAmount.minYield <= amount.MinCropYield && amount.MinCropYield <= amount.MaxCropYield
    | ForageCrop _ -> true)

assert // forage crops have a number of items in the supported range
  gameData.Crops.Values |> Seq.forall (function
    | FarmCrop _ -> true
    | ForageCrop crop ->
      let len = nat crop.Foragables.Length
      ForageCrop.minItems <= len && len <= ForageCrop.maxItems)

assert // valid ratios (no zeros)
  gameData.Products.Values
  |> Seq.collect Table.values
  |> Seq.forall (_.Ratio >> Option.forall (fun (i, o) -> i <> 0u && o <> 0u))


let private settingsCoders = Extra.empty |> Extra.withCustom Encode.date Decode.date

let private encodeCropFilters filters = Encode.object [
  nameof filters.ItemNameSearch, Encode.string filters.ItemNameSearch
  nameof filters.Seasons, Encode.seasons filters.Seasons
  nameof filters.InSeason, Encode.bool filters.InSeason
  if filters.Giant.IsSome then
    nameof filters.Giant, Encode.bool filters.Giant.Value
  if filters.Regrows.IsSome then
    nameof filters.Giant, Encode.bool filters.Regrows.Value
  if filters.Forage.IsSome then
    nameof filters.Giant, Encode.bool filters.Forage.Value
]

let private decodeCropFilters =
  let u = Unchecked.defaultof<CropFilters>
  Decode.object (fun get -> {
    ItemNameSearch = get.Required.Field (nameof u.ItemNameSearch) Decode.string
    Seasons = get.Required.Field (nameof u.Seasons) Decode.seasons
    InSeason = get.Required.Field (nameof u.InSeason) Decode.bool
    Regrows = get.Optional.Field (nameof u.Regrows) Decode.bool
    Giant = get.Optional.Field (nameof u.Giant) Decode.bool
    Forage = get.Optional.Field (nameof u.Forage) Decode.bool
  })

let private encodeSettings = Encode.Auto.generateEncoder<Settings> (extra = settingsCoders)
let private decodeSettings = Decode.Auto.generateDecoder<Settings> (extra = settingsCoders)

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
        Decode.string |> Decode.map (function
          | "" -> Some None
          | fert -> Some (Some fert))
      ]))

let private uiCoders =
  Extra.empty
  |> Extra.withCustom encodeCropFilters decodeCropFilters
  |> Extra.withCustom encodeNestedOption decodeNestedOption

let private encodeUI = Encode.Auto.generateEncoder<UIState> (extra = uiCoders)
let private decodeUI = Decode.Auto.generateDecoder<UIState> (extra = uiCoders)


[<RequireQualifiedAccess>]
module Encode =
  let settings = encodeSettings
  let cropFilters = encodeCropFilters
  let ui = encodeUI
  let state = Encode.tuple2 settings ui
  let preset preset = Encode.object [
    nameof preset.Name, Encode.string preset.Name
    if preset.UniqueId.IsSome then
      nameof preset.UniqueId, Encode.int64 preset.UniqueId.Value
    nameof preset.Settings, encodeSettings preset.Settings
  ]


[<RequireQualifiedAccess>]
module Decode =
  let settings = decodeSettings
  let cropFilters = decodeCropFilters
  let ui = decodeUI
  let state = Decode.tuple2 settings ui
  let preset =
    let u = Unchecked.defaultof<Preset>
    Decode.object (fun get -> {
      Name = get.Required.Field (nameof u.Name) Decode.string
      UniqueId = get.Optional.Field (nameof u.UniqueId) Decode.int64
      Settings = get.Required.Field (nameof u.Settings) decodeSettings
    })


let defaultSettings =
  let selections = Selections.createAllSelected gameData
  {
    Selected = {
      selections with
        CustomFertilizerPrices.Values = Map.ofArray [|
          "Basic Fertilizer", 4u
          "Quality Fertilizer", 19u
          "Deluxe Fertilizer", 216u
          "Speed-Gro", 25u
          "Deluxe Speed-Gro", 42u
          "Hyper Speed-Gro", 376u
        |]

        CustomSeedPrices.Values = Map.ofArray [|
          831u<_>, 24u
          833u<_>, 400u
          885u<_>, 8u
        |]

        CustomSellPrices.Values = Map.ofArray [|
          495u<_>, (294u, false)
          496u<_>, (294u, false)
          497u<_>, (294u, false)
          498u<_>, (294u, false)
        |]
    }
    Game = GameVariables.common
    Profit = {
      SeedStrategy = BuyFirstSeed
      PayForFertilizer = true
      PayForDestroyedFertilizer = true
    }
  }

let defaultPresets = [
  {
    Name = "Year 1"
    UniqueId = None
    Settings = {
      defaultSettings with
        Selected.Crops =
          defaultSettings.Selected.Crops - Set.ofArray [|
            347u<_>; 476u<_>; 478u<_>; 485u<_>; 486u<_>; 489u<_>
            494u<_>; 499u<_>; 802u<_>; 831u<_>; 833u<_>; 885u<_>
          |]
        Selected.Fertilizers = Set.ofArray [| "Basic Fertilizer"; "Speed-Gro" |]
        Selected.Products = defaultSettings.Selected.Products |> Map.map (fun _ products ->
          products |> Set.remove Processor.mill)
    }
  }
  {
    Name = "End Game"
    UniqueId = None
    Settings = {
      defaultSettings with
        Game.Skills.Farming.Level = Skill.maxLevel
        Game.Skills.Foraging.Level = Skill.maxLevel
        Game.Skills.Professions = Set.ofArray [| Tiller; Artisan; Gatherer; Botanist |]
        Game.Multipliers.BearsKnowledge = true
        Game.CropAmount.SpecialCharm = true
        Profit.SeedStrategy = StockpileSeeds
    }
  }
]

let defaultRanker = {
  RankItem = RankCrops
  RankMetric = Gold
  TimeNormalization = PerSeason
  ShowInvalid = false
  BrushSpan = 0u, 20u
  SelectedCropAndFertilizer = None
}

let defaultCropTabState = {
  Filters = CropFilters.empty
  CropSort = 4u, true
  ProductSort = 0u, true
  SeedSort = 0u, true
  ProductQuality = Quality.Normal
  NormalizeProductPrices = false
}

let defaultUI = {
  Mode = Ranker
  Ranker = defaultRanker
  OptimizationObjective = MaximizeGold
  SettingsTab = Misc
  OpenDetails = Set.ofArray [|
    OpenDetails.CropFilters
    OpenDetails.Crops
    OpenDetails.Fertilizers
    OpenDetails.Skills
    OpenDetails.RankerSummary
    OpenDetails.RankerGrowthCalendar
    OpenDetails.OptimizerSummary
    OpenDetails.OptimizerGrowthCalendar
  |]
  CropTab = defaultCropTabState
  FertilizerSort = 0u, true
  FertilizerPriceSort = 0u, true
}

let defaultApp = {
  Data = gameData
  State = defaultSettings, defaultUI
  Presets = defaultPresets
}


[<RequireQualifiedAccess>]
module LocalStorage =
  let [<Literal>] VersionKey = "Version"
  let [<Literal>] StateKey = "State"
  let [<Literal>] PresetsKey = "Presets"

  let inline private getItem key = Browser.WebStorage.localStorage.getItem key |> unbox<string option>
  let inline private setItem key data =
    try Browser.WebStorage.localStorage.setItem (key, data)
    with e ->
      // setItem can throw if localstorage is full
      console.error (errorWithMessage $"Failed to save {key}, local storage might be full." e)

  let private saveVersion () = setItem VersionKey (string App.version)

  let private trySave key encoder value =
    value
    |> encoder
    |> Encode.toString 0
    |> setItem key

  let saveState state = trySave StateKey Encode.state state
  let savePresets presets = trySave PresetsKey (Encode.mapSeq Encode.preset) presets

  let private saveAll app =
    saveVersion ()
    saveState app.State
    savePresets app.Presets

  let private tryLoad key decoder =
    match getItem key with
    | None ->
      console.error $"Failed to load {key} from local storage: key not found."
      None
    | Some json ->
      match json |> Decode.fromString decoder with
      | Ok app -> Some app
      | Error e ->
        console.error $"Failed to load {key} from local storage: {e}"
        None

  let private loadState () =
    tryLoad StateKey Decode.state |> Option.defaultValue defaultApp.State

  let private loadPresets () =
    tryLoad PresetsKey (Decode.list Decode.preset) |> Option.defaultValue []

  let loadApp () =
    match getItem VersionKey |> Option.map Version.tryParse with
    | None ->
      // assume first time loading
      console.info "No version key found in local storage, loading default app..."
      saveAll defaultApp
      defaultApp

    | Some None ->
      // something's really wrong if this is the case
      failwith $"Invalid version: {getItem VersionKey}"

    | Some (Some ver) when ver.Major <> App.version.Major ->
      failwith $"Unexpected major version: {ver.Major}"
      // load and convert data here (once next version comes out)
      // console.info $"Upgrading to version {ver.Major}..."

    | Some (Some ver) ->
      let app = {
        Data = gameData
        State = loadState ()
        Presets = loadPresets ()
      }

      if ver = App.version then app else

      let app = App.ensureEntries gameData app
      saveAll app
      app

  let inline private reload () = Browser.Dom.window.location.reload ()

  let subscribe dispatch =
    Browser.Dom.window.onstorage <- fun e ->
      match e.key with
      | null ->
        // local storage was cleared (nuclear reset triggered)
        reload ()

      | VersionKey when isNullOrUndefined e.newValue ->
        // user manually deleted version key?
        reload ()

      | VersionKey ->
        // A new version was pushed between two tab opens (this is probably rare)?
        // Just force reload this out-of-date tab if necessary.
        match Version.tryParse e.newValue with
        | Some ver when ver = App.version -> ()
        | _ -> reload () // out of date -> fetch updated scripts, etc.

      | PresetsKey when isNullOrUndefined e.newValue ->
        // user manually deleted key?
        dispatch []

      | PresetsKey ->
        // sync presets
        match e.newValue |> Decode.fromString (Decode.list Decode.preset) with
        | Ok preset -> dispatch preset
        | Error e ->
          // failed to decode presets, presets may now race instead of syncing.
          console.error $"Failed to load saved settings from local storage: {e}"

      | StateKey -> ()

      | key -> console.warn $"Unknown key from storage event: {key}"

  let inline clear () = Browser.WebStorage.localStorage.clear ()


// Rather than depending on an xml parsing library, we use the browser's built-in (native) xml parsing.
// Stardew Valley saves games are simple enough and/or we extract so little data from them
// such that the following manual code is workable...
module private XML =
  open Fable.Core

  let [<Emit "XPathResult.STRING_TYPE">] stringType: obj = jsNative
  let [<Emit "XPathResult.UNORDERED_NODE_ITERATOR_TYPE">] nodeIter: obj = jsNative
  let [<Emit "XPathResult.FIRST_ORDERED_NODE_TYPE">] firstNode: obj = jsNative

  let private getString (doc: Browser.Types.XMLDocument) (node: obj) (path: string): string =
    doc?evaluate(path, node, null, stringType, null)?stringValue

  let stringValue doc node path =
    let value = getString doc node path
    if value = "" then None else Some value

  let private parseWith parser doc node path =
    match parser (getString doc node path) with
    | true, value -> Some value
    | _ -> None

  let natValue = parseWith System.UInt32.TryParse
  let int64Value = parseWith System.Int64.TryParse
  let floatValue = parseWith System.Double.TryParse

  let optionBoolValue doc node path =
    doc?evaluate(path, node, null, firstNode, null)?singleNodeValue |> Option.bind (fun node ->
      match node?attributes?("xsi:nil") with
      | Some attr ->
        attr?value |> Option.bind (fun (str: string) ->
          match System.Boolean.TryParse str with
          | true, true -> Some None
          | _ -> None)
      | None ->
        match System.Boolean.TryParse (getString doc node ".") with
        | true, value -> value |> Some |> Some
        | _ -> None)

  let private array typePath parser (doc: Browser.Types.XMLDocument) (node: obj) path =
    doc?evaluate(path, node, null, firstNode, null)?singleNodeValue |> Option.map (fun arrNode ->
      let arr = ResizeArray ()
      let iter = doc?evaluate(typePath, arrNode, null, nodeIter, null)

      let mutable finished = false
      while not finished do
        match iter?iterateNext() with
        | Some (e: Browser.Types.Element) -> parser e.textContent |> Option.iter arr.Add
        | None -> finished <- true

      resizeToArray arr)

  let natArray = array "int" (fun str ->
    match System.UInt32.TryParse str with
    | true, value -> Some value
    | _ -> None)

  let stringArray = array "string" (function | "" -> None | str -> Some str)

  let [<Emit "new DOMParser()">] private domParser (): obj = jsNative
  let private xmlParser = domParser ()

  let parse (xml: string): Browser.Types.XMLDocument = xmlParser?parseFromString(xml, "text/xml")

open XML

let loadSaveGame xml =
  let doc = parse xml
  doc?evaluate("Farmer", doc, null, firstNode, null)?singleNodeValue |> Option.map (fun (farmer: obj) ->
    let missing = ResizeArray ()
    let tryGet name parser path defaultValue =
      match parser doc farmer path with
      | Some value -> value
      | None ->
        missing.Add name
        defaultValue

    let professions = tryGet "Professions" natArray "professions" [||]
    let eventsSeen = tryGet "Seen Events" natArray "eventsSeen" [||]
    let mailReceived = tryGet "Received Mail" stringArray "mailReceived" [||]
    let specialCharm =
      tryGet "Special Charm" optionBoolValue "hasSpecialCharm" (Some CropAmountSettings.common.SpecialCharm)
      |> Option.defaultValue CropAmountSettings.common.SpecialCharm

    let farmingLevel = tryGet "Farming Level" natValue "farmingLevel" Skill.zero.Level |> min Skill.maxLevel
    let foragingLevel = tryGet "Foraging Level" natValue "foragingLevel" Skill.zero.Level |> min Skill.maxLevel
    let day = tryGet "Day" natValue "dayOfMonthForSaveGame" Date.firstDay |> max Date.firstDay |> min Date.lastDay
    let season = tryGet "Season" natValue "seasonForSaveGame" (uint Season.Spring) |> int |> enum
    let profitMargin = tryGet "Difficulty Modifier" floatValue "difficultyModifier" Multipliers.common.ProfitMargin
    let farmerName = tryGet "Farmer Name" stringValue "name" "Imported Save"
    let uniqueId = int64Value doc farmer "UniqueMultiplayerID"
    if uniqueId.IsNone then missing.Add "Unique Game ID"
    let professions =
      [|
        Tiller, 1u
        Artisan, 4u
        Agriculturist, 5u
        Gatherer, 13u
        Botanist, 16u
      |]
      |> Array.choose (fun (profession, number) ->
        if professions |> Array.contains number
        then Some profession
        else None)
      |> Set.ofArray

    {
      Name = farmerName
      UniqueId = uniqueId
      Settings = {
        defaultSettings with
          Game.Skills.Farming.Level = farmingLevel
          Game.Skills.Foraging.Level = foragingLevel
          Game.Skills.Professions = professions
          Game.Multipliers.ProfitMargin = profitMargin
          Game.Multipliers.BearsKnowledge = eventsSeen |> Array.contains 2120303u
          Game.CropAmount.SpecialCharm = specialCharm
          Game.StartDate = { Season = season; Day = day }
          Game.JojaMembership = mailReceived |> Array.contains "JojaMember"
      }
    },
    resizeToArray missing)
