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

let private extractedData: JsonValue = importAll "../../public/data/Extracted.json"
let private supplementalData: JsonValue = importAll "../../public/data/Supplemental.json5"
let private settingsData: JsonValue = importAll "../../public/data/Settings.json5"

let private load json decoder =
  json
  |> Decode.fromValue "$" decoder
  |> Result.get

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
// if regrow: extraItem amount >= 1.0 if extraItem = seed
// if forage: no items = seed
// seedItem.Category = Seed
#endif



module private Shorthand =
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
          | Some (SelectAllKeys b) -> b
          | Some (SelectKeys (select, keys)) -> select = keys.Contains key)
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

    SellRaw: (SeedId * ItemId) SelectKeys option
    SelectedProducts: Table<Processor, (SeedId * ItemId) SelectKeys> option
    SellForageSeeds: SeedId SelectKeys option

    UseHarvestedSeeds: SeedId SelectKeys option
    UseSeedMaker: SeedId SelectKeys option
    UseForageSeeds: SeedId SelectKeys option

    CustomSeedPrices: KeySelection<SeedId, nat> option
    CustomFertilizerPrices: KeySelection<FertilizerName, nat> option
    CustomSellPrices: KeySelection<SeedId * ItemId, nat * bool> option

    Skills: Skills option
    Multipliers: Multipliers option
    ModData: ModData option
    CropAmount: CropAmountSettings option
    JojaMembership: bool option
    Irrigated: bool option

    StartDate: Date option
    EndDate: Date option
    Location: Location option

    SeedStrategy: SeedStrategy option
    PayForFertilizer: bool option
    ReplaceLostFertilizer: bool option
  }

  module [<RequireQualifiedAccess>] Decode =
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

          SellRaw = field (nameof u.SellRaw) (selectKeys (Decode.tuple2 Decode.seedId Decode.itemId))
          SelectedProducts = field (nameof u.SelectedProducts) (Decode.table ProcessorName (selectKeys (Decode.tuple2 Decode.seedId Decode.itemId)))
          SellForageSeeds = field (nameof u.SellForageSeeds) (selectKeys Decode.seedId)

          UseHarvestedSeeds = field (nameof u.UseHarvestedSeeds) (selectKeys Decode.seedId)
          UseSeedMaker = field (nameof u.UseSeedMaker) (selectKeys Decode.seedId)
          UseForageSeeds = field (nameof u.UseForageSeeds) (selectKeys Decode.seedId)

          CustomSeedPrices = field (nameof u.CustomSeedPrices) (keySelection Decode.seedId (Decode.mapObjParse Decode.parseSeedId Decode.uint32))
          CustomFertilizerPrices = field (nameof u.CustomFertilizerPrices) (keySelection Decode.string (Decode.mapObj id Decode.uint32))
          CustomSellPrices = field (nameof u.CustomSellPrices) (keySelection (Decode.tuple2 Decode.seedId Decode.itemId) (Decode.Auto.generateDecoder ()))

          Skills = field (nameof u.Skills) (Decode.Auto.generateDecoder ())
          Multipliers = field (nameof u.Multipliers) (Decode.Auto.generateDecoder ())
          ModData = field (nameof u.ModData) (Decode.Auto.generateDecoder ())
          CropAmount = field (nameof u.CropAmount) (Decode.Auto.generateDecoder ())
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
      Game = {
        Skills = short.Skills |> Option.defaultValue Skills.zero
        Multipliers = short.Multipliers |> Option.defaultValue Multipliers.common
        ModData = short.ModData |> Option.defaultValue ModData.common
        CropAmount = short.CropAmount |> Option.defaultValue CropAmountSettings.common
        JojaMembership = short.Irrigated |> Option.defaultValue false
        Irrigated = short.Irrigated |> Option.defaultValue false

        StartDate = short.StartDate |> Option.defaultValue { Season = Season.Spring; Day = Date.firstDay }
        EndDate = short.EndDate |> Option.defaultValue { Season = Season.Fall; Day = Date.lastDay }
        Location = short.Location |> Option.defaultValue Farm
      }

      Profit = {
        SeedStrategy = short.SeedStrategy |> Option.defaultValue BuyFirstSeed
        PayForFertilizer = short.PayForFertilizer |> Option.defaultValue true
        ReplaceLostFertilizer = short.ReplaceLostFertilizer |> Option.defaultValue true
      }

      Selected = {
        Crops = short.SelectedCrops |> selectKeys gameData.Crops.Keys
        SeedPrices = short.SelectedSeedPrices |> columnSelect gameData.SeedPrices

        NoFertilizer = short.AllowNoFertilizer |> Option.defaultValue true
        Fertilizers = short.SelectedFertilizers |> selectKeys gameData.Fertilizers.Keys
        FertilizerPrices = short.SelectedFertilizerPrices |> columnSelect gameData.FertilizerPrices

        SellRaw = short.SellRaw |> selectKeys seedItemPairs
        Products =
          columnSelect
            (seedItemPairs |> Table.ofKeys (snd >> gameData.Products.Find))
            short.SelectedProducts
        SellForageSeeds = short.SellForageSeeds |> selectKeys gameData.ForageCrops.Keys

        UseHarvestedSeeds =
          selectKeys
            (seedItemPairs |> Seq.choose (fun (seed, item) ->
              if nat seed = nat item
              then Some seed
              else None))
            short.UseHarvestedSeeds

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
      }
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
    nameof filters.Giant, Encode.bool filters.Giant.Value
  if filters.Regrows.IsSome then
    nameof filters.Giant, Encode.bool filters.Regrows.Value
  if filters.Forage.IsSome then
    nameof filters.Giant, Encode.bool filters.Forage.Value
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
        Decode.string |> Decode.map (function
          | "" -> Some None
          | fert -> Some (Some fert))
      ] ))

let private ui =
  Extra.empty
  |> Extra.withCustom encodeCropFilters decodeCropFilters
  |> Extra.withCustom encodeNestedOption decodeNestedOption

let private encodeUI = Encode.Auto.generateEncoder<UIState> (extra=ui)
let private decodeUI = Decode.Auto.generateDecoder<UIState> (extra=ui)



module [<RequireQualifiedAccess>] Encode =
  let settings = encodeSettings
  let cropFilters = encodeCropFilters
  let ui = encodeUI

  let state state = Encode.object [
    nameof state.Settings, settings state.Settings
    nameof state.UI, ui state.UI
  ]

  let savedSettings (saved: _ list) = saved |> Encode.mapSeq (Encode.tuple2 Encode.string settings)


module [<RequireQualifiedAccess>] Decode =
  let settings = decodeSettings
  let cropFilters = decodeCropFilters
  let ui = decodeUI

  let state =
    let u = Unchecked.defaultof<State>
    Decode.object (fun get -> {
      Settings = get.Required.Field (nameof u.Settings) settings
      UI = get.Required.Field (nameof u.UI) ui
    } )

  let savedSettings = Decode.list (Decode.tuple2 Decode.string settings)



let defaultSavedSettings = lazy (
  Shorthand.Decode.shorthandSettings
  |> Decode.map Shorthand.toSettings
  |> Decode.keyValuePairs
  |> load settingsData
)

#if DEBUG
// validate: at least one saved settings in default

// validate for all settings:
// skills
// multipliers
// date
// cropamount
#endif

let defaultApp = lazy {
  Data = gameData
  State = {
    Settings = snd defaultSavedSettings.Value[0]
    UI = UIState.initial
  }
  SavedSettings = defaultSavedSettings.Value
}

module LocalStorage =
  let [<Literal>] VersionKey = "Version"
  let [<Literal>] StateKey = "State"
  let [<Literal>] SavedSettingsKey = "SavedSettings"

  let inline private getItem key = Browser.WebStorage.localStorage.getItem key |> unbox<string option>
  let inline private setItem key data = Browser.WebStorage.localStorage.setItem (key, data)

  // update version on load, in case other tabs are open
  let updateVersion () = setItem VersionKey App.version.String

  let private trySave name key encoder value =
    try
      value
      |> encoder
      |> Encode.toString 0
      |> setItem key
    with e ->
      // setItem can throw if localstorage is full
      console.error $"Failed to save {name}: {e}"

  let saveState state = trySave "app state" StateKey Encode.state state
  let saveSettings settings = trySave "settings" SavedSettingsKey Encode.savedSettings settings

  let private tryLoad name key decoder =
    match getItem key with
    | None ->
      console.error $"Failed to load {name} from local storage: key not found."
      None
    | Some json ->
      match json |> Decode.fromString decoder with
      | Ok app -> Some app
      | Error e ->
        console.error $"Failed to load {name} from local storage: {e}"
        None

  let loadApp () =
    match getItem VersionKey with
    | None ->
      // assume first time loading
      console.info "No version key found in local storage, loading default app..."
      let app = defaultApp.Value
      updateVersion ()
      saveState app.State
      saveSettings app.SavedSettings
      app
    | Some ver ->
      match Version.tryParse ver with
      | None ->
        console.error "Invalid version. Ignoring existing data in local storage..."
        defaultApp.Value
      | Some ver when ver.Major <> App.version.Major ->
        // convert data
        console.error "Incompatable major version. Ignoring existing data in local storage..."
        defaultApp.Value
      | Some ver ->
        let state = tryLoad "app state" StateKey Decode.state |> Option.defaultWith (fun () -> defaultApp.Value.State)
        let savedSettings = tryLoad "saved settings" SavedSettingsKey Decode.savedSettings |> Option.defaultValue []
        let state, savedSettings =
          if ver.Minor = App.version.Minor then state, savedSettings else
          let adaptSettings (settings: Settings) = { settings with Selected = Selections.adapt gameData settings.Selected }
          { state with Settings = adaptSettings state.Settings },
          savedSettings |> List.map (fun (name, settings) -> name, adaptSettings settings)

        {
          Data = gameData
          State = state
          SavedSettings = savedSettings
        }

  let inline private reload () = Browser.Dom.window.location.reload ()

  let subscribe dispatch =
    Browser.Dom.window.onstorage <- fun e ->
      match e.key with
      | key when isNullOrUndefined key ->
        // local storage was cleared (hard reset triggered)
        reload ()

      | VersionKey when isNullOrUndefined e.newValue ->
        // user manually deleted version key?
        reload ()

      | VersionKey ->
        // A new version was pushed between two tab opens (this is probably rare)?
        // Just force reload this out-of-date tab if necessary.
        match Version.tryParse e.newValue with
        | None -> reload () // version format/parsing changed, reload?
        | Some ver ->
          if ver.Major <> App.version.Major || ver.Minor <> App.version.Minor then
            // need to load updated scripts, etc.
            reload ()

      | SavedSettingsKey when isNullOrUndefined e.newValue ->
        // user manually deleted key?
        dispatch []

      | SavedSettingsKey ->
        // load new savedSettings
        match e.newValue |> Decode.fromString Decode.savedSettings with
        | Ok saved -> dispatch saved
        | Error e ->
          // failed to decode saved settings, saved settings may now race instead of syncing.
          console.error $"Failed to load saved settings from local storage: {e}"

      | StateKey -> ()

      | key -> console.warn $"Unknown key from storage event: {key}"

  let inline clear () = Browser.WebStorage.localStorage.clear ()



// Rather than depending on an xml parsing library, we use the browser's built-in (native) xml parsing.
// Stardew valley saves games are simple enough and/or we extract so little data from them
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

  let boolValue = parseWith System.Boolean.TryParse
  let natValue = parseWith System.UInt32.TryParse
  let floatValue = parseWith System.Double.TryParse

  let private array typePath parser (doc: Browser.Types.XMLDocument) (node: obj) path =
    let arr = ResizeArray ()
    let iter = doc?evaluate(path + "/" + typePath, node, null, nodeIter, null)

    let mutable finished = false
    while not finished do
      match iter?iterateNext() with
      | Some (e: Browser.Types.Element) -> parser e.textContent |> Option.iter arr.Add
      | None -> finished <- true

    resizeToArray arr

  let natArray = array "int" (fun str ->
    match System.UInt32.TryParse str with
    | true, value -> Some value
    | _ -> None)

  let stringArray = array "string" (function | "" -> None | str -> Some str)

  let [<Emit "new DOMParser()">] private domParser (): obj = jsNative
  let private xmlParser = domParser ()

  let parse (xml: string): Browser.Types.XMLDocument = xmlParser?parseFromString(xml, "text/xml")
  // if doc.tagName = "parsererror"
  // then None
  // else Some doc

open XML

let loadSaveGame (xml: string) =
  let doc = parse xml
  doc?evaluate("Farmer", doc, null, firstNode, null)?singleNodeValue |> Option.map (fun (farmer: obj) ->
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

    let specialCharm = tryGet "Special Charm" boolValue "hasSpecialCharm" CropAmountSettings.common.SpecialCharm
    let farmingLevel = tryGet "Farming Level" natValue "farmingLevel" Skill.zero.Level |> min Skill.maxLevel
    let foragingLevel = tryGet "Foraging Level" natValue "foragingLevel" Skill.zero.Level |> min Skill.maxLevel
    let day = tryGet "Day" natValue "dayOfMonthForSaveGame" Date.firstDay |> max Date.firstDay |> min Date.lastDay
    let season = tryGet "Season" natValue "seasonForSaveGame" (uint Season.Spring) |> int |> enum
    let profitMargin = tryGet "Difficulty Modifier" floatValue "difficultyModifier" Multipliers.common.ProfitMargin
    let farmerName = tryGet "Farmer Name" stringValue "name" "Imported Save"
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

    let settings = {
      GameVariables.common with
        Skills = {
          Skills.zero with
            Farming = { Skill.zero with Level = farmingLevel }
            Foraging = { Skill.zero with Level = foragingLevel }
            Professions = professions
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

    (farmerName, settings), resizeToArray missing)
