module StardewValleyStonks.WebApp.Update

open StardewValleyStonks

type SkillMessage =
  | SetLevel of nat
  | SetBuff of nat

type SkillsMessage =
  | SetFarming of SkillMessage
  | SetForaging of SkillMessage
  | SetProfession of Profession * bool
  | SetIgnoreSkillLevelRequirements of bool
  | SetIgnoreProfessionConflicts of bool

type CropAmountMessage =
  | SetSpecialCharm of bool
  | SetLuckBuff of nat
  | SetPossibleGiantCropsPerTile of float
  | SetShavingToolLevel of ToolLevel option

type MultipliersMessage =
  | SetProfitMargin of float
  | SetBearsKnowledge of bool
  | SetTillerForForagedFruit of bool

type ModDataMessage =
  | SetQualityProducts of bool
  | SetQualityProcessors of Processor * bool

type GameVariablesMessage =
  | SetSkills of SkillsMessage
  | SetMultipliers of MultipliersMessage
  | SetModData of ModDataMessage
  | SetCropAmount of CropAmountMessage
  | SetJojaMembership of bool
  | SetIrrigated of bool
  | SetStartDate of Date
  | SetEndDate of Date
  | SetLocation of Location

type SelectionMessage<'a when 'a: comparison> =
  | SetSelected of 'a * bool
  | SetManySelected of 'a Set * bool

type CustomMessage<'key, 'price when 'key: comparison> =
  | AddCustom of 'key * 'price
  | EditCustom of 'key * 'price
  | RemoveCustom of 'key
  | SelectCustom of 'key SelectionMessage

type SelectionsMessage =
  | SelectCrops of SeedId SelectionMessage
  | SelectSeedPrices of Vendor * SeedId SelectionMessage

  | SelectNoFertilizer of bool
  | SelectFertilizers of FertilizerName SelectionMessage
  | SelectFertilizerPrices of Vendor * FertilizerName SelectionMessage

  | SelectSellRaw of ItemId SelectionMessage
  | SelectProducts of Processor * ItemId SelectionMessage

  | SelectUseHarvestedSeeds of SeedId SelectionMessage
  | SelectUseSeedMaker of SeedId SelectionMessage
  | SelectUseForageSeeds of SeedId SelectionMessage

  | SetCustomFertilizerPrice of CustomMessage<FertilizerName, nat>
  | SetCustomSeedPrice of CustomMessage<SeedId, nat>
  | SetCustomSellPrice of CustomMessage<ItemId, nat * bool>

type ProfitMessage =
  | SetSeedStrategy of SeedStrategy
  | SetPayForFertilizer of bool
  | SetPayForDestroyedFertilizer of bool

type SettingsMessage =
  | SetGameVariables of GameVariablesMessage
  | SetProfit of ProfitMessage
  | SetSelections of SelectionsMessage

type CropFiltersMessage =
  | SetNameSearch of string
  | SetInSeason of bool
  | SetSeasons of Seasons
  | SetRegrows of bool option
  | SetGiant of bool option
  | SetForage of bool option
  | ClearFilters

type CropTabMessage =
  | SetCropFilters of CropFiltersMessage
  | SetCropSort of TableSort
  | SetProductSort of TableSort
  | SetSeedSort of TableSort
  | SetProductQuality of Quality
  | SetNormalizeProductPrices of bool

type RankerMessage =
  | SetRankItem of RankItem
  | SetRankMetric of RankMetric
  | SetTimeNormalization of TimeNormalization
  | SetShowInvalid of bool
  | SetBrushSpan of nat * nat
  | SetSelectedCropAndFertilizer of (SeedId option * FertilizerName option option) option

type UIMessage =
  | SetAppMode of AppMode
  | SetRanker of RankerMessage
  | SetOptimizationObjective of OptimizationObjective
  | SetSettingsTab of SettingsTab
  | SetDetailsOpen of OpenDetails * bool
  | SetCropTabState of CropTabMessage
  | SetFertilizerSort of TableSort
  | SetFertilizerPriceSort of TableSort

type PresetsMessage =
  | LoadSaveGame of Preset
  | SavePreset of string * Settings
  | RenamePreset of int * string
  | DeletePreset of int

type StateMessage =
  | SetSettings of SettingsMessage
  | SetUI of UIMessage
  | LoadSettings of Settings

type AppMessage =
  | SetState of StateMessage
  | SetPresets of PresetsMessage
  | SyncPresets of Preset list
  | NuclearReset

let private setSelected makeSelected value set =
  if makeSelected
  then set |> Set.add value
  else set |> Set.remove value

let skill msg skill =
  match msg with
  | SetLevel level -> { skill with Level = level }
  | SetBuff buff -> { skill with Buff = buff }

let skills msg skills =
  match msg with
  | SetFarming msg -> { skills with Farming = skills.Farming |> skill msg }
  | SetForaging msg -> { skills with Foraging = skills.Foraging |> skill msg }
  | SetProfession (profession, selected) ->
    let professions =
      match profession, selected with
      | _ when skills.IgnoreProfessionConflicts -> skills.Professions
      | Tiller, false -> skills.Professions |> Set.remove Artisan |> Set.remove Agriculturist
      | Artisan, true -> skills.Professions |> Set.add Tiller |> Set.remove Agriculturist
      | Agriculturist, true -> skills.Professions |> Set.add Tiller |> Set.remove Artisan
      | Gatherer, false -> skills.Professions |> Set.remove Botanist
      | Botanist, true -> skills.Professions |> Set.add Gatherer
      | _ -> skills.Professions

    { skills with Professions = professions |> setSelected selected profession }

  | SetIgnoreSkillLevelRequirements value -> { skills with IgnoreSkillLevelRequirements = value }
  | SetIgnoreProfessionConflicts value -> { skills with IgnoreProfessionConflicts = value }

let cropAmount msg amount =
  match msg with
  | SetSpecialCharm value -> { amount with SpecialCharm = value }
  | SetLuckBuff value -> { amount with LuckBuff = value }
  | SetPossibleGiantCropsPerTile value -> { amount with PossibleGiantCropsPerTile = value }
  | SetShavingToolLevel value -> { amount with ShavingToolLevel = value }

let multipliers msg multipliers =
  match msg with
  | SetProfitMargin value -> { multipliers with ProfitMargin = value }
  | SetBearsKnowledge value -> { multipliers with BearsKnowledge = value }
  | SetTillerForForagedFruit value -> { multipliers with TillerForForagedFruit = value }

let modData msg modData =
  match msg with
  | SetQualityProducts value -> { modData with QualityProducts = value }
  | SetQualityProcessors (processor, selected) ->
    { modData with QualityProcessors = modData.QualityProcessors |> setSelected selected processor }

let gameVariables msg vars =
  match msg with
  | SetSkills msg -> { vars with Skills = skills msg vars.Skills }
  | SetMultipliers msg -> { vars with Multipliers = multipliers msg vars.Multipliers }
  | SetModData msg -> { vars with ModData = modData msg vars.ModData }
  | SetCropAmount msg -> { vars with CropAmount = cropAmount msg vars.CropAmount }
  | SetJojaMembership value -> { vars with JojaMembership = value }
  | SetIrrigated value -> { vars with Irrigated = value }
  | SetStartDate date -> { vars with StartDate = date }
  | SetEndDate date -> { vars with EndDate = date }
  | SetLocation location -> { vars with Location = location }

let private select msg set =
  match msg with
  | SetSelected (id', selected) -> set |> setSelected selected id'
  | SetManySelected (keys, selected) ->
    if selected
    then set + keys
    else set - keys

let private mapSelectWith containsKey key msg map =
  match msg with
  | SetSelected (id', selected) -> map |> Map.change id' (Option.map (setSelected selected key))
  | SetManySelected (keys, selected) ->
    map |> Map.map (fun id' selection ->
      if containsKey id' key && keys.Contains id'
      then selection |> setSelected selected key
      else selection)

let private mapSelect (data: Table<_, Table<_,_>>) key msg map =
  mapSelectWith (fun id' key -> data[id'].ContainsKey key) key msg map

let custom msg selection =
  match msg with
  | AddCustom (key, value) ->
    { selection with
        Values = selection.Values |> Map.add key value
        Selected = selection.Selected |> Set.add key
    }
  | RemoveCustom key ->
    { selection with
        Selected = selection.Selected |> Set.remove key
        Values = selection.Values |> Map.remove key
    }
  | EditCustom (key, value) -> { selection with Values = selection.Values |> Map.add key value }
  | SelectCustom msg -> { selection with Selected = selection.Selected |> select msg }

let selections data msg (selection: Selections) =
  match msg with
  | SelectCrops msg -> { selection with Crops = selection.Crops |> select msg }
  | SelectSeedPrices (vendor, msg) ->
    { selection with SeedPrices = selection.SeedPrices |> mapSelect data.SeedPrices vendor msg }

  | SelectNoFertilizer selected -> { selection with NoFertilizer = selected }
  | SelectFertilizers msg -> { selection with Fertilizers = selection.Fertilizers |> select msg }
  | SelectFertilizerPrices (vendor, msg) ->
    { selection with FertilizerPrices = selection.FertilizerPrices |> mapSelect data.FertilizerPrices vendor msg }

  | SelectSellRaw msg -> { selection with SellRaw = selection.SellRaw |> select msg }
  | SelectProducts (processor, msg) ->
    { selection with
        Products = selection.Products |> mapSelectWith (fun item processor ->
          GameData.product data processor item |> Option.isSome) processor msg
    }

  | SelectUseHarvestedSeeds msg -> { selection with UseHarvestedSeeds = selection.UseHarvestedSeeds |> select msg }
  | SelectUseSeedMaker msg -> { selection with UseSeedMaker = selection.UseSeedMaker |> select msg }
  | SelectUseForageSeeds msg -> { selection with UseForageSeeds = selection.UseForageSeeds |> select msg }

  | SetCustomFertilizerPrice msg ->
    { selection with CustomFertilizerPrices = selection.CustomFertilizerPrices |> custom msg }
  | SetCustomSeedPrice msg -> { selection with CustomSeedPrices = selection.CustomSeedPrices |> custom msg }
  | SetCustomSellPrice msg -> { selection with CustomSellPrices = selection.CustomSellPrices |> custom msg }

let profit msg profit =
  match msg with
  | SetSeedStrategy strategy -> { profit with SeedStrategy = strategy }
  | SetPayForFertilizer value -> { profit with PayForFertilizer = value }
  | SetPayForDestroyedFertilizer value -> { profit with PayForDestroyedFertilizer = value }

let settings data msg (settings: Settings) =
  match msg with
  | SetGameVariables msg -> { settings with Game = settings.Game |> gameVariables msg }
  | SetProfit msg -> { settings with Profit = settings.Profit |> profit msg }
  | SetSelections msg -> { settings with Selected = settings.Selected |> selections data msg }

let cropFilters msg filters =
  match msg with
  | SetNameSearch value -> { filters with NameSearch = value }
  | SetInSeason value -> { filters with InSeason = value }
  | SetSeasons value -> { filters with Seasons = value }
  | SetRegrows value -> { filters with Regrows = value }
  | SetGiant value -> { filters with Giant = value }
  | SetForage value -> { filters with Forage = value }
  | ClearFilters -> CropFilters.empty

let cropTab msg state =
  match msg with
  | SetCropFilters msg -> { state with Filters = cropFilters msg state.Filters }
  | SetCropSort (col, asc) -> { state with CropSort = col, asc }
  | SetProductSort (col, asc) -> { state with ProductSort = col, asc }
  | SetSeedSort (col, asc) -> { state with SeedSort = col, asc }
  | SetProductQuality quality -> { state with ProductQuality = quality }
  | SetNormalizeProductPrices value -> { state with NormalizeProductPrices = value }

let ranker msg ranker =
  match msg with
  | SetRankItem item -> { ranker with RankItem = item }
  | SetRankMetric metric -> { ranker with RankMetric = metric }
  | SetTimeNormalization norm -> { ranker with TimeNormalization = norm }
  | SetShowInvalid value -> { ranker with ShowInvalid = value }
  | SetBrushSpan (start, finish) -> { ranker with BrushSpan = start, finish }
  | SetSelectedCropAndFertilizer pair -> { ranker with SelectedCropAndFertilizer = pair }

let presets msg presets =
  match msg with
  | LoadSaveGame preset ->
    preset.UniqueId
    |> Option.bind (fun uniqueId -> presets |> List.tryFindIndex (Preset.hasId uniqueId))
    |> Option.defaultOrMap (preset :: presets) (fun i -> presets |> List.updateAt i preset)

  | SavePreset (name, settings) ->
    {
      Name = name
      UniqueId = None
      Settings = settings
    } :: presets

  | RenamePreset (i, name) ->
    presets |> List.updateAt i { (presets |> List.item i) with Name = name }

  | DeletePreset i -> presets |> List.removeAt i

let ui msg ui =
  match msg with
  | SetAppMode mode -> { ui with Mode = mode }
  | SetRanker msg -> { ui with Ranker = ranker msg ui.Ranker }
  | SetOptimizationObjective mode -> { ui with OptimizationObjective = mode }
  | SetSettingsTab tab -> { ui with SettingsTab = tab }
  | SetDetailsOpen (details, selected) -> { ui with OpenDetails = ui.OpenDetails |> setSelected selected details }
  | SetCropTabState msg -> { ui with CropTab = cropTab msg ui.CropTab }
  | SetFertilizerSort (col, asc) -> { ui with FertilizerSort = col, asc }
  | SetFertilizerPriceSort (col, asc) -> { ui with FertilizerPriceSort = col, asc }

let state msg data (oldSettings, oldUI) =
  match msg with
  | SetSettings msg -> oldSettings |> settings data msg, oldUI
  | SetUI msg -> oldSettings, oldUI |> ui msg
  | LoadSettings settings -> settings, oldUI
