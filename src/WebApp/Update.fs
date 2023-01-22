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
  | SetGiantChecksPerTile of float
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
  | AddCustom of 'key
  | SetCustom of 'key * 'price
  | RemoveCustom of 'key
  | SelectCustom of 'key SelectionMessage

type SelectionsMessage =
  | SelectCrops of SeedId SelectionMessage
  | SelectSeedPrices of Vendor * SeedId SelectionMessage

  | SelectNoFertilizer of bool
  | SelectFertilizers of FertilizerName SelectionMessage
  | SelectFertilizerPrices of Vendor * FertilizerName SelectionMessage

  | SelectSellRaw of (SeedId * ItemId) SelectionMessage
  | SelectProducts of Processor * (SeedId * ItemId) SelectionMessage
  | SelectSellForageSeeds of SeedId SelectionMessage

  | SelectUseHarvestedSeeds of SeedId SelectionMessage
  | SelectUseSeedMaker of SeedId SelectionMessage
  | SelectUseForageSeeds of SeedId SelectionMessage

  | SetCustomFertilizerPrice of CustomMessage<FertilizerName, nat>
  | SetCustomSeedPrice of CustomMessage<SeedId, nat>
  | SetCustomSellPrice of CustomMessage<SeedId * ItemId, nat * bool>

type ProfitMessage =
  | SetSeedStrategy of SeedStrategy
  | SetPayForFertilizer of bool
  | SetReplaceLostFertilizer of bool

type SettingsMessage =
  | SetGameVariables of GameVariablesMessage
  | SetProfit of ProfitMessage
  | SetSelections of SelectionsMessage

type CropFiltersMessage =
  | SetInSeason of bool
  | SetSeasons of Seasons
  | SetRegrows of bool option
  | SetGiant of bool option
  | SetForage of bool option
  | ClearFilters

type RankerMessage =
  | SetRankItem of RankItem
  | SetRankMetric of RankMetric
  | SetTimeNormalization of TimeNormalization
  | SetShowInvalid of bool
  | SetBrushSpan of nat * nat
  | SetSelectedCropAndFertilizer of (SeedId option * FertilizerName option option) option

type SortMessage = int * bool

type UIMessage =
  | SetAppMode of AppMode
  | SetRanker of RankerMessage
  | SetSolverMode of SolverMode
  | SetSettingsTab of SettingsTab
  | SetDetailsOpen of OpenDetails * bool
  | SetCropFilters of CropFiltersMessage
  | SetFertilizerSort of SortMessage
  | SetFertilizerPriceSort of SortMessage
  | SetCropSort of SortMessage
  | SetProductSort of SortMessage
  | SetSeedSort of SortMessage
  | SetProductQuality of Quality
  | SetShowNormalizedProductPrices of bool

type SavedSettingsMessage =
  | LoadSaveGame of string * GameVariables
  | SaveSettings of string
  | RenameSettings of int * string
  | DeleteSettings of int

type StateMessage =
  | SetSettings of SettingsMessage
  | SetUI of UIMessage
  | LoadSettings of Settings

type AppMessage =
  | SetState of StateMessage
  | SetSavedSettings of SavedSettingsMessage
  | SyncSavedSettings of (string * Settings) list
  | HardReset

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
  | SetGiantChecksPerTile value -> { amount with GiantChecksPerTile = value }
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
  | SetSelected (id', selected) -> map |> Map.change id' (Option.map <| setSelected selected key)
  | SetManySelected (keys, selected) ->
    map |> Map.map (fun id' selection ->
      if containsKey id' key && keys.Contains id'
      then selection |> setSelected selected key
      else selection)

let private mapSelect (data: Table<_, Table<_,_>>) key msg map = mapSelectWith (fun id' key -> data[id'].ContainsKey key) key msg map

let custom defaultValue msg selection =
  match msg with
  | AddCustom key ->
    { selection with Values = selection.Values |> Map.add key defaultValue }
  | RemoveCustom key ->
    { selection with
        Selected = selection.Selected |> Set.remove key
        Values = selection.Values |> Map.remove key
    }
  | SetCustom (key, value) -> { selection with Values = selection.Values |> Map.add key value }
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
    { selection with Products = selection.Products |> mapSelectWith (fun (_, item) processor -> GameData.product data item processor |> Option.isSome) processor msg }
  | SelectSellForageSeeds msg -> { selection with SellForageSeeds = selection.SellForageSeeds |> select msg }

  | SelectUseHarvestedSeeds msg -> { selection with UseHarvestedSeeds = selection.UseHarvestedSeeds |> select msg }
  | SelectUseSeedMaker msg -> { selection with UseSeedMaker = selection.UseSeedMaker |> select msg }
  | SelectUseForageSeeds msg -> { selection with UseForageSeeds = selection.UseForageSeeds |> select msg }

  | SetCustomFertilizerPrice msg -> { selection with CustomFertilizerPrices = selection.CustomFertilizerPrices |> custom 0u msg }
  | SetCustomSeedPrice msg -> { selection with CustomSeedPrices = selection.CustomSeedPrices |> custom 0u msg }
  | SetCustomSellPrice msg -> { selection with CustomSellPrices = selection.CustomSellPrices |> custom (0u, false) msg }

let profit msg profit =
  match msg with
  | SetSeedStrategy strategy -> { profit with SeedStrategy = strategy }
  | SetPayForFertilizer value -> { profit with PayForFertilizer = value }
  | SetReplaceLostFertilizer value -> { profit with ReplaceLostFertilizer = value }

let tableSort multi ((col, asc) as s) sort =
  match sort |> List.tryFindIndexBack (fst >> (=) col), multi with
  | Some i, true -> sort |> List.updateAt i (col, not asc)
  | Some _, false -> [ col, not asc ]
  | None, true -> s :: sort
  | None, false -> [ s ]

let settings data msg (settings: Settings) =
  match msg with
  | SetGameVariables msg -> { settings with Game = settings.Game |> gameVariables msg }
  | SetProfit msg -> { settings with Profit = settings.Profit |> profit msg }
  | SetSelections msg -> { settings with Selected = settings.Selected |> selections data msg }

let cropFilters msg filters =
  match msg with
  | SetInSeason value -> { filters with InSeason = value }
  | SetSeasons value -> { filters with Seasons = value }
  | SetRegrows value -> { filters with Regrows = value }
  | SetGiant value -> { filters with Giant = value }
  | SetForage value -> { filters with Forage = value }
  | ClearFilters -> CropFilters.empty

let ranker msg ranker =
  match msg with
  | SetRankItem item -> { ranker with RankItem = item }
  | SetRankMetric metric -> { ranker with RankMetric = metric }
  | SetTimeNormalization norm -> { ranker with TimeNormalization = norm }
  | SetShowInvalid value -> { ranker with ShowInvalid = value }
  | SetBrushSpan (start, finish) -> { ranker with BrushSpan = start, finish }
  | SetSelectedCropAndFertilizer pair -> { ranker with SelectedCropAndFertilizer = pair }

let savedSettings msg settings saved =
  match msg with
  | LoadSaveGame (name, vars) -> (name, { settings with Game = vars }) :: saved
  | SaveSettings name -> (name, settings) :: saved
  | RenameSettings (i, name) -> saved |> List.updateAt i (name, saved |> List.item i |> snd)
  | DeleteSettings i -> saved |> List.removeAt i

let ui msg ui =
  match msg with
  | SetAppMode mode -> { ui with Mode = mode }
  | SetRanker msg -> { ui with Ranker = ranker msg ui.Ranker }
  | SetSolverMode mode -> { ui with SolverMode = mode }
  | SetSettingsTab tab -> { ui with SettingsTab = tab }
  | SetDetailsOpen (details, selected) -> { ui with OpenDetails = ui.OpenDetails |> setSelected selected details }
  | SetCropFilters msg -> { ui with CropFilters = cropFilters msg ui.CropFilters }
  | SetFertilizerSort (col, asc) -> { ui with FertilizerSort = col, asc }
  | SetFertilizerPriceSort (col, asc) -> { ui with FertilizerPriceSort = col, asc }
  | SetCropSort (col, asc) -> { ui with CropSort = col, asc }
  | SetProductSort (col, asc) -> { ui with ProductSort = col, asc }
  | SetSeedSort (col, asc) -> { ui with SeedSort = col, asc }
  | SetProductQuality quality -> { ui with ProductQuality = quality }
  | SetShowNormalizedProductPrices value -> { ui with ShowNormalizedProductPrices = value }

let state msg data state =
  match msg with
  | SetSettings msg -> { state with Settings = state.Settings |> settings data msg }
  | SetUI msg -> { state with UI = state.UI |> ui msg }
  | LoadSettings settings -> { state with Settings = settings }
