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
  | SetGiantChecksPerTile of float
  | SetShavingToolLevel of nat option
  | SetSpecialCharm of bool
  | SetLuckBuff of nat

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
  | SetCropAmount of CropAmountMessage
  | SetModData of ModDataMessage
  | SetIrrigated of bool
  | SetJojaMembership of bool
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

  | SelectFertilizers of FertilizerName SelectionMessage
  | SetAllowNoFertilizer of bool
  | SelectFertilizerPrices of Vendor * FertilizerName SelectionMessage

  | SelectSellRawItems of (SeedId * ItemId) SelectionMessage
  | SelectProducts of Processor * (SeedId * ItemId) SelectionMessage
  | SelectSellForageSeeds of SeedId SelectionMessage

  | SelectUseRawSeeds of SeedId SelectionMessage
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
  | SetSelections of SelectionsMessage
  | SetProfit of ProfitMessage

type SortMessage = bool * (int * bool)

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
  | SetBrushSpan of nat * nat
  | SetSelectedCropAndFertilizer of (SeedId option * FertilizerName option option) option

type UIMessage =
  | SetAppMode of AppMode
  | SetSettingsTab of SettingsTab
  | SetDetailsOpen of OpenDetails * bool

  | SetFertilizerSort of SortMessage
  | SetFertilizerPriceSort of SortMessage
  | SetCropSort of SortMessage
  | SetProductSort of SortMessage
  | SetSeedSort of SortMessage
  | SetProductQuality of Quality
  | SetShowNormalizedProductPrices of bool

  | SetCropFilters of CropFiltersMessage
  | SetRanker of RankerMessage

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

let inline private setSelected makeSelected set =
  if makeSelected
  then Set.add set
  else Set.remove set

let skill msg skill =
  match msg with
  | SetLevel l -> { skill with Level = l }
  | SetBuff b -> { skill with Buff = b }

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
  | SetGiantChecksPerTile value -> { amount with GiantChecksPerTile = value }
  | SetShavingToolLevel value -> { amount with ShavingToolLevel = value }
  | SetSpecialCharm value -> { amount with SpecialCharm = value }
  | SetLuckBuff value -> { amount with LuckBuff = value }

let multipliers msg multipliers =
  match msg with
  | SetProfitMargin value -> { multipliers with ProfitMargin = value }
  | SetBearsKnowledge value -> { multipliers with BearsKnowledge = value }
  | SetTillerForForagedFruit value -> { multipliers with TillerForForagedFruit = value }

let modData msg modData =
  match msg with
  | SetQualityProducts value -> { modData with QualityProducts = value }
  | SetQualityProcessors (processor, selected) -> { modData with QualityProcessors = modData.QualityProcessors |> setSelected selected processor }

let gameVariables msg vars =
  match msg with
  | SetSkills msg -> { vars with Skills = skills msg vars.Skills }
  | SetMultipliers msg -> { vars with Multipliers = multipliers msg vars.Multipliers }
  | SetCropAmount msg -> { vars with CropAmount = cropAmount msg vars.CropAmount }
  | SetModData msg -> { vars with ModData = modData msg vars.ModData }
  | SetIrrigated value -> { vars with Irrigated = value }
  | SetJojaMembership value -> { vars with JojaMembership = value }
  | SetStartDate date -> { vars with StartDate = date }
  | SetEndDate date -> { vars with EndDate = date }
  | SetLocation location -> { vars with Location = location }

let select msg set =
  match msg with
  | SetSelected (id', selected) -> set |> setSelected selected id'
  | SetManySelected (keys, selected) ->
    if selected
    then set + keys
    else set - keys

let mapSelectWith containsKey key msg map =
  match msg with
  | SetSelected (id', selected) -> map |> Map.change id' (Option.map <| setSelected selected key)
  | SetManySelected (keys, selected) ->
    map |> Map.map (fun id' selection ->
      if keys.Contains id' && containsKey id' key
      then selection |> setSelected selected key
      else selection)

let mapSelect (data: Table<_,_>) key msg map = mapSelectWith (data.Find >> flip Table.containsKey) key msg map

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
  | SelectSeedPrices (vendor, msg) -> { selection with SeedPrices = selection.SeedPrices |> mapSelect data.SeedPrices vendor msg }

  | SetAllowNoFertilizer value -> { selection with NoFertilizer = value }
  | SelectFertilizers msg -> { selection with Fertilizers = selection.Fertilizers |> select msg }
  | SelectFertilizerPrices (vendor, msg) -> { selection with FertilizerPrices = selection.FertilizerPrices |> mapSelect data.FertilizerPrices vendor msg }

  | SelectSellRawItems msg -> { selection with SellRaw = selection.SellRaw |> select msg }
  | SelectProducts (processor, msg) -> { selection with Products = selection.Products |> mapSelectWith (fun (_, item) processor -> data.Products[item].ContainsKey processor) processor msg }
  | SelectSellForageSeeds msg -> { selection with SellForageSeeds = selection.SellForageSeeds |> select msg }

  | SelectUseRawSeeds msg -> { selection with UseHarvestedSeeds = selection.UseHarvestedSeeds |> select msg }
  | SelectUseSeedMaker msg -> { selection with UseSeedMaker = selection.UseSeedMaker |> select msg }
  | SelectUseForageSeeds msg -> { selection with UseForageSeeds = selection.UseForageSeeds |> select msg }

  | SetCustomFertilizerPrice msg -> { selection with CustomFertilizerPrices = selection.CustomFertilizerPrices |> custom 0u msg }
  | SetCustomSeedPrice msg -> { selection with CustomSeedPrices = selection.CustomSeedPrices |> custom 0u msg }
  | SetCustomSellPrice msg -> { selection with CustomSellPrices = selection.CustomSellPrices |> custom (0u, false) msg }

let profit msg profit =
  match msg with
  | SetSeedStrategy value -> { profit with SeedStrategy = value }
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
  | SetSelections msg -> { settings with Selected = settings.Selected |> selections data msg }
  | SetProfit msg -> { settings with Profit = settings.Profit |> profit msg }

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
  | SetSettingsTab tab -> { ui with SettingsTab = tab }
  | SetDetailsOpen (details, selected) -> { ui with OpenDetails = ui.OpenDetails |> setSelected selected details }

  | SetFertilizerSort (multi, s) -> { ui with FertilizerSort = tableSort multi s ui.FertilizerSort }
  | SetFertilizerPriceSort (multi, s) -> { ui with FertilizerPriceSort = tableSort multi s ui.FertilizerPriceSort }
  | SetCropSort (multi, s) -> { ui with CropSort = tableSort multi s ui.CropSort }
  | SetProductSort (multi, s) -> { ui with ProductSort = tableSort multi s ui.ProductSort }
  | SetSeedSort (multi, s) -> { ui with SeedSort = tableSort multi s ui.SeedSort }
  | SetProductQuality q -> { ui with ProductQuality = q }
  | SetShowNormalizedProductPrices b -> { ui with ShowNormalizedProductPrices = b }

  | SetCropFilters msg -> { ui with CropFilters = cropFilters msg ui.CropFilters }
  | SetRanker msg -> { ui with Ranker = ranker msg ui.Ranker }

let state msg data state =
  match msg with
  | SetSettings msg -> { state with Settings = state.Settings |> settings data msg }
  | SetUI msg -> { state with UI = state.UI |> ui msg }
  | LoadSettings settings -> { state with Settings = settings }