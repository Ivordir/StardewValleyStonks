namespace StardewValleyStonks.WebApp

open StardewValleyStonks
open Fable.Core

type [<Erase>] TableSort = SortByColumns of (int * bool) list

type SettingsTab =
  | Skills
  | Crops
  | Fertilizers
  | Misc
  | [<CompiledName ("Load / Save")>] LoadSettings

module SettingsTab = let all = unitUnionCases<SettingsTab>


type RankItem =
  | [<CompiledName ("All Pairs")>] RankCropsAndFertilizers
  | [<CompiledName ("Crops")>] RankCrops
  | [<CompiledName ("Fertilizers")>] RankFertilizers

module RankItem = let all = unitUnionCases<RankItem>


type RankMetric =
  | Gold
  | ROI
  | XP

module RankMetric =
  let unit = function
    | Gold -> "g"
    | ROI -> "%"
    | XP -> "xp"

  let fullName = function
    | Gold -> "Gold"
    | ROI -> "Return on Investment"
    | XP -> "Experience Points"

  let all = unitUnionCases<RankMetric>


type AppMode =
  | Ranker
  | Solver
module AppMode = let all = unitUnionCases<AppMode>


type [<RequireQualifiedAccess>] OpenDetails =
  | Fertilizers
  | FertilizerPrices
  | Crops
  | Products
  | SeedSources
  | Mod
  | RankerGrowthCalendar
  | RankerProfitBreakdown
  | SolverGrowthCalendar
  | SolverProfitBreakdown

type CropFilters = {
  InSeason: bool
  Seasons: Seasons
  Regrows: bool option
  Giant: bool option
  Forage: bool option
  // name
}

module CropFilters =
  let empty = {
    InSeason = true
    Seasons = Seasons.All
    Regrows = None
    Giant = None
    Forage = None
  }


type Ranker = {
  RankItem: RankItem
  RankMetric: RankMetric
  TimeNormalization: TimeNormalization
  BrushSpan: nat * nat
  SelectedCropAndFertilizer: (SeedId option * FertilizerName option option) option
}

type App = {
  DefaultModel: Model

  Model: Model
  SavedModels: (string * Model) list

  AppMode: AppMode
  SettingsTab: SettingsTab
  OpenDetails: OpenDetails Set

  FertilizerSort: TableSort
  FertilizerPriceSort: TableSort
  CropSort: TableSort
  ProductSort: TableSort
  SeedSort: TableSort
  ProductQuality: Quality
  ShowNormalizedProductPrices: bool

  CropFilters: CropFilters
  Ranker: Ranker
}

type SelectionMessage<'a when 'a: comparison> =
  | SetSelected of 'a * bool
  | SetManySelected of 'a Set * bool

type SkillMessage =
  | SetLevel of nat
  | SetBuff of nat

type FarmingMessage =
  | SetFarmingSkill of SkillMessage
  | SetTiller of bool
  | SetArtisan of bool
  | SetAgriculturist of bool

type ForagingMessage =
  | SetForagingSkill of SkillMessage
  | SetGatherer of bool
  | SetBotanist of bool

type CustomMessage<'key, 'price when 'key: comparison> =
  | AddCustom of 'key
  | SetCustom of 'key * 'price
  | RemoveCustom of 'key
  | SelectCustom of 'key SelectionMessage

type SkillsMessage =
  | SetFarming of FarmingMessage
  | SetForaging of ForagingMessage
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
  | SetForagedFruitTillerOverrides of ItemId * bool

type ModDataMessage =
  | SetQualityProducts of bool
  | SetQualityProcessors of Processor * bool

type ModelMessage =
  | SelectCrops of SeedId SelectionMessage
  | SelectSeedPrices of Vendor * SeedId SelectionMessage

  | SelectFertilizers of FertilizerName SelectionMessage
  | SetAllowNoFertilizer of bool
  | SelectFertilizerPrices of Vendor * FertilizerName SelectionMessage

  | SetSkills of SkillsMessage
  | SetMultipliers of MultipliersMessage
  | SetCropAmount of CropAmountMessage
  | SetModData of ModDataMessage
  | SetIrrigated of bool
  | SetJojaMembership of bool

  | SelectSellRawItems of (SeedId * ItemId) SelectionMessage
  | SelectProducts of Processor * (SeedId * ItemId) SelectionMessage
  | SelectSellForageSeeds of SeedId SelectionMessage

  | SelectUseRawSeeds of SeedId SelectionMessage
  | SelectUseSeedMaker of SeedId SelectionMessage
  | SelectUseForageSeeds of SeedId SelectionMessage

  | SetCustomFertilizerPrice of CustomMessage<FertilizerName, nat>
  | SetCustomSeedPrice of CustomMessage<SeedId, nat>
  | SetCustomSellPrice of CustomMessage<SeedId * ItemId, nat * bool>

  | SetStartDate of Date
  | SetEndDate of Date
  | SetLocation of Location

  | SetSeedStrategy of SeedStrategy
  | SetPayForFertilizer of bool
  | SetReplaceLostFertilizer of bool


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

type AppMessage =
  | SetModel of ModelMessage
  | LoadSavedModel of int
  | SaveCurrentModel of string
  | RenameSavedModel of int * string
  | DeleteSavedModel of int
  | ResetModel

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


module Update =
  let inline private setSelected makeSelected set =
    if makeSelected
    then Set.add set
    else Set.remove set

  let tableSort multi ((col, asc) as s) (SortByColumns sort) =
    match sort |> List.tryFindIndexBack (fst >> (=) col), multi with
    | Some i, true -> sort |> List.updateAt i (col, not asc)
    | Some _, false -> [ col, not asc ]
    | None, true -> s :: sort
    | None, false -> [ s ]
    |> SortByColumns

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

  let skill msg skill =
    match msg with
    | SetLevel l -> { skill with Level = l }
    | SetBuff b -> { skill with Buff = b }

  let farming ignoreConflicts msg (farming: Farming) =
    match msg with
    | SetFarmingSkill msg -> farming |> skill msg
    | SetTiller tiller ->
      if tiller || ignoreConflicts then
        { farming with Professions = {| farming.Professions with Tiller = tiller |} }
      else
        { farming with
            Professions = {|
              Tiller = false
              Artisan = false
              Agriculturist = false
            |}
        }
    | SetArtisan artisan ->
      if not artisan || ignoreConflicts then
        { farming with Professions = {| farming.Professions with Artisan = artisan |} }
      else
        { farming with
            Professions = {|
              Tiller = true
              Artisan = true
              Agriculturist = false
            |}
        }
    | SetAgriculturist agri ->
      if not agri || ignoreConflicts then
        { farming with Professions = {| farming.Professions with Agriculturist = agri |} }
      else
        { farming with
            Professions = {|
              Tiller = true
              Artisan = false
              Agriculturist = true
            |}
        }

  let foraging ignoreConflicts msg (foraging: Foraging) =
    match msg with
    | SetForagingSkill msg -> foraging |> skill msg
    | SetGatherer gatherer ->
      if gatherer || ignoreConflicts then
        { foraging with Professions = {| foraging.Professions with Gatherer = gatherer |} }
      else
        { foraging with
            Professions = {|
              Gatherer = false
              Botanist = false
            |}
        }
    | SetBotanist botanist ->
      if not botanist || ignoreConflicts then
        { foraging with Professions = {| foraging.Professions with Botanist = botanist |} }
      else
        { foraging with
            Professions = {|
              Gatherer = true
              Botanist = true
            |}
        }

  let skills msg skills =
    match msg with
    | SetFarming msg -> { skills with Farming = farming skills.IgnoreProfessionConflicts msg skills.Farming }
    | SetForaging msg -> { skills with Foraging = foraging skills.IgnoreProfessionConflicts msg skills.Foraging }
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
    | SetForagedFruitTillerOverrides (item, selected) ->
      { multipliers with ForagedFruitTillerOverrides = multipliers.ForagedFruitTillerOverrides |> setSelected selected item }

  let modData msg modData =
    match msg with
    | SetQualityProducts value -> { modData with QualityProducts = value }
    | SetQualityProcessors (processor, selected) -> { modData with QualityProcessors = modData.QualityProcessors |> setSelected selected processor }

  let selection defaultValue msg selection =
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

  let model msg (model: Model) =
    match msg with
    | SelectCrops msg -> { model with SelectedCrops = model.SelectedCrops |> select msg }
    | SelectFertilizers msg -> { model with SelectedFertilizers = model.SelectedFertilizers |> select msg }
    | SetAllowNoFertilizer value -> { model with AllowNoFertilizer = value }

    | SetSkills msg -> { model with Skills = skills msg model.Skills }
    | SetMultipliers msg -> { model with Multipliers = multipliers msg model.Multipliers }
    | SetCropAmount msg -> { model with CropAmount = cropAmount msg model.CropAmount }

    | SelectFertilizerPrices (vendor, msg) -> { model with SelectedFertilizerPrices = model.SelectedFertilizerPrices |> mapSelect model.Data.FertilizerPrices vendor msg }
    | SelectSeedPrices (vendor, msg) -> { model with SelectedSeedPrices = model.SelectedSeedPrices |> mapSelect model.Data.SeedPrices vendor msg }
    | SelectProducts (processor, msg) -> { model with SelectedProducts = model.SelectedProducts |> mapSelectWith (fun (_, item) processor -> model.Data.Products[item].ContainsKey processor) processor msg }

    | SelectSellRawItems msg -> { model with SellRawItems = model.SellRawItems |> select msg }

    | SelectUseRawSeeds msg -> { model with UseRawSeeds = model.UseRawSeeds |> select msg }
    | SelectUseSeedMaker msg -> { model with UseSeedMaker = model.UseSeedMaker |> select msg }

    | SetCustomFertilizerPrice msg -> { model with CustomFertilizerPrices = model.CustomFertilizerPrices |> selection 0u msg }
    | SetCustomSeedPrice msg -> { model with CustomSeedPrices = model.CustomSeedPrices |> selection 0u msg }
    | SetCustomSellPrice msg -> { model with CustomSellPrices = model.CustomSellPrices |> selection (0u, false) msg }

    | SelectSellForageSeeds msg -> { model with SellForageSeeds = model.SellForageSeeds |> select msg }
    | SelectUseForageSeeds msg -> { model with UseForageSeeds = model.UseForageSeeds |> select msg }

    | SetIrrigated value -> { model with Irrigated = value }
    | SetJojaMembership value -> { model with JojaMembership = value }

    | SetStartDate date -> { model with StartDate = date }
    | SetEndDate date -> { model with EndDate = date }
    | SetLocation location -> { model with Location = location }

    | SetSeedStrategy value -> { model with SeedStrategy = value }
    | SetPayForFertilizer value -> { model with PayForFertilizer = value }
    | SetReplaceLostFertilizer value -> { model with ReplaceLostFertilizer = value }

    | SetModData msg -> { model with ModData = modData msg model.ModData }


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

  let app msg app =
    match msg with
    | SetModel msg -> { app with Model = app.Model |> model msg }, []
    | LoadSavedModel i -> { app with Model = snd app.SavedModels[i] }, []
    | SaveCurrentModel name -> { app with SavedModels = (name, app.Model) :: app.SavedModels }, []
    | RenameSavedModel (i, name) -> { app with SavedModels = app.SavedModels |> List.updateAt i (name, app.SavedModels |> List.item i |> snd) }, []
    | DeleteSavedModel i -> { app with SavedModels = app.SavedModels |> List.removeAt i }, []
    | ResetModel -> { app with Model = app.DefaultModel }, []

    | SetAppMode mode -> { app with AppMode = mode }, []
    | SetSettingsTab tab -> { app with SettingsTab = tab }, []
    | SetDetailsOpen (details, selected) -> { app with OpenDetails = app.OpenDetails |> setSelected selected details }, []

    | SetFertilizerSort (multi, s) -> { app with FertilizerSort = tableSort multi s app.FertilizerSort }, []
    | SetFertilizerPriceSort (multi, s) -> { app with FertilizerPriceSort = tableSort multi s app.FertilizerPriceSort }, []
    | SetCropSort (multi, s) -> { app with CropSort = tableSort multi s app.CropSort }, []
    | SetProductSort (multi, s) -> { app with ProductSort = tableSort multi s app.ProductSort }, []
    | SetSeedSort (multi, s) -> { app with SeedSort = tableSort multi s app.SeedSort }, []
    | SetProductQuality q -> { app with ProductQuality = q }, []
    | SetShowNormalizedProductPrices b -> { app with ShowNormalizedProductPrices = b }, []

    | SetCropFilters msg -> { app with CropFilters = cropFilters msg app.CropFilters }, []
    | SetRanker msg -> { app with Ranker = ranker msg app.Ranker }, []
