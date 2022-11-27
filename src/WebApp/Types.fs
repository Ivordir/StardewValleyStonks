namespace StardewValleyStonks.WebApp

open StardewValleyStonks
open Fable.Core

[<AutoOpen>]
module internal Util =
  let inline item' (seed: SeedId): ItemId = seed * 1u<_>
  let inline seed' (item: ItemId): SeedId = item * 1u<_>

  let inline refEqual a b = System.Object.ReferenceEquals (a, b)

  let minBy projection a b = if projection a <= projection b then a else b
  let maxBy projection a b = if projection a >= projection b then a else b

  let compareBy projection a b = compare (projection a) (projection b)
  let compareByRev projection a b = compare (projection b) (projection a)

  let inline unitUnionCases<'a> =
    typeof<'a>
    |> Reflection.FSharpType.GetUnionCases
    |> Array.map (fun x -> Reflection.FSharpValue.MakeUnion (x, Array.empty) |> unbox<'a>)

  let sortWithLast last x y =
    match x = last, y = last with
    | true, true -> 0
    | true, false -> 1
    | false, true -> -1
    | false, false -> compare x y

  let sortWithLastBy last projection x y = sortWithLast last (projection x) (projection y)

  let sortWithLastRev last x y =
    match x = last, y = last with
    | true, true -> 0
    | true, false -> 1
    | false, true -> -1
    | false, false -> compare y x

  let sortWithLastByRev last projection x y = sortWithLastRev last (projection x) (projection y)

  let sortByMany comparers seq =
    let comparers = Array.ofSeq comparers
    seq
    |> Seq.sortWith (fun x y ->
      let mutable comparison = 0
      let mutable i = 0
      while comparison = 0 && i < comparers.Length do
        comparison <- comparers[i] x y
        i <- i + 1
      comparison)
    |> Array.ofSeq
    // comparers
    // |> Array.tryPick (fun comparer ->
    //   match comparer x y with
    //   | 0 -> None
    //   | x -> Some x)
    // |> Option.defaultValue 0)
  // sorted


type Location =
  | Farm
  | Greenhouse
  | [<CompiledName ("Ginger Island")>] GingerIsland

module Location = let all = unitUnionCases<Location>


type TimeNormalization =
  | [<CompiledName ("Total")>] TotalPeriod
  | [<CompiledName ("Per Day")>] PerDay
  | [<CompiledName ("Per Season")>] PerSeason

module TimeNormalization = let all = unitUnionCases<TimeNormalization>


type SeedStrategy =
  | [<CompiledName ("Buy First Seed")>] BuyFirstSeed
  | [<CompiledName ("Stockpile Seeds")>] StockpileSeeds
  | [<CompiledName ("Ignore Seeds")>] IgnoreSeeds

module SeedStrategy = let all = unitUnionCases<SeedStrategy>


type Selection<'a, 'b when 'a: comparison> = {
  Values: Map<'a, 'b>
  Selected: 'a Set
}

module Selection =
  let empty = {
    Values = Map.empty
    Selected = Set.empty
  }

  let selectedValue key selection =
    if selection.Selected.Contains key
    then Some selection.Values[key]
    else None

  let allSelected selection =
    selection.Values.Keys |> Seq.forall selection.Selected.Contains


type Settings = {
  SelectedCrops: SeedId Set
  SelectedSeedPrices: Map<SeedId, Vendor Set>

  AllowNoFertilizer: bool
  SelectedFertilizers: FertilizerName Set
  SelectedFertilizerPrices: Map<FertilizerName, Vendor Set>

  SellRawItems: (SeedId * ItemId) Set
  SelectedProducts: Map<SeedId * ItemId, Processor Set>
  SellForageSeeds: SeedId Set

  UseRawSeeds: SeedId Set
  UseSeedMaker: SeedId Set
  UseForageSeeds: SeedId Set

  CustomSeedPrices: Selection<SeedId, nat>
  CustomFertilizerPrices: Selection<FertilizerName, nat>
  CustomSellPrices: Selection<SeedId * ItemId, nat * bool>

  Skills: Skills
  Multipliers: Multipliers
  CropAmount: CropAmountSettings
  ModData: ModData
  JojaMembership: bool
  Irrigated: bool

  StartDate: Date
  EndDate: Date
  Location: Location

  SeedStrategy: SeedStrategy
  PayForFertilizer: bool
  ReplaceLostFertilizer: bool
}

module Settings =
  let private ensureEntries keys (oldMap: Map<_,_>) =
    keys |> Map.ofKeys (oldMap.TryFind >> Option.defaultValue Set.empty)

  let adapt data settings = {
    settings with
      SelectedSeedPrices = settings.SelectedSeedPrices |> ensureEntries data.Crops.Keys
      SelectedFertilizerPrices =settings.SelectedFertilizerPrices |> ensureEntries data.Fertilizers.Keys
      SelectedProducts = settings.SelectedProducts |> ensureEntries (GameData.seedItemPairs data)
  }


type CustomChoice<'a, 'b> =
  | NonCustom of 'a
  | Custom of 'b

type GrowthSpan = {
  Span: DateSpan
  Stages: nat array
  GrowthTime: nat
  Harvests: nat
}


type TableSort = (int * bool) list

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
  Data: GameData
  Settings: Settings
  SavedSettings: (string * Settings) list

  Mode: AppMode
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
