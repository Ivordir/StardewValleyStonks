namespace StardewValleyStonks.WebApp

open StardewValleyStonks
open Fable.Core

[<AutoOpen>]
module internal Util =
  let console = JS.console

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



type TimeNormalization =
  | [<CompiledName ("Total")>] TotalPeriod
  | [<CompiledName ("Per Day")>] PerDay
  | [<CompiledName ("Per Season")>] PerSeason





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




type Selections = {
  Crops: SeedId Set
  SeedPrices: Map<SeedId, Vendor Set>

  NoFertilizer: bool
  Fertilizers: FertilizerName Set
  FertilizerPrices: Map<FertilizerName, Vendor Set>

  SellRaw: (SeedId * ItemId) Set
  Products: Map<SeedId * ItemId, Processor Set>
  SellForageSeeds: SeedId Set

  UseHarvestedSeeds: SeedId Set
  UseSeedMaker: SeedId Set
  UseForageSeeds: SeedId Set

  CustomSeedPrices: Selection<SeedId, nat>
  CustomFertilizerPrices: Selection<FertilizerName, nat>
  CustomSellPrices: Selection<SeedId * ItemId, nat * bool>
}

module Selections =
  let private ensureEntries keys (oldMap: Map<_,_>) =
    keys |> Map.ofKeys (oldMap.TryFind >> Option.defaultValue Set.empty)

  let adapt (data: GameData) (selected: Selections) = {
    selected with
      SeedPrices = selected.SeedPrices |> ensureEntries data.Crops.Keys
      FertilizerPrices =selected.FertilizerPrices |> ensureEntries data.Fertilizers.Keys
      Products = selected.Products |> ensureEntries (GameData.seedItemPairs data)
  }


type SeedStrategy =
  | [<CompiledName ("Buy First Seed")>] BuyFirstSeed
  | [<CompiledName ("Stockpile Seeds")>] StockpileSeeds
  | [<CompiledName ("Ignore Seeds")>] IgnoreSeeds

type ProfitSettings = {
  SeedStrategy: SeedStrategy
  PayForFertilizer: bool
  ReplaceLostFertilizer: bool
}

type Settings = {
  Game: GameVariables
  Selected: Selections
  Profit: ProfitSettings
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


type RankItem =
  | [<CompiledName ("All Pairs")>] RankCropsAndFertilizers
  | [<CompiledName ("Crops")>] RankCrops
  | [<CompiledName ("Fertilizers")>] RankFertilizers


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


type AppMode =
  | Ranker
  | Solver

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

module Ranker =
  let initial = {
    RankItem = RankCrops
    RankMetric = Gold
    TimeNormalization = PerSeason
    BrushSpan = 0u, 24u
    SelectedCropAndFertilizer = None
  }

type UIState = {
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

module UIState =
  let initial = {
    Mode = Ranker
    SettingsTab = Skills
    OpenDetails = Set.ofArray [|
      OpenDetails.Crops
      OpenDetails.Fertilizers
      OpenDetails.RankerProfitBreakdown
      OpenDetails.RankerGrowthCalendar
    |]
    FertilizerSort = [ 4, true; 3, true ]
    FertilizerPriceSort = [ 0, true ]
    CropSort = [ 1, true; 5, true ]
    ProductSort = [ 0, true ]
    SeedSort = [ 0, true ]
    ProductQuality = Quality.Normal
    ShowNormalizedProductPrices = false
    CropFilters = CropFilters.empty
    Ranker = Ranker.initial
  }

type State = {
  Settings: Settings
  UI: UIState
}

type App = {
  Data: GameData // refetched on every tab load, do not save
  SavedSettings: (string * Settings) list // save separately to coordinate between multiple tabs
  State: State // race, last edited tab gets its state saved
}

type Version = {
  String: string
  Major: nat
  Minor: nat
  Patch: nat
}

module Version =
  let private (|Nat|_|) (str: string) =
    match System.UInt32.TryParse str with
    | true, value -> Some (Nat value)
    | _ -> None

  let tryParse (str: string) =
    match str.Split '.' with
    | [| Nat major; Nat minor; Nat patch |] ->
      Some {
        String = str
        Major = major
        Minor = minor
        Patch = patch
      }
    | _ -> None

  let inline parse str = tryParse str |> Option.get


module App =
  // Major = new schema -> convert data
  // Minor = potentially new crops, fertilizers, or items on crops -> adapt settings
  // Patch = compatible data -> no action needed
  let version = Version.parse "0.0.1"
