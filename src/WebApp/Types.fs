namespace StardewValleyStonks.WebApp

open StardewValleyStonks
open Fable.Core

module [<AutoOpen>] internal Util =
  let console = JS.console

  let inline konst x _ = x
  let inline curry f a b = f (a, b)

  let inline refEqual a b = System.Object.ReferenceEquals (a, b)

  let minBy projection a b = if projection a <= projection b then a else b
  let maxBy projection a b = if projection a >= projection b then a else b

  let inline resizeToArray (resize: 'a ResizeArray) = unbox<'a array> resize

  let compareBy projection a b = compare (projection a) (projection b)

  let fertilizerOrder seq = seq |> Seq.sortBy Fertilizer.name
  let cropOrder (data: GameData) seq = seq |> Seq.sortBy (Crop.name data.Items.Find)

  let inline unitUnionCases<'a> =
    typeof<'a>
    |> Reflection.FSharpType.GetUnionCases
    |> Array.map (fun x -> Reflection.FSharpValue.MakeUnion (x, [||]) |> unbox<'a>)

  let refMemo f =
    let mutable prevInput = Unchecked.defaultof<_>
    let mutable prevOutput = Unchecked.defaultof<_>
    fun x ->
      if not (refEqual x prevInput) then
        prevInput <- x
        prevOutput <- f x
      prevOutput


module [<RequireQualifiedAccess>] Option =
  let merge merger a b =
    match a, b with
    | Some a, Some b -> Some (merger a b)
    | Some a, None -> Some a
    | None, Some b -> Some b
    | None, None -> None

  let inline min a b = merge min a b
  let inline max a b = merge max a b

  /// Compares two options, treating `None` as the max value instead of the min value.
  let noneMaxCompare a b =
    match a, b with
    | None, None -> 0
    | None, Some _ -> 1
    | Some _, None -> -1
    | Some a, Some b -> compare a b

  let noneMaxCompareBy projection a b = noneMaxCompare (projection a) (projection b)

  let ofResult = function
    | Ok x -> Some x
    | Error _ -> None


module [<RequireQualifiedAccess>] Result =
  let get = function
    | Ok x -> x
    | Error _ -> invalidArg "result" "The result value was Error."

  let isOk = function
    | Ok _ -> true
    | Error _ -> false

  let isError = function
    | Ok _ -> false
    | Error _ -> true


module [<RequireQualifiedAccess>] Array =
  let mapReduce reduction mapping (array: _ array) =
    if array.Length = 0 then invalidArg (nameof array) "The given array cannot be empty."
    let mutable current = mapping array[0]
    for x in array do
      current <- reduction current (mapping x)
    current

  let map2Reduce reduction mapping (array1: _ array) (array2: _ array) =
    let len = array1.Length
    if len <> array2.Length then invalidArg (nameof array1) "The given arrays had different lengths."
    if len = 0 then invalidArg (nameof array1) "The given arrays cannot be empty."

    let mutable current = mapping array1[0] array2[0]
    for i = 1 to len - 1 do
      current <- reduction current (mapping array1[i] array2[i])
    current


module Fertilizer =
  module Opt =
    let displayName = Option.defaultOrMap "No Fertilizer" Fertilizer.name


type CustomChoice<'a, 'b> =
  | NonCustom of 'a
  | Custom of 'b


type Selection<'a, 'b when 'a: comparison> = {
  Values: Map<'a, 'b>
  Selected: 'a Set
}

module [<RequireQualifiedAccess>] Selection =
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
  let private ensureEntries keys (map: Map<_,_>) =
    keys
    |> Seq.map (fun key -> key, map.TryFind key |> Option.defaultValue Set.empty)
    |> Map.ofSeq

  let adapt (data: GameData) (selected: Selections) = {
    selected with
      SeedPrices = selected.SeedPrices |> ensureEntries data.Crops.Keys
      FertilizerPrices = selected.FertilizerPrices |> ensureEntries data.Fertilizers.Keys
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
  Profit: ProfitSettings
  Selected: Selections
}


type AppMode =
  | Ranker
  | Solver

type RankItem =
  | [<CompiledName ("All Pairs")>] RankCropsAndFertilizers
  | [<CompiledName ("Crops")>] RankCrops
  | [<CompiledName ("Fertilizers")>] RankFertilizers

type RankMetric =
  | Gold
  | ROI
  | XP

module [<RequireQualifiedAccess>] RankMetric =
  let unit = function
    | Gold -> "g"
    | ROI -> "%"
    | XP -> "xp"

  let fullName = function
    | Gold -> "Gold"
    | ROI -> "Return on Investment"
    | XP -> "Experience Points"

type TimeNormalization =
  | [<CompiledName ("Total")>] TotalPeriod
  | [<CompiledName ("Per Day")>] PerDay
  | [<CompiledName ("Per Season")>] PerSeason

type Ranker = {
  RankItem: RankItem
  RankMetric: RankMetric
  TimeNormalization: TimeNormalization
  ShowInvalid: bool
  BrushSpan: nat * nat
  SelectedCropAndFertilizer: (SeedId option * FertilizerName option option) option
}

type SolverMode =
  | [<CompiledName ("Gold")>] MaximizeGold
  | [<CompiledName ("XP")>] MaximizeXP

type SettingsTab =
  | Skills
  | Crops
  | Fertilizers
  | Misc
  | [<CompiledName ("Load / Save")>] LoadSettings

type CropTab =
  | [<CompiledName ("Crops")>] CropsTable
  | [<CompiledName ("Products")>] ProductsTable
  | [<CompiledName ("Seeds")>] SeedsTable

type [<RequireQualifiedAccess>] OpenDetails =
  | Fertilizers
  | FertilizerPrices
  | Mod
  | RankerGrowthCalendar
  | RankerProfitBreakdown
  | SolverGrowthCalendar
  | SolverProfitBreakdown

type CropFilters = {
  NameSearch: string
  InSeason: bool
  Seasons: Seasons
  Regrows: bool option
  Giant: bool option
  Forage: bool option
}

module [<RequireQualifiedAccess>] CropFilters =
  let empty = {
    NameSearch = ""
    InSeason = true
    Seasons = Seasons.All
    Regrows = None
    Giant = None
    Forage = None
  }

type TableSort = (int * bool)

type UIState = {
  Mode: AppMode
  Ranker: Ranker
  SolverMode: SolverMode

  SettingsTab: SettingsTab
  CropTab: CropTab
  OpenDetails: OpenDetails Set
  CropFilters: CropFilters

  FertilizerSort: TableSort
  FertilizerPriceSort: TableSort
  CropSort: TableSort
  ProductSort: TableSort
  SeedSort: TableSort
  ProductQuality: Quality
  ShowNormalizedProductPrices: bool
}

type App = {
  Data: GameData // refetched on every tab load, not saved
  SavedSettings: (string * Settings) list // save separately to coordinate between multiple tabs
  State: Settings * UIState // race, last edited tab gets its state saved
}

type Version = {
  String: string
  Major: nat
  Minor: nat
  Patch: nat
}

module [<RequireQualifiedAccess>] Version =
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


module App =
  // Major = completely new schema -> convert data
  // Minor = new fields, other minor schema edits -> edit data
  // Patch = potentially new crops, fertilizers, or items on crops -> adapt settings
  let version = Version.tryParse "0.0.1" |> Option.get
