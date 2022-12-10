namespace StardewValleyStonks

open Fable.Core

// Season bitset, multiple seasons
type [<System.Flags>] Seasons =
  | None   = 0b0000
  | Spring = 0b0001
  | Summer = 0b0010
  | Fall   = 0b0100
  | Winter = 0b1000
  | All    = 0b1111

type Season =
  | Spring = 0
  | Summer = 1
  | Fall = 2
  | Winter = 3

module Seasons =
  let [<Literal>] count = 4

  let inline ofSeason (season: Season) = enum<Seasons> (1 <<< int season)

  let inline intersect (a: Seasons) (b: Seasons) = a &&& b
  let inline overlap a b = intersect a b <> Seasons.None
  let inline contains season seasons = overlap (ofSeason season) seasons

  let inline add season (seasons: Seasons) = seasons ||| ofSeason season
  let inline remove season (seasons: Seasons) = seasons &&& ~~~(ofSeason season)

  let ofSeq seasons = seasons |> Seq.fold (flip add) Seasons.None

  let tryExactlyOne = function
    | Seasons.Spring -> Some Season.Spring
    | Seasons.Summer -> Some Season.Summer
    | Seasons.Fall -> Some Season.Fall
    | Seasons.Winter -> Some Season.Winter
    | _ -> None

  let rec setOrder (a: Seasons) (b: Seasons) =
    if a = Seasons.None || b = Seasons.None then
      compare a b
    else
      let c = compare (int b &&& 1) (int a &&& 1)
      if c = 0
      then setOrder (a >>> 1) (b >>> 1)
      else c

module Season =
  let name (season: Season) = enumName season

  let inline private ofInt (i: int) = i |> enum<Season>

  let next = function
    | Season.Winter -> Season.Spring
    | season -> ofInt (int season + 1)

  let previous = function
    | Season.Spring -> Season.Winter
    | season -> ofInt (int season - 1)

  let ofSeasons season =
    match Seasons.tryExactlyOne season with
    | Some season -> season
    | None -> invalidArg (nameof season) $"The given season: '{season}' was not a single season."

  let seasonsBetween start finish =
    let mutable seasons = Seasons.None
    let mutable season = start
    while season <> finish do
      seasons <- seasons |> Seasons.add season
      season <- next season
    seasons |> Seasons.add season

  let distance (start: Season) (finish: Season) =
    let dist = int finish - int start
    if dist < 0
    then Seasons.count + dist
    else dist
    |> nat

  let span start finish =
    let dist = int (distance start finish)
    let seasons = Array.create (dist + 1) finish
    let mutable season = start
    for i = 0 to dist - 1 do
      seasons[i] <- season
      season <- next season
    seasons

  let all = Array.init Seasons.count enum<Season>


type DateSpan = {
  StartSeason: Season
  EndSeason: Season
  TotalDays: nat
}

module DateSpan =
  let startSeason span = span.StartSeason
  let endSeason span = span.EndSeason
  let totalDays span = span.TotalDays


type Date = {
  Season: Season
  Day: nat
}

module Date =
  let [<Literal>] firstDay = 1u
  let [<Literal>] lastDay = 28u
  let [<Literal>] daysInSeason = 28u

  let inline seasonsBetween start finish = Season.seasonsBetween start.Season finish.Season
  let inline seasonSpan start finish = Season.span start.Season finish.Season

  let seasonsAndDays start finish =
    let seasons = Season.span start.Season finish.Season
    let days = Array.create seasons.Length daysInSeason
    days[0] <- daysInSeason - start.Day + firstDay
    days[days.Length - 1] <- days[days.Length - 1] - (daysInSeason - finish.Day)
    seasons, days

  let totalDays start finish =
    (Season.distance start.Season finish.Season) * daysInSeason - start.Day + finish.Day + 1u

  let spans startDate endDate seasons =
    let spans = ResizeArray()
    let nthSeason, days = seasonsAndDays startDate endDate

    let mutable i = 0
    while i < nthSeason.Length && seasons |> Seasons.contains nthSeason[i] |> not do
      i <- i + 1

    while i < nthSeason.Length do
      let mutable totalDays = days[i]
      let mutable j = i + 1
      while j < nthSeason.Length && seasons |> Seasons.contains nthSeason[j] do
        totalDays <- totalDays + days[j]
        j <- j + 1

      spans.Add {
        StartSeason = nthSeason[i]
        EndSeason = nthSeason[j - 1]
        TotalDays = totalDays
      }

      while j < nthSeason.Length && seasons |> Seasons.contains nthSeason[j] |> not do
        j <- j + 1
      i <- j

    resizeToArray spans


type ToolLevel =
  | Normal = 0
  | Copper = 1
  | Steel = 2
  | Gold = 3
  | Iridium = 4

module ToolLevel =
  let name (toolLevel: ToolLevel) = enumName toolLevel

  let all = Array.init 5 enum<ToolLevel>


type CropAmountSettings = {
  SpecialCharm: bool
  LuckBuff: nat
  GiantChecksPerTile: float
  ShavingToolLevel: ToolLevel option
}

module CropAmountSettings =
  let common = {
    SpecialCharm = false
    LuckBuff = 0u
    GiantChecksPerTile = 8.0
    ShavingToolLevel = None
  }



type CropAmount = {
  MinCropYield: nat
  MaxCropYield: nat
  ExtraCropChance: float
  CanDouble: bool
  FarmLevelsPerYieldIncrease: nat
  FarmingQualities: bool
}

module CropAmount =
  let [<Literal>] minYield = 1u
  let [<Literal>] minExtraCropChance = 0.0
  let [<Literal>] maxExtraCropChance = 1.0
  let [<Literal>] minGiantCropChecks = 0.0
  let [<Literal>] maxGiantCropChecks = 9.0
  let [<Literal>] baseGiantProb = 0.01
  let [<Literal>] giantYield = 2u

  let singleAmount = {
    MinCropYield = 1u
    MaxCropYield = 1u
    ExtraCropChance = 0.0
    CanDouble = true
    FarmLevelsPerYieldIncrease = 0u
    FarmingQualities = true
  }

  let doubleHarvestProb settings =
    // Daily luck follows a uniform distribution [-0.1, 0.1], or [-0.075, 0.125] with special charm
    // P(DoubleHarvest) = DailyLuck / 1200.0 + LuckBuff / 1500.0
    let upperBound = (0.1 + if settings.SpecialCharm then 0.025 else 0.0) / 1200.0 + float settings.LuckBuff / 1500.0

    // the average probability, ignoring all negative 'probabilities'
    let lowerBound = max 0.0 (upperBound - 0.2)
    let avg = (upperBound + lowerBound) / 2.0

    // multiply by the probability of getting a positive value
    avg * (upperBound - lowerBound) / 0.2

  let applyDoubling settings amount =
    // P(DbouleHarvest) * 2x + (1 - P(DoubleHarvest)) * x
    // = P(DbouleHarvest) * 2x + x - P(DoubleHarvest) * x
    // = P(DbouleHarvest) * x + x
    (doubleHarvestProb settings) * amount + amount

  let averageExtraCrops extraChance =
    // sum n=1..inf (extraChance^n)
    (1.0 / (1.0 - min extraChance 0.9)) - 1.0

  let averageCropAmount skills settings amount =
    let maxYieldIncrease =
      if amount.FarmLevelsPerYieldIncrease <> 0u && (amount.MinCropYield > 1u || amount.MaxCropYield > 1u)
      then skills.Farming.Level / amount.FarmLevelsPerYieldIncrease
      else 0u
    let avgYield = float (amount.MinCropYield + amount.MaxCropYield + maxYieldIncrease) / 2.0
    let avgAmount = avgYield + averageExtraCrops amount.ExtraCropChance
    if amount.CanDouble
    then applyDoubling settings avgAmount
    else avgAmount

  open type Quality

  let farmingAmounts skills settings amount farmingQualities =
    let a = averageCropAmount skills settings amount
    if amount.FarmingQualities then
      // Only the first harvested crop can be of higher quality
      if a = 1.0
      then farmingQualities
      else farmingQualities |> Qualities.updateQuality Normal (a - 1.0 + farmingQualities[Normal])
    else
      Qualities.zero |> Qualities.updateQuality Normal a

  let giantCropsFromShaving (shavingToolLevel: ToolLevel) =
    let damage = (int shavingToolLevel) / 2 + 1 |> float
    let numHits = 3.0 / damage |> ceil
    let shavingProb = damage / 5.0
    shavingProb * numHits

  let inline giantCropYield settings =
    float giantYield + (settings.ShavingToolLevel |> Option.defaultOrMap 0.0 giantCropsFromShaving)

  let noGiantCropProb settings =
    (1.0 - baseGiantProb) ** settings.GiantChecksPerTile

  let inline giantCropProb settings = 1.0 - noGiantCropProb settings

  let farmingGiantAmounts skills settings amount farmingQualities =
    let noGiantProb = noGiantCropProb settings
    let expectedGiant = (1.0 - noGiantProb) * giantCropYield settings
    let giantAmounts = farmingAmounts skills settings amount farmingQualities |> Qualities.mult noGiantProb
    giantAmounts |> Qualities.updateQuality Normal (expectedGiant + giantAmounts[Normal])


type SeedPrice =
  | FixedPrice of Vendor * nat
  | ScalingPrice of Vendor * nat option

module SeedPrice =
  let vendor = function
    | FixedPrice (v, _)
    | ScalingPrice (v, _) -> v

module Growth =
  // Stardew Valley passes speed around as a float32.
  // Compared to a float64, this has less precision,
  // leading to small differences in the representation of numbers.
  // This small precision error is sometimes enough to give an extra day of reduction,
  // since the value is later passed into the ceiling function.
  //       Case 1: 0.2f < 0.2
  //   float32               float
  //   0.200000002980232     0.2
  //   * 10.0 growth days    * 10.0 growth days
  //   = 2.00000002980232    = 2.0
  //   |> ceil |> int        |> ceil |> int
  //   = 3                   = 2 (not the same!)
  //   This effect can be seen on crops with a total of 10 growth days (e.g green bean, coffee)
  //   A 0.1 speed reduces the total time to 8 days (instead of 9),
  //   and a 0.2 speed reduces the total time to 7 days (instead of 8).
  //
  //      Case 2: 0.25f = 0.25 (all numbers 1/(2^n))
  //  float32         float
  //  0.25            0.25
  //  ...    Same    ...
  //
  //      Case3: 0.35f > 0.35
  //  float32               float
  //  0.349999994039536     0.35
  //  * 20.0 growth days    * 20.0 growth days
  //  = 6.99999988079071    = 7.0
  //  |> ceil |> int        |> ceil |> int
  //  = 7                   = 7 (wouldn't be equal if floor was used instead of ceil)
  let stagesAndTime speedBonus stages =
    let total = Array.sum' stages
    let stages = Array.copy stages
    let mutable daysToReduce = (JS.Math.fround speedBonus) * (float total) |> ceil |> nat
    let mutable daysReduced = 0u
    let mutable traverses = 0u

    while daysReduced < daysToReduce && traverses < 3u do
      // Handle the first stage
      if stages[0] > 1u then
        stages[0] <- stages[0] - 1u
        daysReduced <- daysReduced + 1u

      // Handle the other stages
      let mutable stage = 1
      while daysReduced < daysToReduce && stage < stages.Length do
        if stages[stage] > 0u then
          stages[stage] <- stages[stage] - 1u
          daysReduced <- daysReduced + 1u
        else
          // A reduction day was wasted reducing a stage below 0 days.
          // Potentially possible?, as the game code does not prevent this from happening on stages except the first.
          daysToReduce <- daysToReduce - 1u
        stage <- stage + 1
      traverses <- traverses + 1u

    stages, total - daysReduced

  let inline time crop speed = stagesAndTime crop speed |> snd

  let harvestsWith regrowTime growthTime days =
    let days = days - 1u
    match regrowTime with
    | Some time ->
      if growthTime > days
      then 0u
      else 1u + (days - growthTime) / time
    | None -> days / growthTime

  let daysUsedWith regrowTime growthTime harvests =
    match regrowTime with
    | Some time -> growthTime + (harvests - 1u) * time + 1u
    | None -> harvests * growthTime + 1u


type FarmCrop = {
  Seasons: Seasons
  Stages: nat array
  RegrowTime: nat option
  Paddy: bool
  Giant: bool
  Seed: SeedId
  Item: ItemId
  Amount: CropAmount
  ExtraItem: (ItemId * float) option
}

module FarmCrop =
  let regrowTime crop = crop.RegrowTime
  let seed crop = crop.Seed
  let seedItem crop: ItemId = seed crop * 1u<_>
  let item crop = crop.Item
  let amount crop = crop.Amount
  let extraItem crop = crop.ExtraItem

  let seasons crop = crop.Seasons
  let growsInSeason season crop = crop.Seasons |> Seasons.contains season
  let growsInSeasons seasons crop = crop.Seasons |> Seasons.overlap seasons

  let xpPerItem item crop =
    let price = item crop.Item |> Item.sellPrice
    (16.0 * log(0.018 * float price + 1.0)) |> round |> nat

  let xpPerHarvest item giantCropProb crop =
    (if crop.Giant then 1.0 - giantCropProb else 1.0) * float (xpPerItem item crop)

  let items crop =
    match crop.ExtraItem with
    | Some (item, _) -> [| crop.Item; item |]
    | None -> [| crop.Item |]

  let inline name item crop = crop.Item |> item |> Item.name


type ForageCrop = {
  Season: Season
  Stages: nat array
  Seed: SeedId
  Items: ItemId array
  SeedRecipeUnlockLevel: nat
}

module ForageCrop =
  let [<Literal>] forageSeedsPerCraft = 10u
  let [<Literal>] xpPerItem = 7u

  let seed crop = crop.Seed
  let seedItem crop: ItemId = seed crop * 1u<_>
  let items crop = crop.Items
  let seedRecipeUnlockLevel crop = crop.SeedRecipeUnlockLevel

  let seedsRecipeUnlocked skills crop = skills.Foraging.Level >= crop.SeedRecipeUnlockLevel

  let seasons crop = Seasons.ofSeason crop.Season
  let growsInSeason season crop = crop.Season = season
  let growsInSeasons seasons crop = seasons |> Seasons.contains crop.Season

  let xpPerHarvest skills = float xpPerItem * if skills |> Skills.professionActive Gatherer then Multiplier.gatherer else 1.0

  let name crop = (Season.name crop.Season) + " Forage"


type Crop =
  | FarmCrop of FarmCrop
  | ForageCrop of ForageCrop

module Crop =
  let name item = function
    | FarmCrop a -> FarmCrop.name item a
    | ForageCrop o -> ForageCrop.name o

  let seasons = function
    | FarmCrop c -> FarmCrop.seasons c
    | ForageCrop c -> ForageCrop.seasons c

  let growsInSeason season = function
    | FarmCrop c -> FarmCrop.growsInSeason season c
    | ForageCrop c -> ForageCrop.growsInSeason season c

  let growsInSeasons seasons = function
    | FarmCrop c -> FarmCrop.growsInSeasons seasons c
    | ForageCrop c -> ForageCrop.growsInSeasons seasons c

  let stages = function
    | FarmCrop c -> c.Stages
    | ForageCrop c -> c.Stages

  let totalDays crop = stages crop |> Array.sum'

  let paddy = function
    | FarmCrop c -> c.Paddy
    | ForageCrop _ -> false

  let giant = function
    | FarmCrop c -> c.Giant
    | _ -> false

  let mainItem = function
    | FarmCrop c -> c.Item
    | ForageCrop f -> f.Items[0]

  let items = function
    | FarmCrop c -> FarmCrop.items c
    | ForageCrop c -> ForageCrop.items c

  let seed = function
    | FarmCrop c -> FarmCrop.seed c
    | ForageCrop c -> ForageCrop.seed c

  let seedItem = function
    | FarmCrop c -> FarmCrop.seedItem c
    | ForageCrop c -> ForageCrop.seedItem c

  let growthTime speed crop = Growth.time speed (stages crop)

  let stagesAndTime speed crop = Growth.stagesAndTime speed (stages crop)

  let regrowTime = function
    | FarmCrop c -> c.RegrowTime
    | ForageCrop _ -> None

  let regrows = regrowTime >> Option.isSome

  let xpPerItem item = function
    | FarmCrop c -> FarmCrop.xpPerItem item c
    | ForageCrop _ -> ForageCrop.xpPerItem

  let xpPerHarvest item giantCropProb skills = function
    | FarmCrop c -> FarmCrop.xpPerHarvest item giantCropProb c
    | ForageCrop _ -> ForageCrop.xpPerHarvest skills

  let isFarm = function
    | FarmCrop _ -> true
    | ForageCrop _ -> false

  let isForage = function
    | FarmCrop _ -> false
    | ForageCrop _ -> true

  let canGetOwnSeedsFromSeedMaker items crop =
    // assume that if mainItem = seed, then mainItem.Category = Seeds
    match mainItem crop |> items |> Item.category with
    | Fruit | Vegetable | Flower -> true
    | _ -> isForage crop

  let makesOwnSeeds crop =
    let seed = seed crop
    items crop |> Array.exists (fun item -> int item = int seed)
