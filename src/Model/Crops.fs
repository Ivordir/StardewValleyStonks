namespace StardewValleyStonks

// Season bitset, multiple seasons
[<System.Flags>]
type Seasons =
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

[<RequireQualifiedAccess>]
module Seasons =
  let inline ofSeason (season: Season) = enum<Seasons> (1 <<< int season)

  let inline intersect (a: Seasons) (b: Seasons) = a &&& b
  let inline disjoint a b = intersect a b = Seasons.None
  let inline nonDisjoint a b = not (disjoint a b)
  let contains season seasons = nonDisjoint (ofSeason season) seasons

  let inline add season (seasons: Seasons) = seasons ||| ofSeason season
  let inline remove season (seasons: Seasons) = seasons &&& ~~~(ofSeason season)

  let ofSeq seasons = seasons |> Seq.fold (fun seasons season -> seasons |> add season) Seasons.None

  let setOrder (a: Seasons) (b: Seasons) = Enum.bitSetOrder (int a) (int b)

[<RequireQualifiedAccess>]
module Season =
  let [<Literal>] count = 4

  let name (season: Season) = Enum.name season

  let private wrapAroundNegative value =
    if value < 0
    then value + count
    else value

  let private plusMod (i: int) (season: Season) = (int season + i) % count

  let inline private plus i season = plusMod i season |> enum<Season>

  // truncated division/mod necessitates handling of negative remainder
  let private minus i season = plusMod -i season |> wrapAroundNegative |> enum<Season>

  let next season = season |> plus 1
  let previous season = season |> minus 1

  let seasonsBetween start finish =
    let mutable seasons = Seasons.None
    let mutable season = start
    while season <> finish do
      seasons <- seasons |> Seasons.add season
      season <- next season
    seasons |> Seasons.add season

  let distance (start: Season) (finish: Season) = wrapAroundNegative (int finish - int start)

  let span start finish = Array.init (distance start finish + 1) (fun i -> start |> plus i)


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

  let daySpan start finish =
    let days = Array.create (Season.distance start.Season finish.Season + 1) daysInSeason
    days[0] <- daysInSeason - start.Day + 1u
    days[days.Length - 1] <- days[days.Length - 1] - (daysInSeason - finish.Day)
    days

  let totalDays start finish =
    nat (Season.distance start.Season finish.Season) * daysInSeason - start.Day + finish.Day + 1u


type ToolLevel =
  | Normal = 0
  | Copper = 1
  | Steel = 2
  | Gold = 3
  | Iridium = 4

[<RequireQualifiedAccess>]
module ToolLevel =
  let name (toolLevel: ToolLevel) = Enum.name toolLevel


type CropAmountSettings = {
  SpecialCharm: bool
  LuckBuff: nat
  PossibleGiantCropsPerTile: float
  ShavingToolLevel: ToolLevel option
}

[<RequireQualifiedAccess>]
module CropAmountSettings =
  let common = {
    SpecialCharm = false
    LuckBuff = 0u
    PossibleGiantCropsPerTile = 3.0
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

[<RequireQualifiedAccess>]
module CropAmount =
  let [<Literal>] minYield = 1u
  let [<Literal>] minExtraCropChance = 0.0
  let [<Literal>] maxExtraCropChance = 1.0
  let [<Literal>] minPossibleGiantCropsPerTile = 0.0
  let [<Literal>] maxPossibleGiantCropsPerTile = 9.0
  let [<Literal>] maxLuckBuff = 1499u
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

  (*
  RolledDailyLuck ~ U{-100, 100}
  That is, a discrete uniform variable whose probability mass function for x in {-100...100} is:
  f(x) = 1 / (100 - -100 + 1) = 1 / 201
  This is the daily luck rolled every day by the game.

  specialCharm = 0.025 if hasSpecialCharm otherwise 0
  doubleHarvestCutoff(rolledDailyLuck) = (rolledDailyLuck / 1000 + specialCharm) / 1200 + luckBuff / 1500 + 0.0001
  assume 0 <= luckBuff <= 1499.69375 such that 0 <= doubleHarvestCutoff <= 1
  The below random value must be less than doubleHarvestCutoff for a double harvest to happen.

  Rand ~ U_[0, 1)
  That is, a continuous uniform variable whose cummulative distribution function is:
          { 0                     for x < 0
  F(x) = { (x - 0) / (1 - 0) = x for x in [0, 1)
          { 1                     for x >= 1
  This is the random value generated every harvest to determine if a double harvest happens.

  P(DoubleHarvest)
  = P(Rand < DoubleHarvestCutoff)
  = sum_(l=-100)^100 P(RolledDailyLuck = l, Rand <= doubleHarvestCutoff(l))      // for all possible l, what is the probability that Rand <= doubleHarvestCutoff(l)?
  = sum_(l=-100)^100 P(RolledDailyLuck = l) * P(Rand <= doubleHarvestCutoff(l))  => RolledDailyLuck and Rand are independent
  = sum_(l=-100)^100 f_RolledDailyLuck(l) * F_Rand(doubleHarvestCutoff(l))       => definition of probability mass function and cummulative distribution function
  = sum_(l=-100)^100 (1 / 201) * doubleHarvestCutoff(l)                          => substitution, l is in {-100...100} and 0 <= doubleHarvestCutoff(l) <= 1
  = (1 / 201) * 201 * (doubleHarvestCutoff(100) + doubleHarvestCutoff(-100)) / 2 => doubleHarvestCutoff is linear
  = (doubleHarvestCutoff(100) + doubleHarvestCutoff(-100)) / 2
  = ((100 / 1000) + (-100 / 1000)) / 1200 / 2 + specialCharm / 1200 + luckBuff / 1500 + 0.0001
  = specialCharm / 1200 + luckBuff / 1500 + 0.0001
  *)
  let doubleHarvestProb settings =
    (if settings.SpecialCharm then 0.025 else 0.0) / 1200.0
    + float settings.LuckBuff / 1500.0
    + 0.0001

  // E(DoubleAmount)
  // = P(DoubleHarvest) * 2x + (1 - P(DoubleHarvest)) * x
  // = P(DoubleHarvest) * 2x + x - P(DoubleHarvest) * x
  // = P(DoubleHarvest) * x + x
  let expectedQuantityWithDoubling settings quantity = (doubleHarvestProb settings) * quantity + quantity

  // sum_(n=1)^inf (min(0.9, extraChance)^n)
  let expectedExtraCropQuantity extraChance = (1.0 / (1.0 - min extraChance 0.9)) - 1.0

  let expectedQuantity skills settings amount =
    let maxYieldIncrease =
      if amount.FarmLevelsPerYieldIncrease <> 0u && (amount.MinCropYield > 1u || amount.MaxCropYield > 1u)
      then skills.Farming.Level / amount.FarmLevelsPerYieldIncrease
      else 0u
    let expectedYield = float (amount.MinCropYield + amount.MaxCropYield + maxYieldIncrease) / 2.0
    let expectedQuantity = expectedYield + expectedExtraCropQuantity amount.ExtraCropChance
    if amount.CanDouble
    then expectedQuantityWithDoubling settings expectedQuantity
    else expectedQuantity

  open type Quality

  let expectedQuantityByQuality skills settings cropAmount farmingQuantities =
    let quantity = expectedQuantity skills settings cropAmount
    // Only the first harvested crop can be of higher quality
    if cropAmount.FarmingQualities
    then farmingQuantities |> Qualities.addNormal (quantity - 1.0)
    else Qualities.zero |> Qualities.addNormal quantity

  let expectedQuantityFromGiantCropShaving (shavingToolLevel: ToolLevel) =
    let damage = float (int shavingToolLevel / 2 + 1)
    let numHits = ceil (3.0 / damage)
    let shavingProb = damage / 5.0
    shavingProb * numHits

  let expectedGiantCropYield settings =
    float giantYield + (settings.ShavingToolLevel |> Option.defaultOrMap 0.0 expectedQuantityFromGiantCropShaving)

  let noGiantCropProb settings = (1.0 - baseGiantProb) ** settings.PossibleGiantCropsPerTile
  let inline giantCropProb settings = 1.0 - noGiantCropProb settings

  let expectedGiantQuantity skills settings amount =
    let nonGiantQuantity = expectedQuantity skills settings amount
    let giantQuantity = expectedGiantCropYield settings
    let noGiantProb = noGiantCropProb settings
    noGiantProb * nonGiantQuantity + (1.0 - noGiantProb) * giantQuantity

  let expectedGiantQuantityByQuality skills settings amount farmingQuantities =
    let noGiantProb = noGiantCropProb settings
    let expectedGiant = (1.0 - noGiantProb) * expectedGiantCropYield settings
    expectedQuantityByQuality skills settings amount farmingQuantities
    |> Qualities.mult noGiantProb
    |> Qualities.addNormal expectedGiant


type SeedPrice =
  | FixedPrice of Vendor * nat
  | ScalingPrice of Vendor * nat option

[<RequireQualifiedAccess>]
module SeedPrice =
  let vendor = function
    | FixedPrice (v, _)
    | ScalingPrice (v, _) -> v


[<RequireQualifiedAccess>]
module Growth =
  (*
  Stardew Valley passes speed around as a float32.
  This is later converted to a float (float64), introducing a small amount of error.
  This small error is sometimes enough to give an extra day of reduction,
  since the value is later passed into the ceiling function.

  Case 1: float 0.2f > 0.2
    0.200000002980232     0.2
    * 10.0 growth days    * 10.0 growth days
    = 2.00000002980232    = 2.0
    |> ceil |> int        |> ceil |> int
    = 3                   = 2
  This effect can be seen on crops with a total of 10 growth days (e.g green bean, coffee)
  A 0.1 speed reduces the total time to 8 days (instead of 9),
  and a 0.2 speed reduces the total time to 7 days (instead of 8).

  Case 2: float 0.25f = 0.25 (all numbers 1/(2^n))
    0.25            0.25
    ...    Same    ...

  Case 3: float 0.35f < 0.35
    0.349999994039536     0.35
    * 20.0 growth days    * 20.0 growth days
    = 6.99999988079071    = 7.0
    |> ceil |> int        |> ceil |> int
    = 7                   = 7
  These wouldn't be equal if floor was used instead of ceil.
  *)
  let inline private fround (x: float) =
    #if FABLE_COMPILER
    Fable.Core.JS.Math.fround x
    #else
    x |> float32 |> float
    #endif

  let stagesAndTime speedBonus stages =
    let total = Array.sum stages
    let stages = Array.copy stages
    let mutable daysToReduce = (fround speedBonus * float total) |> ceil |> nat
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

  let maxHarvests regrowTime growthTime days =
    let days = days - 1u
    match regrowTime with
    | Some _ when growthTime > days -> 0u
    | Some time -> 1u + (days - growthTime) / time
    | None -> days / growthTime

  let daysNeededFor regrowTime growthTime harvests =
    match regrowTime with
    | Some time -> growthTime + time * (harvests - 1u)
    | None -> growthTime * harvests


type FarmCrop = {
  Seed: SeedId
  Item: ItemId
  Amount: CropAmount
  ExtraItem: (ItemId * float) option
  Giant: bool

  Seasons: Seasons
  Stages: nat array
  RegrowTime: nat option
  Paddy: bool
}

[<RequireQualifiedAccess>]
module FarmCrop =
  let [<Literal>] minRegrowTime = 1u
  let [<Literal>] minExtraItemQuantity = 0.0

  let regrowTime crop = crop.RegrowTime
  let seed crop = crop.Seed
  let seedItem crop = crop |> seed |> toItem
  let item crop = crop.Item
  let amount crop = crop.Amount
  let extraItem crop = crop.ExtraItem

  let seasons crop = crop.Seasons
  let growsInSeason season crop = crop.Seasons |> Seasons.contains season
  let growsInSeasons seasons crop = crop.Seasons |> Seasons.nonDisjoint seasons

  let xpPerItem item crop =
    let price = item crop.Item |> Item.sellPrice |> float
    (16.0 * log (0.018 * price + 1.0)) |> round |> nat

  let xpItemsPerHarvest giantCropProb crop =
    if crop.Giant then 1.0 - giantCropProb else 1.0

  let xpPerHarvest item giantCropProb crop =
    float (xpPerItem item crop) * xpItemsPerHarvest giantCropProb crop

  let items crop =
    match crop.ExtraItem with
    | Some (item, _) -> [| crop.Item; item |]
    | None -> [| crop.Item |]

  let inline name item crop = crop.Item |> item |> Item.name


type ForageCrop = {
  Seed: SeedId
  Foragables: ItemId array
  SeedRecipeUnlockLevel: nat
  Season: Season
  Stages: nat array
}

[<RequireQualifiedAccess>]
module ForageCrop =
  let [<Literal>] forageSeedsPerCraft = 10u
  let [<Literal>] xpPerItem = 7u
  let [<Literal>] minItems = 1u
  let [<Literal>] maxItems = forageSeedsPerCraft

  let seed crop = crop.Seed
  let seedItem crop = crop |> seed |> toItem
  let foragables crop = crop.Foragables
  let seedRecipeUnlockLevel crop = crop.SeedRecipeUnlockLevel

  let seedRecipeUnlocked skills crop = skills.Foraging.Level >= crop.SeedRecipeUnlockLevel

  let seasons crop = Seasons.ofSeason crop.Season
  let growsInSeason season crop = crop.Season = season
  let growsInSeasons seasons crop = seasons |> Seasons.contains crop.Season

  let xpItemsPerHarvest skills =
    if skills |> Skills.professionActive Gatherer
    then Multiplier.gatherer
    else 1.0

  let xpPerHarvest skills = float xpPerItem * xpItemsPerHarvest skills

  let name item crop = item (toItem crop.Seed) |> Item.name


type Crop =
  | FarmCrop of FarmCrop
  | ForageCrop of ForageCrop

[<RequireQualifiedAccess>]
module Crop =
  let name item = function
    | FarmCrop crop -> FarmCrop.name item crop
    | ForageCrop crop -> ForageCrop.name item crop

  let seasons = function
    | FarmCrop crop -> FarmCrop.seasons crop
    | ForageCrop crop -> ForageCrop.seasons crop

  let growsInSeason season = function
    | FarmCrop crop -> FarmCrop.growsInSeason season crop
    | ForageCrop crop -> ForageCrop.growsInSeason season crop

  let growsInSeasons seasons = function
    | FarmCrop crop -> FarmCrop.growsInSeasons seasons crop
    | ForageCrop crop -> ForageCrop.growsInSeasons seasons crop

  let stages = function
    | FarmCrop crop -> crop.Stages
    | ForageCrop crop -> crop.Stages

  let growthTime crop = crop |> stages |> Array.sum

  let paddy = function
    | FarmCrop crop -> crop.Paddy
    | ForageCrop _ -> false

  let giant = function
    | FarmCrop crop -> crop.Giant
    | _ -> false

  let mainItem = function
    | FarmCrop crop -> crop.Item
    | ForageCrop crop -> crop.Foragables[0]

  let items = function
    | FarmCrop crop -> FarmCrop.items crop
    | ForageCrop crop -> ForageCrop.foragables crop

  let seed = function
    | FarmCrop crop -> FarmCrop.seed crop
    | ForageCrop crop -> ForageCrop.seed crop

  let seedItem = function
    | FarmCrop crop -> FarmCrop.seedItem crop
    | ForageCrop crop -> ForageCrop.seedItem crop

  let growthTimeWith speed crop = Growth.time speed (stages crop)

  let growthStagesAndTime speed crop = Growth.stagesAndTime speed (stages crop)

  let regrowTime = function
    | FarmCrop crop -> crop.RegrowTime
    | ForageCrop _ -> None

  let regrows = regrowTime >> Option.isSome

  let xpPerItem item = function
    | FarmCrop crop -> FarmCrop.xpPerItem item crop
    | ForageCrop _ -> ForageCrop.xpPerItem

  let xpItemsPerHarvest giantCropProb skills = function
    | FarmCrop crop -> FarmCrop.xpItemsPerHarvest giantCropProb crop
    | ForageCrop _ -> ForageCrop.xpItemsPerHarvest skills

  let xpPerHarvest item giantCropProb skills = function
    | FarmCrop crop -> FarmCrop.xpPerHarvest item giantCropProb crop
    | ForageCrop _ -> ForageCrop.xpPerHarvest skills

  let isFarm = function
    | FarmCrop _ -> true
    | ForageCrop _ -> false

  let isForage = function
    | FarmCrop _ -> false
    | ForageCrop _ -> true

  let canGetOwnSeedsFromSeedMaker item crop = crop |> mainItem |> item |> Processor.seedMakerAccepts

  let makesOwnSeeds crop = crop |> items |> Array.contains (seedItem crop)

  let chooseSeeds predicate crops = crops |> Seq.choose (fun crop ->
    if predicate crop then Some (seed crop) else None)
