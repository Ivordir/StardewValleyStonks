namespace StardewValleyStonks

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

module [<RequireQualifiedAccess>] Seasons =
  let [<Literal>] count = 4

  let inline ofSeason (season: Season) = enum<Seasons> (1 <<< int season)

  let inline intersect (a: Seasons) (b: Seasons) = a &&& b
  let inline overlap a b = intersect a b <> Seasons.None
  let inline contains season seasons = overlap (ofSeason season) seasons

  let inline add season (seasons: Seasons) = seasons ||| ofSeason season
  let inline remove season (seasons: Seasons) = seasons &&& ~~~(ofSeason season)

  let ofSeq seasons = seasons |> Seq.fold (fun seasons season -> seasons |> add season) Seasons.None

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

module [<RequireQualifiedAccess>] Season =
  let name (season: Season) = enumName season

  let inline private ofInt (i: int) = enum<Season> i

  let next = function
    | Season.Winter -> Season.Spring
    | season -> ofInt (int season + 1)

  let previous = function
    | Season.Spring -> Season.Winter
    | season -> ofInt (int season - 1)

  let ofSeasons seasons =
    match Seasons.tryExactlyOne seasons with
    | Some season -> season
    | None -> invalidArg (nameof seasons) $"The given seasons: '{seasons}' was not a single season."

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
    let seasons = Array.zeroCreate (dist + 1)
    let mutable season = start
    for i = 0 to dist do
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
    let seasons = seasonSpan start finish
    let days = Array.create seasons.Length daysInSeason
    days[0] <- daysInSeason - start.Day + firstDay
    days[days.Length - 1] <- days[days.Length - 1] - (daysInSeason - finish.Day)
    seasons, days

  let totalDays start finish =
    (Season.distance start.Season finish.Season) * daysInSeason - start.Day + finish.Day + 1u

  let spans startDate endDate seasons =
    let spans = ResizeArray ()
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

    spans.ToArray ()


type ToolLevel =
  | Normal = 0
  | Copper = 1
  | Steel = 2
  | Gold = 3
  | Iridium = 4

module [<RequireQualifiedAccess>] ToolLevel =
  let name (toolLevel: ToolLevel) = enumName toolLevel

  let all = Array.init 5 enum<ToolLevel>


type CropAmountSettings = {
  SpecialCharm: bool
  LuckBuff: nat
  GiantChecksPerTile: float
  ShavingToolLevel: ToolLevel option
}

module [<RequireQualifiedAccess>] CropAmountSettings =
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

  let doubleHarvestProb settings =
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

    (if settings.SpecialCharm then 0.025 else 0.0) / 1200.0
    + float settings.LuckBuff / 1500.0
    + 0.0001

  let expectedDoubleAmount settings amount =
    // E(DoubleAmount)
    // = P(DoubleHarvest) * 2x + (1 - P(DoubleHarvest)) * x
    // = P(DoubleHarvest) * 2x + x - P(DoubleHarvest) * x
    // = P(DoubleHarvest) * x + x
    (doubleHarvestProb settings) * amount + amount

  let expectedExtraCrops extraChance =
    // sum_(n=1)^inf (min(0.9, extraChance)^n)
    (1.0 / (1.0 - min extraChance 0.9)) - 1.0

  let expectedAmount skills settings amount =
    let maxYieldIncrease =
      if amount.FarmLevelsPerYieldIncrease <> 0u && (amount.MinCropYield > 1u || amount.MaxCropYield > 1u)
      then skills.Farming.Level / amount.FarmLevelsPerYieldIncrease
      else 0u
    let expectedYield = float (amount.MinCropYield + amount.MaxCropYield + maxYieldIncrease) / 2.0
    let expectedAmount = expectedYield + expectedExtraCrops amount.ExtraCropChance
    if amount.CanDouble
    then expectedDoubleAmount settings expectedAmount
    else expectedAmount

  open type Quality

  let expectedAmountByQuality skills settings cropAmount farmingQualities =
    let amount = expectedAmount skills settings cropAmount
    // Only the first harvested crop can be of higher quality
    if cropAmount.FarmingQualities
    then farmingQualities |> Qualities.updateQuality Normal (amount - 1.0 + farmingQualities[Normal])
    else Qualities.zero |> Qualities.updateQuality Normal amount

  let expectedGiantCropsFromShaving (shavingToolLevel: ToolLevel) =
    let damage = (int shavingToolLevel) / 2 + 1 |> float
    let numHits = 3.0 / damage |> ceil
    let shavingProb = damage / 5.0
    shavingProb * numHits

  let expectedGiantCropYield settings =
    float giantYield + (settings.ShavingToolLevel |> Option.defaultOrMap 0.0 expectedGiantCropsFromShaving)

  let noGiantCropProb settings =
    (1.0 - baseGiantProb) ** settings.GiantChecksPerTile

  let inline giantCropProb settings = 1.0 - noGiantCropProb settings

  let expectedGiantAmount skills settings amount =
    let nonGiantAmount = expectedAmount skills settings amount
    let giantAmount = expectedGiantCropYield settings
    let noGiantProb = noGiantCropProb settings
    noGiantProb * nonGiantAmount + (1.0 - noGiantProb) * giantAmount

  let expectedGiantAmountByQuality skills settings amount farmingQualities =
    let noGiantProb = noGiantCropProb settings
    let expectedGiant = (1.0 - noGiantProb) * expectedGiantCropYield settings
    let farmingAmounts = expectedAmountByQuality skills settings amount farmingQualities |> Qualities.mult noGiantProb
    farmingAmounts |> Qualities.updateQuality Normal (expectedGiant + farmingAmounts[Normal])


type SeedPrice =
  | FixedPrice of Vendor * nat
  | ScalingPrice of Vendor * nat option

module [<RequireQualifiedAccess>] SeedPrice =
  let vendor = function
    | FixedPrice (v, _)
    | ScalingPrice (v, _) -> v


module Growth =
  (*
  Stardew Valley passes speed around as a float32.
  This is later converted to a float (float64), introducing a small amout of error.
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
    let mutable daysToReduce = (fround speedBonus) * (float total) |> ceil |> nat
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
    | Some time -> growthTime + (harvests - 1u) * time
    | None -> harvests * growthTime


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

module [<RequireQualifiedAccess>] FarmCrop =
  let [<Literal>] minRegrowTime = 1u
  let [<Literal>] minExtraItemAmount = 0.0

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
    let price = item crop.Item |> Item.sellPrice |> float
    (16.0 * log (0.018 * price + 1.0)) |> round |> nat

  let xpItemsPerHarvest giantCropProb crop =
    if crop.Giant then 1.0 - giantCropProb else 1.0

  let xpPerHarvest item giantCropProb crop =
    xpItemsPerHarvest giantCropProb crop * float (xpPerItem item crop)

  let items crop =
    match crop.ExtraItem with
    | Some (item, _) -> [| crop.Item; item |]
    | None -> [| crop.Item |]

  let inline name item crop = crop.Item |> item |> Item.name


type ForageCrop = {
  Seed: SeedId
  Items: ItemId array
  SeedRecipeUnlockLevel: nat
  Season: Season
  Stages: nat array
}

module [<RequireQualifiedAccess>] ForageCrop =
  let [<Literal>] forageSeedsPerCraft = 10u
  let [<Literal>] xpPerItem = 7u
  let [<Literal>] minItems = 1u
  let [<Literal>] maxItems = forageSeedsPerCraft

  let seed crop = crop.Seed
  let seedItem crop: ItemId = seed crop * 1u<_>
  let items crop = crop.Items
  let seedRecipeUnlockLevel crop = crop.SeedRecipeUnlockLevel

  let seedRecipeUnlocked skills crop = skills.Foraging.Level >= crop.SeedRecipeUnlockLevel

  let seasons crop = Seasons.ofSeason crop.Season
  let growsInSeason season crop = crop.Season = season
  let growsInSeasons seasons crop = seasons |> Seasons.contains crop.Season

  let xpItemsPerHarvest skills = if skills |> Skills.professionActive Gatherer then Multiplier.gatherer else 1.0
  let xpPerHarvest skills = xpItemsPerHarvest skills * float xpPerItem

  let name crop = (Season.name crop.Season) + " Forage"


type Crop =
  | FarmCrop of FarmCrop
  | ForageCrop of ForageCrop

module [<RequireQualifiedAccess>] Crop =
  let name item = function
    | FarmCrop crop -> FarmCrop.name item crop
    | ForageCrop crop -> ForageCrop.name crop

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

  let totalDays crop = crop |> stages |> Array.sum

  let paddy = function
    | FarmCrop crop -> crop.Paddy
    | ForageCrop _ -> false

  let giant = function
    | FarmCrop crop -> crop.Giant
    | _ -> false

  let mainItem = function
    | FarmCrop crop -> crop.Item
    | ForageCrop crop -> crop.Items[0]

  let items = function
    | FarmCrop crop -> FarmCrop.items crop
    | ForageCrop crop -> ForageCrop.items crop

  let seed = function
    | FarmCrop crop -> FarmCrop.seed crop
    | ForageCrop crop -> ForageCrop.seed crop

  let seedItem = function
    | FarmCrop crop -> FarmCrop.seedItem crop
    | ForageCrop crop -> ForageCrop.seedItem crop

  let growthTime speed crop = Growth.time speed (stages crop)

  let stagesAndTime speed crop = Growth.stagesAndTime speed (stages crop)

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

  let canGetOwnSeedsFromSeedMaker crop = seedItem crop <> mainItem crop

  let makesOwnSeeds crop = crop |> items |> Array.contains (seedItem crop)
