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


// Single season only
type [<Erase>] Season = Season of Seasons

module Seasons =
  let [<Literal>] count = 4

  let inline name (seasons: Seasons) = enumName seasons

  let inline intersect (a: Seasons) (b: Seasons) = a &&& b
  let inline overlap a b = intersect a b <> Seasons.None
  let inline contains (Season season) (seasons: Seasons) = overlap season seasons

  let inline add (Season season) (seasons: Seasons) = seasons ||| season
  let inline remove (Season season) (seasons: Seasons) = seasons &&& (~~~season)

  let inline ofSeason (Season season) = season
  let inline ofSeq seasons = seasons |> Seq.fold (flip add) Seasons.None

  let tryExactlyOne = function
    | Seasons.Spring
    | Seasons.Summer
    | Seasons.Fall
    | Seasons.Winter as season -> Season season |> Some
    | _ -> None

  let setOrder (a: Seasons) (b: Seasons) =
    let rec next a b =
      if a = Seasons.None || b = Seasons.None then
        compare a b
      else
        let c = compare (int b &&& 1) (int a &&& 1)
        if c = 0
        then next (a >>> 1) (b >>> 1)
        else c
    next a b

module Season =
  open type Seasons

  let Spring = Season Spring
  let Summer = Season Summer
  let Fall = Season Fall
  let Winter = Season Winter

  let name (Season season) =
    match season with
    | Seasons.Spring -> nameof Spring
    | Seasons.Summer -> nameof Summer
    | Seasons.Fall -> nameof Fall
    | Seasons.Winter -> nameof Winter
    | _ -> invalidArg (nameof season) $"The given season: '{season}' was not a single season."

  let inline private ofInt (i: int) = i |> enum |> Season

  let next = function
    | Season Seasons.Winter -> Spring
    | Season season -> ofInt (int season <<< 1)

  let previous = function
    | Season Seasons.Spring -> Winter
    | Season season -> ofInt (int season >>> 1)

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

  let span start finish =
    let seasons = ResizeArray ()
    let mutable season = start
    while season <> finish do
      seasons.Add season
      season <- next season
    seasons.Add season
    seasons.ToArray ()

  let distance start finish =
    let mutable distance = 0u
    let mutable season = start
    while season <> finish do
      distance <- distance + 1u
      season <- next season
    distance

  let all = Block.init Seasons.count (fun i -> ofInt (1 <<< i))


type DateSpan = {
  StartSeason: Season
  EndSeason: Season
  TotalDays: nat
}

module DateSpan =
  let inline startSeason span = span.StartSeason
  let inline endSeason span = span.EndSeason
  let inline totalDays span = span.TotalDays

type [<Erase>] Date = Date of season: Season * day: nat

module Date =
  let [<Literal>] firstDay = 1u
  let [<Literal>] lastDay = 28u
  let [<Literal>] daysInSeason = 28u

  let inline season (Date (season, _)) = season
  let inline day (Date (_, day)) = day

  let seasonsBetween (Date (start, _)) (Date (finish, _)) = Season.seasonsBetween start finish
  let seasonSpan (Date (start, _)) (Date (finish, _)) = Season.span start finish

  let seasonsAndDays (Date (startSeason, startDay)) (Date (endSeason, endDay)) =
    let seasons = Season.span startSeason endSeason
    let days = Array.create seasons.Length daysInSeason
    days[0] <- daysInSeason - startDay + firstDay
    days[days.Length - 1] <- days[days.Length - 1] - (daysInSeason - endDay)
    seasons, days

  let totalDays (Date (startSeason, startDay)) (Date (endSeason, endDay)) =
    (Season.distance startSeason endSeason) * daysInSeason - startDay + endDay + 1u

  let spans startDate endDate seasons =
    let spans = ResizeArray()
    let nthSeason, days = seasonsAndDays startDate endDate

    let mutable start = season startDate
    let mutable totalDays = 0u
    for i = 0 to nthSeason.Length - 1 do
      let season = nthSeason[i]
      if seasons |> Seasons.contains season then
        if totalDays = 0u then start <- season
        totalDays <- totalDays + days[i]
      elif totalDays > 0u then
        spans.Add {
          StartSeason = start
          EndSeason = nthSeason[i - 1]
          TotalDays = totalDays
        }
        totalDays <- 0u
    if totalDays > 0u then
      spans.Add {
        StartSeason = start
        EndSeason = season endDate
        TotalDays = totalDays
      }

    spans.ToArray()

  let inline dayValid day = day |> onClosedInterval firstDay lastDay

type Date with
  member inline this.Season = Date.season this
  member inline this.Day = Date.day this







type CropAmount = {
  MinCropYield: nat
  MaxCropYield: nat
  ExtraCropChance: float
  CanDouble: bool
  Giant: bool
  FarmLevelsPerYieldIncrease: nat
  FarmingQualities: bool
}

type CropAmountSettings = {
  SpecialCharm: bool
  LuckBuff: nat
  GiantChecksPerTile: float
  ShavingToolLevel: nat option
}

module CropAmount =
  let [<Literal>] minExtraCropChance = 0.0
  let [<Literal>] maxExtraCropChance = 1.0
  let [<Literal>] minGiantCropChecks = 0.0
  let [<Literal>] maxGiantCropChecks = 9.0
  let [<Literal>] maxShavingToolLevel = 4u
  let [<Literal>] baseGiantProb = 0.01
  let [<Literal>] giantYield = 2u

  let singleAmount = {
    MinCropYield = 1u
    MaxCropYield = 1u
    ExtraCropChance = 0.0
    CanDouble = true
    Giant = false
    FarmLevelsPerYieldIncrease = 0u
    FarmingQualities = true
  }

  let doubleCropProb settings =
    (float settings.LuckBuff / 1500.0) + (if settings.SpecialCharm then 0.025 / 1200.0 else 0.0)

  let applyDoubling settings extraAmount = (doubleCropProb settings) * extraAmount + extraAmount

  let cropsFromExtraChance extraChance = (1.0 / (1.0 - min extraChance 0.9)) - 1.0

  let averageCropYield skills settings amount =
    let yieldIncrease =
      if (amount.MinCropYield > 1u || amount.MaxCropYield > 1u) && amount.FarmLevelsPerYieldIncrease <> 0u
      then skills.Farming.Level / amount.FarmLevelsPerYieldIncrease
      else 0u
    let avgExtraYield = float (amount.MinCropYield + amount.MaxCropYield + yieldIncrease) / 2.0
    let avgAmount = avgExtraYield + cropsFromExtraChance amount.ExtraCropChance
    if amount.CanDouble
    then applyDoubling settings avgAmount
    else avgAmount

  open type Quality

  let farmingAmounts skills settings amount farmingQualities =
    let a = averageCropYield skills settings amount
    if amount.FarmingQualities then
      if a = 1.0
      then farmingQualities
      else farmingQualities |> Qualities.updateQuality Normal (a - 1.0 + farmingQualities[Normal])
    else
      Qualities.zero |> Qualities.updateQuality Normal a

  let giantCropsFromShaving shavingToolLevel =
    let damage = shavingToolLevel / 2u + 1u |> float
    let numHits = 3.0 / damage |> ceil
    let shavingProb = damage / 5.0
    shavingProb * numHits

  let inline giantCropYield settings =
    float giantYield + (settings.ShavingToolLevel |> Option.defaultOrMap 0.0 giantCropsFromShaving)

  let noGiantCropProb settings =
    (1.0 - baseGiantProb) ** settings.GiantChecksPerTile

  let inline giantCropProb settings = 1.0 - noGiantCropProb settings

  let applyGiantAmount settings farmingAmounts =
    let noGiantProb = noGiantCropProb settings
    let expectedGiant = (1.0 - noGiantProb) * giantCropYield settings
    let giantAmounts = farmingAmounts |> Qualities.map ((*) noGiantProb)
    giantAmounts |> Qualities.updateQuality Normal (expectedGiant + giantAmounts[Normal])

  let farmingGiantAmounts skills settings amount farmingQualities =
    let a = farmingAmounts skills settings amount farmingQualities
    if amount.Giant
    then applyGiantAmount settings a
    else a


type SeedPrice =
  | FixedPrice of Vendor * nat
  | ScalingPrice of Vendor * nat option

module SeedPrice =
  let vendor = function
    | FixedPrice (v, _)
    | ScalingPrice (v, _) -> v

  let createFixed vendor price = FixedPrice (vendor, price)
  let createScalingFrom vendor seedPrice = ScalingPrice (vendor, Some seedPrice)
  let createScaling vendor = ScalingPrice (vendor, None)


type [<Measure>] SeedNum
type SeedId = int<SeedNum>
type GrowthData = {
  Seasons: Seasons
  Stages: nat Block
  TotalTime: nat
  RegrowTime: nat option
  Paddy: bool
  Seed: SeedId
}

module Growth =
  let inline seasons growth = growth.Seasons
  let inline growsInSeason season growth = growth.Seasons |> Seasons.contains season
  let inline growsInSeasons seasons growth = growth.Seasons |> Seasons.overlap seasons
  let inline stages growth = growth.Stages
  let inline totalTime growth = growth.TotalTime
  let inline regrowTime growth = growth.RegrowTime
  let inline paddy growth = growth.Paddy
  let inline seed growth = growth.Seed
  let inline seedItem growth = growth.Seed * 1<_> : ItemId

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
  let stagesAndTime growth speedBonus =
    let stages = Block.toArray growth.Stages
    let mutable daysToReduce = (JS.Math.fround speedBonus) * (float growth.TotalTime) |> ceil |> nat
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

    stages, growth.TotalTime - daysReduced

  let inline time crop speed = stagesAndTime crop speed |> snd

  let harvestsWith growthTime days growth =
    let days = days - 1u
    match growth.RegrowTime with
    | Some time ->
      if growthTime > days
      then 0u
      else 1u + (days - growthTime) / time
    | None -> days / growthTime

  let daysUsedWith growthTime harvests growth =
    match growth.RegrowTime with
    | Some time -> growthTime + (harvests - 1u) * time + 1u
    | None -> harvests * growthTime + 1u

  let consecutiveHarvestsWith growthTime dateSpans growth =
    dateSpans |> Array.map (fun span -> harvestsWith growthTime span.TotalDays growth)

  let consecutiveHarvests dateSpans speedBonus growth = consecutiveHarvestsWith (time growth speedBonus) dateSpans growth

  let consecutiveHarvestsData dateSpans speedBonus growth =
    let stages, time = stagesAndTime growth speedBonus
    let harvests = dateSpans |> Array.map (fun span -> harvestsWith time span.TotalDays growth)
    {|
      Stages = stages
      GrowthTime = time
      Harvests = harvests
    |}


type FarmCrop = {
  Growth: GrowthData
  Item: ItemId
  Amount: CropAmount
  ExtraItem: (ItemId * float) option
}

module FarmCrop =
  let inline growth crop = crop.Growth
  let inline regrowTime crop = crop.RegrowTime
  let inline seed crop = crop.Growth.Seed
  let inline seedItem crop = crop.Growth |> Growth.seedItem
  let inline item crop = crop.Item
  let inline amount crop = crop.Amount
  let inline extraItem crop = crop.ExtraItem

  let inline growsInSeason season crop = crop.Growth |> Growth.growsInSeason season
  let inline growsInSeasons seasons crop = crop.Growth |> Growth.growsInSeasons seasons

  let xpPerHarvest item crop =
    let price = item crop.Item |> Item.sellPrice
    (16.0 * log(0.018 * float price + 1.0)) |> round |> nat

  let items crop =
    match crop.ExtraItem with
    | Some (item, _) -> Block.wrap [| crop.Item; item |]
    | None -> Block.singleton crop.Item

  let inline name item crop = crop.Item |> item |> Item.name

  let extraItemAmountValid: float -> _ = nonZero
  let regrowTimeValid: nat -> _ = nonZero


type ForageCrop = {
  Growth: GrowthData
  Items: ItemId Block
  SeedRecipeUnlockLevel: nat
}

module ForageCrop =
  let [<Literal>] forageSeedsPerCraft = 10u
  let [<Literal>] xpPerItem = 7u

  let inline growth crop = crop.Growth
  let inline seed crop = crop.Growth.Seed
  let inline seedItem crop = crop.Growth |> Growth.seedItem
  let inline items crop = crop.Items
  let inline seedRecipeUnlockLevel crop = crop.SeedRecipeUnlockLevel

  let inline seedsRecipeUnlocked skills crop = Skills.foragingLevelMet crop.SeedRecipeUnlockLevel skills

  let inline growsInSeason season = growth >> Growth.growsInSeason season
  let inline growsInSeasons seasons = growth >> Growth.growsInSeasons seasons

  let xpPerHarvest botanist = float xpPerItem * if botanist then 1.2 else 1.0

  let name crop = (Seasons.name crop.Growth.Seasons) + " Forage"


type Crop =
  | FarmCrop of FarmCrop
  | ForageCrop of ForageCrop

module Crop =
  let growth = function
    | FarmCrop a -> a.Growth
    | ForageCrop o -> o.Growth

  let name item = function
    | FarmCrop a -> FarmCrop.name item a
    | ForageCrop o ->  ForageCrop.name o

  let seasons = growth >> Growth.seasons
  let growsInSeason season = growth >> Growth.growsInSeason season
  let growsInSeasons seasons = growth >> Growth.growsInSeasons seasons

  let stages = growth >> Growth.stages
  let totalDays = growth >> Growth.totalTime
  let paddy = growth >> Growth.paddy
  let mainItem = function
    | FarmCrop c -> c.Item
    | ForageCrop f -> f.Items[0]
  let items = function
    | FarmCrop c -> FarmCrop.items c
    | ForageCrop c -> ForageCrop.items c
  let seed = growth >> Growth.seed
  let seedItem = growth >> Growth.seedItem

  let growthTime = growth >> Growth.time
  let regrowTime = growth >> Growth.regrowTime
  let regrows = regrowTime >> Option.isSome

  let xpPerHarvest botanist item = function
    | FarmCrop c -> FarmCrop.xpPerHarvest item c |> float
    | ForageCrop _ -> ForageCrop.xpPerHarvest botanist

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
    items crop |> Block.exists (fun item -> int item = int seed)
