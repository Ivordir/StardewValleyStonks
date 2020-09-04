namespace StardewValleyStonks

open Types

type HarvestCrop =
  { Crop: Item
    SellRaw: Override
    Products: Map<NameOf<Processor>, Product> }

type HarvestItem =
  { Item: HarvestCrop
    Amount: float
    Replant: Override option }

module HarvestItem =
  let item harvestItem = harvestItem.Item
  let replant harvestItem = harvestItem.Replant

type SeedMaker =
  { SellSeeds: Override
    Replant: Override }

module SeedMaker =
  let initial =
    { SellSeeds = None
      Replant = None }

type Plant =
  | SeedMakerPlant of SeedMaker
  | RawPlant of Override

type BaseCrop =
  { Name: string
    Selected: bool
    Seasons: Set<Season>
    SelectedSeasons: Set<Season>
    GrowthStages: int list
    TotalGrowthTime: int
    GrowthMultipliers: Set<Multiplier>
    Seed: Item
    PriceFrom: Map<NameOf<Source>, Buy>
    BuySeeds: Override }

type Crop =
  | RegularCrop of
      {| Base: BaseCrop
         Crop: HarvestCrop
         ExtraCrops: float
         DoubleCrops: bool
         SeedMaker: SeedMaker
         HarvestItem: HarvestItem option |}
  | RegrowCrop of
      {| Base: BaseCrop
         RegrowTime: int
         Crop: HarvestCrop
         ExtraCrops: float
         Trelis: bool
         IndoorsOnly: bool
         Replant: Plant |}
  | GiantCrop of
      {| Base: BaseCrop
         Crop: HarvestCrop
         SeedMaker: SeedMaker |}
  | ForageCrop of
      {| Base: BaseCrop
         SeedMaker: SeedMaker
         SeedMakerInput: NameOf<Item>
         Crops: Map<NameOf<Item>, HarvestCrop>
         SellForageSeeds: Override
         ForageSeedsReplant: Override |}
  | Bush of
      {| Base: BaseCrop
         RegrowTime: int
         HarvestDayStart: int
         HarvestDayEnd: int
         Crop: HarvestCrop |}

type CropSort =
  | ByName
  | Seasons
  | TotalGrowthTime
  | RegrowTime
  | SeedPrice

type CreateCropItem =
  | SellPrice of int
  | CropItem of Item

module CropItem =
  let create name = function
    | SellPrice price -> Item.createCrop name price
    | CropItem item -> item 

type CreateSeed =
  | SeedSell of int
  | Seed of Item

module Seed =
 let create name = function
  | SeedSell price -> Item.create (name + " Seeds") price
  | Seed item -> item

module HarvestCrop =
  let crop harvestCrop = harvestCrop.Crop

  let nameOfCrop = crop >> Item.nameOf

  let createWith item products =
    { Crop = item
      SellRaw = None
      Products = Product.createAll item products |> toMapByKey Product.processor }

  let create = CropItem.create >>| createWith

module Crop =
  let private baseName crop = crop.Name
  let private baseSelected crop = crop.Selected
  let private baseSeasons crop = crop.Seasons
  let private baseSelectedSeasons crop = crop.SelectedSeasons
  let private baseGrowthStages crop = crop.GrowthStages
  let private baseTotalGrowthTime crop = crop.TotalGrowthTime
  let private baseGrowthMultipliers crop = crop.GrowthMultipliers
  let private basePriceFrom crop = crop.PriceFrom
  let private baseBuySeedsOverride crop = crop.BuySeeds

  let private baseToggle (crop: BaseCrop) = { crop with Selected = not crop.Selected }
  let private baseAddGrowthMultiplier multiplier crop =
    { crop with GrowthMultipliers = crop.GrowthMultipliers.Add multiplier }

  let baseCrop = function
    | RegularCrop c -> c.Base
    | RegrowCrop r -> r.Base
    | GiantCrop g -> g.Base
    | ForageCrop f -> f.Base
    | Bush t -> t.Base

  let name = baseCrop >> baseName
  let selected = baseCrop >> baseSelected
  let seasons = baseCrop >> baseSeasons
  let selectedSeasons = baseCrop >> baseSelectedSeasons
  let growthStages = baseCrop >> baseGrowthStages
  let totalGrowthTime = baseCrop >> baseTotalGrowthTime
  let growthMultipliers = baseCrop >> baseGrowthMultipliers
  let priceFrom = baseCrop >> basePriceFrom
  let buySeedsOverride = baseCrop >> baseBuySeedsOverride

  let regrowTime = function
    | RegrowCrop r -> Some r.RegrowTime
    | Bush t -> Some t.RegrowTime
    | _ -> None

  let seedMakerData = function
    | RegularCrop c -> Some (c.Base.Seed, c.SeedMaker)
    | RegrowCrop r ->
        match r.Replant with
        | SeedMakerPlant s -> Some (r.Base.Seed, s)
        | RawPlant _ -> None
    | GiantCrop g -> Some (g.Base.Seed, g.SeedMaker)
    | ForageCrop f -> Some (f.Base.Seed, f.SeedMaker)
    | Bush _ -> None

  let withBase baseCrop = function
    | RegularCrop c ->
      RegularCrop {| c with Base = baseCrop |}
    | RegrowCrop r ->
      RegrowCrop {| r with Base = baseCrop |}
    | GiantCrop g ->
      GiantCrop {| g with Base = baseCrop |}
    | ForageCrop f ->
      ForageCrop {| f with Base = baseCrop |}
    | Bush t ->
      Bush {| t with Base = baseCrop |}

  let applyBase = flip withBase

  let modifyBase modify crop =
    crop
    |> baseCrop
    |> modify
    |> applyBase crop

  let modifyBaseWith modify modification = modifyBase (modify modification)

  let toggle = modifyBase baseToggle
  let addGrowthMultiplier = modifyBaseWith baseAddGrowthMultiplier

  let nameOf = toNameOf name
  let private img name = "img/Crops/" + name + ".png"
  let image = name >> img
  let imageOfName (name: NameOf<Crop>) = img <| ofName name

  let private toF32 = Fable.Core.JS.Constructors.Float32Array.Create 1
  // (x: float) |> float32 |> float doesn't actually do a conversion (thanks javascript...), so here we use typed arrays to achieve the same effect.
  // This is necessary, because Stardew Valley passes speed around as a float32 which then gets coverted to a float, introducing some small error (or something like that).
  // This small error is sometimes enough to give an extra day of reduction since the value is later passed into the ceiling function.
  // Some case studies:
  //       Case 1: 0.2f < 0.2
  //   float32               -> float
  //   0.2                   0.200000002980232
  //   * 10.0 growth days    * 10.0 growth days
  //   = 2.0                 = 2.00000002980232
  //   |> ceil |> int        |> ceil |> int
  //   = 2                   = 3 (not the same!)
  //   This effect can be seen on crops with a total of 10 growth days (e.g green bean, coffee)
  //   A 0.1 speed reduces the total time to 8 days (instead of 9),
  //   and a 0.2 speed reduces the total time to 7 days (instead of 8).
  //
  //      Case 2: 0.25f = 0.25 (all numbers where 1/(2^n))
  //  float32        -> float
  //  0.25           0.25
  //  ...   Same    ...
  //
  //      Case3: 0.35f > 0.35
  //  float32               -> float
  //  0.35                  0.349999994039536
  //  * 20.0 growth days    * 20.0 growth days
  //  = 7.0                 = 6.99999988079071
  //  |> ceil |> int        |> ceil |> int
  //  = 7                   = 7 (wouldn't be equal if a floor was used instead of ceil)

  let private toF32AndBack (value: float) =
    toF32.[0] <- float32 value
    float toF32.[0]

  let growthTimeWith speed crop =
    if speed = 0.0 then
      totalGrowthTime crop
    else
      let growthStages = Array.ofList <| growthStages crop
      let mutable reductionDays = (speed |> toF32AndBack) * (totalGrowthTime crop |> float) |> ceil |> int
      let mutable daysReduced = 0
      let mutable traverses = 0
      while daysReduced < reductionDays && traverses < 3 do
        let mutable stage = 0
        while daysReduced < reductionDays && stage < growthStages.Length do
          if stage > 0 || growthStages.[stage] > 1 then
            if growthStages.[stage] > 0 then
              growthStages.[stage] <- growthStages.[stage] - 1
              daysReduced <- daysReduced + 1
            else
              // A reduction day was wasted reducing a stage below 0 days.
              reductionDays <- reductionDays - 1
          stage <- stage + 1
        reductionDays <- reductionDays - 1
        // Note the subtraction of a day of reduction after a full traverse.
        // This is because Stardew Valley keeps a "shadow" growth stage of 99999 at the end of growth stage arrays.
        // A day of reduction is wasted on this shadow stage every traverse, leading to a lower than expected reduction in growth time.
        // For example, Cauliflower (growth time = 12 days) with 0.35 speed should be reduced by 12 * 0.35 = 4.2 |> ceil |> int = 5 days,
        // but it is only actually reduced by 4 for a final growth time of 8 days.
        traverses <- traverses + 1
      totalGrowthTime crop - daysReduced

  let isInSeason startDate endDate crop =
    let seasons = selectedSeasons crop
    let isInRange = Season.isBetween startDate.Season endDate.Season
    not seasons.IsEmpty
    && startDate = endDate || isInRange seasons.MinimumElement || isInRange seasons.MaximumElement

  let growthPeriods startDate endDate crop =
    let maxSeasons = 1 + Season.distance startDate.Season endDate.Season
    let mutable days = 0
    let mutable seasonsPassed = 0
    let mutable season = startDate.Season
    seq {
      while seasonsPassed < maxSeasons do
        if selectedSeasons crop |> Set.contains season then
          while seasonsPassed < maxSeasons && selectedSeasons crop |> Set.contains season do
            days <- days + Date.daysIn season startDate endDate
            season <- Season.next season
            seasonsPassed <- seasonsPassed + 1
          days - 1
          days <- 0
        else
          season <- Season.next season
          seasonsPassed <- seasonsPassed + 1
    }

  let harvestsWithin startDate endDate fertSpeed multiplierSpeed = function
  | RegularCrop _ | GiantCrop _ | ForageCrop _ as crop ->
      let growthTime = growthTimeWith (fertSpeed + multiplierSpeed) crop
      Seq.sumBy (fun growthPeriod -> growthPeriod / growthTime) (growthPeriods startDate endDate crop)
  | RegrowCrop r as crop ->
      let growthTime = growthTimeWith (fertSpeed + multiplierSpeed) crop
      Seq.sumBy (fun growthPeriod ->
        if growthPeriod < growthTime
        then 0
        else (growthPeriod - growthTime) / r.RegrowTime + 1)
        (growthPeriods startDate endDate crop)
  | Bush b as crop ->
      let matureDate = startDate + growthTimeWith multiplierSpeed crop
      // Does not provide accurate results if HarvestDayStart = 1 and HarvestDayEnd = 28
      if matureDate |> Date.isNotBetween startDate endDate then
        0
      elif matureDate.Day > b.HarvestDayStart then
        (b.HarvestDayEnd - matureDate.Day + 1) / b.RegrowTime
        + (b.HarvestDayEnd - b.HarvestDayStart + 1) / b.RegrowTime * Season.distance matureDate.Season endDate.Season
      else
        (b.HarvestDayEnd - b.HarvestDayStart + 1) / b.RegrowTime * (1 + Season.distance matureDate.Season endDate.Season)


  let private createBaseWith
    growthMultipliers
    name
    seed
    prices
    (seasons: Season list)
    growthStages =
    let seasonsSet = set seasons
    { Name = name
      Selected = true
      Seasons = seasonsSet
      SelectedSeasons = seasonsSet
      GrowthStages = growthStages
      TotalGrowthTime = Seq.sum growthStages
      GrowthMultipliers = growthMultipliers
      Seed = seed
      PriceFrom = Buy.createAll seed.BasePrice prices |> toMapByKey Buy.source
      BuySeeds = None }

  let private onlyAgri = set [ Multiplier.agri ]

  let private createBaseSeed = createBaseWith onlyAgri

  let private createBase name seed = createBaseSeed name (Seed.create name seed)


  let private createRegularWith
    harvestItem
    doubleCropChance
    extraCrops
    name
    seed
    prices
    seasons
    growthStages
    cropItem
    products =
    RegularCrop
      {| Base = createBaseSeed name seed prices seasons growthStages
         Crop = HarvestCrop.create name cropItem products
         ExtraCrops = extraCrops
         DoubleCrops = doubleCropChance
         SeedMaker = SeedMaker.initial
         HarvestItem = harvestItem |}

  let private createRegular doubleCropChance extraCrops name seed = createRegularWith None doubleCropChance extraCrops name (Seed.create name seed)

  let createScythe = createRegular false 0.0
  let createAmount = createRegular true
  let create = createAmount 0.0

  let private createHarvestItemAmount
    doubleCropChance
    extraCrops
    harvestItem
    harvestAmount
    harvestProducts
    name
    seed =
    match harvestItem with
    | Some item ->
        createRegularWith
          (Some
            { Amount = harvestAmount
              Item = HarvestCrop.createWith item harvestProducts
              Replant = None } )
          doubleCropChance
          extraCrops
          name
          (Seed.create name seed)
    | None ->
        let createdSeed = Seed.create name seed
        createRegularWith
          (Some
            { Amount = harvestAmount
              Item = HarvestCrop.createWith createdSeed harvestProducts
              Replant = Some None } )
          doubleCropChance
          extraCrops
          name
          createdSeed

  let private createHarvestItemWith = createHarvestItemAmount false 0.0

  let createHarvestItem = createHarvestItemWith << Some
  let createHarvestReplant = createHarvestItemWith None


  let private createRegrowCrop
    indoorsOnly
    trelis
    extraCrops
    name
    seed
    prices
    seasons
    growthStages
    regrowTime
    cropItem
    products =
    RegrowCrop
      {| Base = createBase name seed prices seasons growthStages 
         RegrowTime = regrowTime
         Crop = HarvestCrop.create name cropItem products
         ExtraCrops = extraCrops
         Trelis = trelis
         IndoorsOnly = indoorsOnly
         Replant = SeedMakerPlant SeedMaker.initial |}

  let private createOutdoors = createRegrowCrop false
  
  let private createTrelisAmount = createOutdoors true
  let createTrelis = createTrelisAmount 0.0

  let createRegrow = createOutdoors false
  let createRegrowNoExtra = createRegrow 0.0

  let private createRegrowCropHarvestReplant
    indoorsOnly
    trelis
    extraCrops
    name
    seed
    prices
    seasons
    growthStages
    regrowTime
    products =
    let createdSeed = Seed.create name seed
    RegrowCrop
      {| Base = createBaseSeed name createdSeed prices seasons growthStages 
         RegrowTime = regrowTime
         Crop = HarvestCrop.createWith createdSeed products
         ExtraCrops = extraCrops
         Trelis = trelis
         IndoorsOnly = indoorsOnly
         Replant = RawPlant None |}

  let createRegrowHarvestReplant = createRegrowCropHarvestReplant false false


  let createGiantCrop
    name
    seed
    prices
    seasons
    growthStages
    cropItem
    products =
    GiantCrop
      {| Base = createBase name seed prices seasons growthStages
         Crop = HarvestCrop.create name cropItem products
         SeedMaker = SeedMaker.initial |}


  let createBush
    name
    seed
    prices
    seasons
    growthStages
    regrowTime
    harvestDayStart
    harvestDayEnd
    cropItem
    products =
    Bush
      {| Base = createBaseWith Set.empty name (Seed.create name seed) prices seasons growthStages
         RegrowTime = regrowTime
         HarvestDayStart = harvestDayStart
         HarvestDayEnd = harvestDayEnd
         Crop = HarvestCrop.create name cropItem products |}


  let withExtraChance chance = 1.0 / (1.0 - min chance 0.9) - 1.0
  let withYield cropYield = cropYield - 1 |> float
  let withExtraCrops cropYield extraChance = withYield cropYield + withExtraChance extraChance


type Sell =
  | RawCrop of Item
  | Product of Product
  | SeedsFromSeedMaker of Item

type Replant =
  | BuySeeds
  | UseRawCrop of Quality
  | SeedMaker of Quality

type BaseCacheCrop =
  { BaseCrop: BaseCrop
    GrowthMultiplier: float
    SeedPrice: int
    SeedSources: Set<NameOf<Source>> }

// Haha... "Cash" Crop
type CacheCrop =
  | RegularCache of
      {| Base: BaseCacheCrop
         CropSell: Map<Quality, Set<Sell>>
         HarvestedItemSell: Set<Sell>
         Replant: Set<Replant> list |}
  | GiantCache of
      {| Base: BaseCacheCrop
         CropSell: Map<Quality, Set<Sell>>
         Replant: Set<Replant> list |}
  | ForageCache of
      {| Base: BaseCacheCrop
         ForageSell: Map<NameOf<Item>, Map<Quality, Set<Sell>>>
         SellForageSeeds: bool
         Replant: Set<Replant> list
         ForageReplant: bool |}
  | AncientCache of
      {| Base: BaseCacheCrop
         AncientSell: Map<Quality, Set<Sell>>
         Replant: Set<Replant> list |}
  | TeaCache of
      {| Base: BaseCacheCrop
         TeaSell: Set<Sell> |}

type CropSelect =
  | RegularSelect of
      {| CropSell: Sell option
         HarvestedItemSell: Sell option
         Replant: Replant list |}
  | GiantSelect of
      {| CropSell: Sell
         Replant: Replant list |}
  | ForageSelect of
      {| ForageSell: Map<NameOf<Item>, Sell>
         Replant: Replant list |}
  | AncientSelect of
      {| AncientSell: Sell
         Replant: Replant list |}
  | TeaSelect of
      {| TeaSell: Sell |}
