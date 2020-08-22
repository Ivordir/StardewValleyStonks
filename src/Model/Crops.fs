namespace StardewValleyStonks

open Types

type HarvestCrop =
  { Item: Item
    SellRaw: Override
    Products: Map<NameOf<Processor>, Product> }

module HarvestedCrop =
  let crop harvestedCrop = harvestedCrop.Item

  let nameOfCrop = crop >> Item.nameOf

  let create item products =
    { Item = item
      SellRaw = None
      Products = Product.createAll item products |> listToMapByKey Product.processor }

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
    GrowthMultipliers: Set<NameOf<Multiplier>>
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
  | Selected
  | Seasons
  | TotalGrowthTime
  | RegrowTime
  | SeedPrice

type CreateSeed =
  | SeedSell of int
  | Seed of Item

type CreateCropItem =
  | SellPrice of int
  | CropItem of Item
  | SameAsSeed

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
  let private toF32AndBack (value: float) =
    toF32.[0] <- float32 value
    float toF32.[0]

  let growthTimeWith speed crop =
    if speed = 0.0 then
      totalGrowthTime crop
    else
      let growthStages = Array.ofList <| growthStages crop
      let mutable maxReduction = ((speed |> toF32AndBack) * (totalGrowthTime crop |> float)) |> ceil |> int
      let mutable daysReduced = 0
      let mutable traverses = 0
      while daysReduced < maxReduction && traverses < 3 do
        let mutable stage = 0
        while daysReduced < maxReduction && stage < growthStages.Length do
          if growthStages.[stage] > (if stage = 0 then 1 else 0) then
            growthStages.[stage] <- growthStages.[stage] - 1
            daysReduced <- daysReduced + 1
          stage <- stage + 1
        maxReduction <- maxReduction - 1
        traverses <- traverses + 1
      totalGrowthTime crop - daysReduced

  let isInSeason startDate endDate crop =
    selectedSeasons crop |> Set.minElement <= endDate.Season
    && selectedSeasons crop |> Set.maxElement >= startDate.Season

  let growthDays startDate endDate crop =
    let rec helper sum season =
      if selectedSeasons crop |> Set.contains season
      then helper (sum + Date.daysIn season startDate endDate) (Season.next season)
      else sum
    (helper 0 (max startDate.Season (selectedSeasons crop |> Set.minElement))) - 1

  let harvestsWithin startDate endDate speed = function
  | RegularCrop _ | GiantCrop _ | ForageCrop _ as crop -> (growthDays startDate endDate crop) / (growthTimeWith speed crop)
  | RegrowCrop r as crop ->
      let days = growthDays startDate endDate crop
      let growthTime = growthTimeWith speed crop
      if days < growthTime
      then 0
      else (days - growthTime) / r.RegrowTime + 1
  | Bush b as crop ->
      let matureDate = startDate + growthTimeWith speed crop
      // Does not provide accurate results if HarvestDayStart = 1 and HarvestDayEnd = 28
      if matureDate > endDate then
        0
      elif matureDate.Day > b.HarvestDayStart then
        (b.HarvestDayEnd - matureDate.Day + 1) / b.RegrowTime
        + (b.HarvestDayEnd - b.HarvestDayStart + 1) / b.RegrowTime * (endDate.Season - matureDate.Season)
      else
        (b.HarvestDayEnd - b.HarvestDayStart + 1) / b.RegrowTime * (matureDate.Season - endDate.Season + 1)

  let private createBase
    name
    (seasons: Season list)
    growthStages
    seed
    createPrices =
    let seasonsSet = set seasons
    { Name = name
      Selected = true
      Seasons = seasonsSet
      SelectedSeasons = seasonsSet
      GrowthStages = growthStages
      TotalGrowthTime = List.sum growthStages
      GrowthMultipliers = Multiplier.agri
      Seed = seed
      PriceFrom = Buy.createAll seed.BasePrice createPrices |> listToMapByKey Buy.source
      BuySeeds = None }

  let private createSeed name = function
    | SeedSell price -> Item.create (name + " Seeds") price
    | Seed item -> item

  let private createCropItem name seed = function
    | SellPrice price -> Item.createCrop name price
    | CropItem item -> item
    | SameAsSeed -> seed

  let private createSeedAndCropItem name seed cropItem =
    let createdSeed = createSeed name seed
    createdSeed, createCropItem name createdSeed cropItem

  let private createRegular
    doubleCropChance
    extraCrops
    name
    (seasons: Season list)
    growthStages
    seed
    prices
    cropItem
    products =
    let createdSeed, createdCropItem = createSeedAndCropItem name seed cropItem
    RegularCrop
      {| Base = createBase name seasons growthStages createdSeed prices
         Crop = HarvestedCrop.create createdCropItem products
         ExtraCrops = extraCrops
         DoubleCrops = doubleCropChance
         SeedMaker = SeedMaker.initial
         HarvestItem = None |}

  let createAmount = createRegular true
  let createScythe = createRegular false 0.0
  let create = createAmount 0.0

  let private createRegrowCrop
    indoorsOnly
    trelis
    extraCrops
    name
    seasons
    growthStages
    regrowTime
    seed
    createPrices
    cropItem
    products =
    let createdSeed, createdCropItem = createSeedAndCropItem name seed cropItem
    RegrowCrop
      {| Base = createBase name seasons growthStages createdSeed createPrices
         RegrowTime = regrowTime
         Crop = HarvestedCrop.create createdCropItem products
         ExtraCrops = extraCrops
         Trelis = trelis
         IndoorsOnly = indoorsOnly
         Replant =
           if cropItem = SameAsSeed then
             RawPlant None
           else
             SeedMakerPlant SeedMaker.initial |}

  let private createOutdoors = createRegrowCrop false
  let createTrelis = createOutdoors true 0.0
  let createRegrow = createOutdoors false
  let createRegrowNoExtra = createRegrow 0.0

  let createGiantCrop
    name
    season
    growthStages
    seed
    createPrices
    cropItem
    products =
    let createdSeed, createdCropItem = createSeedAndCropItem name seed cropItem
    GiantCrop
      {| Base = createBase name [ season ] growthStages createdSeed createPrices
         Crop = HarvestedCrop.create createdCropItem products
         SeedMaker = SeedMaker.initial |}

  let createWithHarvestItem
    name
    seasons
    growthStages
    seed
    createPrices
    cropItem
    products
    harvestItem
    harvestProducts
    harvestAmount =
    let createdSeed, createdCropItem = createSeedAndCropItem name seed cropItem
    let createdHarvestItem = createCropItem name createdSeed cropItem
    RegularCrop
      {| Base = createBase name seasons growthStages createdSeed createPrices
         Crop = HarvestedCrop.create createdCropItem products
         ExtraCrops = 0.0
         DoubleCrops = false
         SeedMaker = SeedMaker.initial
         HarvestItem =
           Some
             { Amount = harvestAmount
               Item = HarvestedCrop.create createdHarvestItem harvestProducts
               Replant = if harvestItem = SameAsSeed then Some None else None } |}

  let private createBush
    name
    seasons
    growthStages
    regrowTime
    harvestDayStart
    harvestDayEnd
    seed
    createPrices
    cropItem
    products =
    let createdSeed, createdCropItem = createSeedAndCropItem name seed cropItem
    Bush
      {| Base = createBase name seasons growthStages createdSeed createPrices
         RegrowTime = regrowTime
         HarvestDayStart = harvestDayStart
         HarvestDayEnd = harvestDayEnd
         Crop = HarvestedCrop.create createdCropItem products |}

  let withExtraChance chance = 1.0 / (1.0 - min chance 0.9) - 1.0
  let withYield cropYield = cropYield - 1 |> float
  let withExtraCrops cropYield extraChance = withYield cropYield + withExtraChance extraChance

  let all =
    [ create
        "Blue Jazz"
        [ Spring ]
        [ 1; 2; 2; 2 ]
        (Seed <| Item.create "Jazz Seeds" 15)
        PierreAndJoja
        (SellPrice 50)
        NoProduct

      createGiantCrop
        "Cauliflower"
        Spring
        [ 1; 2; 4; 4; 1 ]
        (SeedSell 40)
        PierreAndJoja
        (SellPrice 175)
        Vegetable

      createRegrow
        (withExtraCrops 4 0.02)
        "Coffee"
        [ Spring; Summer ]
        [ 1; 2; 2; 3; 2 ]
        2
        (Seed <| Item.create "Coffee Bean" 15)
        (PriceList [ Buy.create 2500 "Traveling Merchant" ] )
        SameAsSeed
        (ProductList [ Product.createRatio 5 "Keg" (Item.create "Coffee" 150) 1.0 ] )

      create
        "Garlic"
        [ Spring ]
        [ 1; 1; 1; 1 ]
        (SeedSell 20)
        (PriceList [ Buy.createYear2 40 "Pierre" ] )
        (SellPrice 60)
        Vegetable

      createTrelis
        "Green Bean"
        [ Spring ]
        [ 1; 1; 1; 3; 4 ]
        3
        (Seed <| Item.create "Bean Starter" 30)
        PierreAndJoja
        (SellPrice 40)
        Vegetable

      createScythe
        "Kale"
        [ Spring ]
        [ 1; 2; 2; 1 ]
        (SeedSell 35)
        PierreAndJoja
        (SellPrice 110)
        Vegetable

      create
        "Parsnip"
        [ Spring ]
        [ 1; 1; 1; 1 ]
        (SeedSell 10)
        PierreAndJoja
        (SellPrice 35)
        Vegetable

      createAmount
        (withExtraChance 0.2)
        "Potato"
        [ Spring ]
        [ 1; 1; 1; 2; 1 ]
        (SeedSell 25)
        PierreAndJoja
        (SellPrice 80)
        Vegetable

      create
        "Rhubarb"
        [ Spring ]
        [ 2; 2; 2; 3; 4]
        (SeedSell 50)
        Oasis
        (SellPrice 220)
        Fruit

      createRegrow
        (withExtraChance 0.02)
        "Strawberry"
        [ Spring ]
        [ 1; 1; 2; 2; 2 ]
        4
        (SeedSell 0)
        (PriceList [ Buy.create 100 "Pierre" ] )
        (SellPrice 120)
        Fruit

      create
        "Tulip"
        [ Spring ]
        [ 1; 1; 2; 2 ]
        (Seed <| Item.create "Tulip Bulb" 10)
        PierreAndJoja
        (SellPrice 30)
        NoProduct

      createAmount
        (withExtraChance 0.1)
        "Rice"
        [ Spring ]
        [ 1; 2; 2; 3 ]
        (Seed <| Item.create "Rice Shoot" 20)
        Pierre
        (CropItem <| Item.createCrop "Unmilled Rice" 30)
        (CreateAndList (Vegetable, [ Product.create "Mill" (Item.create "Rice" 100) ] ))
      |> addGrowthMultiplier (Name "Irrigated")

      createRegrow
        (withExtraCrops 3 0.02)
        "Blueberry"
        [ Summer ]
        [ 1; 3; 3; 4; 2 ]
        4
        (SeedSell 40)
        Pierre
        (SellPrice 50)
        Fruit

      createRegrowNoExtra
        "Corn"
        [ Summer; Fall ]
        [ 2; 3; 3; 3; 3 ]
        4
        (SeedSell 75)
        PierreAndJoja
        (SellPrice 50)
        (CreateAndList (Vegetable, [ Product.oil ] ))

      createTrelis
        "Hops"
        [ Summer ]
        [ 1; 1; 2; 3; 4 ]
        4
        (Seed <| Item.create "Hops Starter" 30)
        PierreAndJoja
        (SellPrice 25)
        (CreateAndList (Jar Pickle, [ Product.createKeg "Pale Ale" 300 ] ))

      createRegrow
        (withExtraChance 0.03)
        "Hot Pepper"
        [ Summer ]
        [ 1; 1; 1; 1; 1 ]
        3
        (Seed <| Item.create "Pepper Seeds" 20)
        PierreAndJoja
        (SellPrice 40)
        Fruit

      createGiantCrop
        "Melon"
        Summer
        [ 1; 2; 3; 3; 3 ]
        (SeedSell 40)
        PierreAndJoja
        (SellPrice 250)
        Fruit

      create
        "Poppy"
        [ Summer ]
        [ 1; 2; 2; 2 ]
        (SeedSell 50)
        PierreAndJoja
        (SellPrice 140)
        NoProduct

      create
        "Radish"
        [ Summer ]
        [ 2; 1; 2; 1 ]
        (SeedSell 20)
        PierreAndJoja
        (SellPrice 90)
        Vegetable

      create
        "Red Cabbage"
        [ Summer ]
        [ 2; 1; 2; 2; 2 ]
        (SeedSell 50)
        (PriceList [ Buy.createYear2 100 "Pierre" ] )
        (SellPrice 260)
        Vegetable

      create
        "Starfruit"
        [ Summer ]
        [ 2; 3; 2; 3; 3 ]
        (SeedSell 200)
        Oasis
        (SellPrice 750)
        Fruit

      create
        "Summer Spangle"
        [ Summer ]
        [ 1; 2; 3; 2 ]
        (Seed <| Item.create "Spangle Seeds" 25)
        PierreAndJoja
        (SellPrice 90)
        NoProduct

      let oilProduct = ProductList [ Product.oil ]
      createWithHarvestItem
        "Sunflower"
        [ Summer; Fall ]
        [ 1; 2; 3; 2 ]
        (SeedSell 20)
        (PriceList
          [ Buy.create 200 "Pierre"
            Buy.create 125 "Joja" ] )
        (SellPrice 80)
        oilProduct
        SameAsSeed
        oilProduct
        1.0

      createRegrow
        (withExtraChance 0.05)
        "Tomato"
        [ Summer ]
        [ 2; 2; 2; 2; 3 ]
        4
        (SeedSell 25)
        PierreAndJoja
        (SellPrice 60)
        Fruit

      createWithHarvestItem
        "Wheat"
        [ Summer; Fall ]
        [ 1; 1; 1; 1 ]
        (SeedSell 5)
        PierreAndJoja
        (SellPrice 25)
        (CreateAndList
          ( Jar Pickle,
            [ Product.createKeg "Beer" 200
              Product.create "Mill" (Item.create "Wheat Flour" 50) ] ))
        (CropItem <| Item.create "Hay" 50)
        NoProduct
        0.4

      createScythe
        "Amaranth"
        [ Fall ]
        [ 1; 2; 2; 2 ]
        (SeedSell 35)
        PierreAndJoja
        (SellPrice 150)
        Vegetable

      create
        "Artichoke"
        [ Fall ]
        [ 2; 2; 1; 2; 1 ]
        (SeedSell 15)
        (PriceList [ Buy.createYear2 30 "Pierre" ] )
        (SellPrice 160)
        Vegetable

      create
        "Beet"
        [ Fall ]
        [ 1; 1; 2; 2 ]
        (SeedSell 10)
        Oasis
        (SellPrice 100)
        (CreateAndList (Vegetable, [ Product.createRatio 1 "Mill" (Item.create "Sugar" 50) 3.0 ] ))

      create
        "Bok Choy"
        [ Fall ]
        [ 1; 1; 1; 1 ]
        (SeedSell 25)
        PierreAndJoja
        (SellPrice 80)
        Vegetable

      createRegrow
        (withExtraCrops 2 0.1)
        "Cranberries"
        [ Fall ]
        [ 1; 2; 1; 1; 2 ]
        5
        (SeedSell 60)
        (JojaWith <| Price.create 240 "Pierre")
        (SellPrice 75)
        Fruit

      createRegrow
        (withExtraChance 0.002)
        "Eggplant"
        [ Fall ]
        [ 1; 1; 1; 1; 1 ]
        5
        (SeedSell 10)
        PierreAndJoja
        (SellPrice 60)
        Vegetable

      create
        "Fairy Rose"
        [ Fall ]
        [ 1; 4; 4; 3 ]
        (Seed <| Item.create "Fairy Seeds" 100)
        PierreAndJoja
        (SellPrice 290)
        NoProduct

      createTrelis
        "Grape"
        [ Fall ]
        [ 1; 1; 2; 3; 3 ]
        3
        (Seed <| Item.create "Grape Starter" 30)
        PierreAndJoja
        (SellPrice 30)
        Fruit

      createGiantCrop
        "Pumpkin"
        Fall
        [ 1; 2; 3; 4; 3 ]
        (SeedSell 50)
        PierreAndJoja
        (SellPrice 320)
        Vegetable

      create
        "Sweet Gem Berry"
        [ Fall ]
        [ 2; 4; 6; 6; 6 ]
        (Seed <| Item.create "Rare Seed" 200)
        (PriceList [ Buy.create 1000 "Traveling Merchant" ] )
        (CropItem <| Item.create "Sweet Gem Berry" 3000)
        NoProduct

      create
        "Yam"
        [ Fall ]
        [ 1; 3; 3; 3 ]
        (SeedSell 30)
        PierreAndJoja
        (SellPrice 160)
        Vegetable

      createRegrowNoExtra
        "Ancient Fruit"
        [ Spring; Summer; Fall ]
        [ 2; 7; 7; 7; 5 ]
        7
        (Seed <| Item.create "Ancient Seeds" 30)
        NoPrice
        (SellPrice 550)
        Fruit

      createBush
        "Tea"
        [ Spring; Summer; Fall ]
        [ 10; 10 ]
        1
        22
        28
        (Seed <| Item.create "Tea Sapling" 500)
        (PriceList [ Buy.create 100 "Crafting" ])
        (CropItem <| Item.createCrop "Tea Leaves" 50)
        (CreateAndList (Vegetable, [ Product.createKeg "Green Tea" 100 ] )) ]

type Sell =
  | RawCrop of Crop: Item
  | Product of Product
  | SeedsFromSeedMaker of Seed: Item

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
