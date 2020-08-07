namespace StardewValleyStonks

open Types

type HarvestedCrop =
  { Crop: Item
    SellRawItemOverride: Override
    Products: Map<NameOf<Processor>, Product> }

module HarvestedCrop =
  let crop harvestedCrop = harvestedCrop.Crop

  let nameOfCrop = crop >> Item.nameOf

  let create item products =
    { Crop = item
      SellRawItemOverride = None
      Products = Product.createAll item products |> listToMap Product.processor }

type SeedMaker =
  { Input: Item
    SellSeedsOverride: Override
    ReplantOverride: Override }

module SeedMaker =
  let create input =
    { Input = input
      SellSeedsOverride = None
      ReplantOverride = None }

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
    BuySeedsOverride: Override
    SeedMaker: SeedMaker option
    RawCropReplantOverride: Override option }
  member this.Toggle = { this with Selected = not this.Selected }

type Crop =
  | RegularCrop of
      {| Base: BaseCrop
         RegrowTime: int option
         Crop: HarvestedCrop
         ExtraCrops: float
         DoubleCropChance: bool
         Trelis: bool
         IndoorsOnly: bool
         FertilizerCompatable: bool
         HarvestedItem: (float * HarvestedCrop) option |}
  | GiantCrop of
      {| Base: BaseCrop
         Crop: HarvestedCrop |}
  | ForageCrop of
      {| Base: BaseCrop
         NumberOfCropKinds: int
         Crops: Map<NameOf<Item>, HarvestedCrop>
         SellForageSeedsOverride: Override
         ForageSeedsReplantOverride: Override |}
  member this.Toggle =
    match this with
    | RegularCrop c ->
      RegularCrop {| c with Base = c.Base.Toggle |}
    | GiantCrop g ->
      GiantCrop {| g with Base = g.Base.Toggle |}
    | ForageCrop f ->
      ForageCrop {| f with Base = f.Base.Toggle |}

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
  let private baseBuySeedsOverride crop = crop.BuySeedsOverride
  let private baseSeedMaker crop = crop.SeedMaker
  let private baseRawCropReplantOverride crop = crop.RawCropReplantOverride

  let private baseAddGrowthMultiplier multiplier crop =
    { crop with GrowthMultipliers = crop.GrowthMultipliers.Add(multiplier) }

  let baseCrop = function
    | RegularCrop c -> c.Base
    | GiantCrop g -> g.Base
    | ForageCrop f -> f.Base

  let name = baseCrop >> baseName
  let selected = baseCrop >> baseSelected
  let seasons = baseCrop >> baseSeasons
  let selectedSeasons = baseCrop >> baseSelectedSeasons
  let growthStages = baseCrop >> baseGrowthStages
  let totalGrowthTime = baseCrop >> baseTotalGrowthTime
  let growthMultipliers = baseCrop >> baseGrowthMultipliers
  let priceFrom = baseCrop >> basePriceFrom
  let buySeedsOverride = baseCrop >> baseBuySeedsOverride
  let seedMaker = baseCrop >> baseSeedMaker
  let rawCropReplantOverride = baseCrop >> baseRawCropReplantOverride

  let regrowTime = function
    | RegularCrop c -> c.RegrowTime
    | _ -> None

  let withBase baseCrop = function
    | RegularCrop r ->
      RegularCrop {| r with Base = baseCrop |}
    | GiantCrop g ->
      GiantCrop {| g with Base = baseCrop |}
    | ForageCrop f ->
      ForageCrop {| f with Base = baseCrop |}

  let applyBase = flip withBase

  let modifyBase baseWith modification crop =
    crop
    |> baseCrop
    |> baseWith modification
    |> applyBase crop

  let addGrowthMultiplier = modifyBase baseAddGrowthMultiplier

  let withHarvestedItem amount item = function
    | RegularCrop c ->
      RegularCrop
        {| c with
             HarvestedItem = Some (amount, item)
             Base =
               if item.Crop = c.Base.Seed then
                 { c.Base with RawCropReplantOverride = Some None }
               else
                 c.Base |}
    | GiantCrop _ -> invalidArg "crop" "Giant crops don't have harvested items."
    | ForageCrop _ -> invalidArg "crop" "Forage crops don't have harvested items."

  let nameOf = toNameOf name

  open Fable.Core.JsInterop
  let private convertToF32AndBack (f64: float): float = importMember "./util.js"

  let growthTimeWith speed crop =
    if speed = 0.0 then
      totalGrowthTime crop
    else
      let growthStages = Array.ofList <| growthStages crop
      let mutable maxReduction = (convertToF32AndBack speed) * (float <| totalGrowthTime crop) |> ceil |> int
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

  let createSeedMaker cropItem =
    Some
      { Input = cropItem
        SellSeedsOverride = None
        ReplantOverride = None }

  let private createBase
    name
    (seasons: Season list)
    growthStages
    seed
    createPrices
    seedMakerItem
    rawCropReplant =
    let seasonsSet = set seasons
    { Name = name
      Selected = true
      Seasons = seasonsSet
      SelectedSeasons = seasonsSet
      GrowthStages = growthStages
      TotalGrowthTime = List.sum growthStages
      GrowthMultipliers = Multiplier.agri
      Seed = seed
      PriceFrom = Buy.createAll seed.BasePrice createPrices |> listToMap Buy.source
      BuySeedsOverride = None
      SeedMaker = Option.bind (SeedMaker.create >> Some) seedMakerItem
      RawCropReplantOverride = if rawCropReplant then Some None else None }

  let createSeed name = function
    | SeedSell price -> Item.create (name + " Seeds") price
    | Seed item -> item

  let createSeedAndCropItem name seed cropItem =
    let createdSeed = createSeed name seed
    createdSeed,
    match cropItem with
    | SellPrice price -> Item.createCrop name price
    | CropItem item -> item
    | SameAsSeed -> createdSeed

  let private createCrop
    seedMaker
    doubleCropChance
    extraCrops
    regrowTime
    name
    (seasons: Season list)
    growthStages
    seed
    prices
    cropItem
    products =
    let createdSeed, createdCropItem = createSeedAndCropItem name seed cropItem
    RegularCrop
      {| Base =
           createBase
            name
            seasons
            growthStages
            createdSeed
            prices
            (if seedMaker then Some createdCropItem else None)
            (cropItem = SameAsSeed)
         RegrowTime = regrowTime
         Crop = HarvestedCrop.create createdCropItem products
         ExtraCrops = extraCrops
         DoubleCropChance = doubleCropChance
         Trelis = false
         IndoorsOnly = false
         FertilizerCompatable = true
         HarvestedItem = None |}

  let createWithSeedMaker = createCrop true
  let createWithoutSeedMaker = createCrop false

  let createWithAmount = createWithSeedMaker true
  let createScythe = createWithSeedMaker false 0.0 None

  let createWithRegrow = createWithAmount 0.0

  let create = createWithRegrow None

  let extraCrops cropYield extraChance = 1.0 / (1.0 - extraChance) + float cropYield - 2.0
  let extraChance chance = extraCrops 1 chance
  let withYield cropYield = extraCrops cropYield 0.0

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
      {| Base = createBase name [ season ] growthStages createdSeed createPrices (Some createdCropItem) false
         Crop = HarvestedCrop.create createdCropItem products |}

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

      createWithoutSeedMaker
        true
        (extraCrops 4 0.02)
        (Some 2)
        "Coffee"
        [ Spring; Summer ]
        [ 1; 2; 2; 3; 2 ]
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

      createWithRegrow
        (Some 3)
        "Green Bean"
        [ Spring ]
        [ 1; 1; 1; 3; 4 ]
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

      createWithAmount
        (extraChance 0.2)
        None
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

      createWithAmount
        (extraChance 0.02)
        (Some 4)
        "Strawberry"
        [ Spring ]
        [ 1; 1; 2; 2; 2 ]
        (SeedSell 0)
        (PriceList [ Buy.create 100 "Pierre" ] )
        (SellPrice 120)
        Fruit

      create
        "Tulip"
        [ Spring ]
        [ 1; 1; 2; 2 ]
        (SeedSell 30)
        PierreAndJoja
        (CropItem <| Item.createCrop "Tulip Bulb" 10)
        NoProduct

      createWithSeedMaker
        false
        (extraChance 0.1)
        None
        "Rice"
        [ Spring ]
        [ 1; 2; 2; 3 ]
        (Seed <| Item.create "Rice Shoot" 20)
        Pierre
        (CropItem <| Item.createCrop "Unmilled Rice" 30)
        (CreateAndList (Vegetable, [ Product.create "Mill" (Item.create "Rice" 100) ] ))
      |> addGrowthMultiplier (Name "Irrigated")

      createWithAmount
        (extraCrops 3 0.02)
        (Some 4)
          "Blueberry"
          [ Summer ]
          [ 1; 3; 3; 4; 2 ]
          (SeedSell 40)
          Pierre
          (SellPrice 50)
          Fruit

      createWithRegrow
        (Some 4)
        "Corn"
        [ Summer; Fall ]
        [ 2; 3; 3; 3; 3 ]
        (SeedSell 75)
        PierreAndJoja
        (SellPrice 50)
        (CreateAndList (Vegetable, [ Product.oil ] ))

      createWithRegrow
        (Some 4)
        "Hops"
        [ Summer ]
        [ 1; 1; 2; 3; 4 ]
        (Seed <| Item.create "Hops Starter" 30)
        PierreAndJoja
        (SellPrice 25)
        (CreateAndList (Jar Pickle, [ Product.createKeg "Pale Ale" 300 ] ))

      createWithAmount
        (extraChance 0.03)
        (Some 3)
        "Hot Pepper"
        [ Summer ]
        [ 1; 1; 1; 1; 1 ]
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

      createScythe
        "Sunflower"
        [ Summer; Fall ]
        [ 1; 2; 3; 2 ]
        (SeedSell 20)
        (PriceList
          [ Buy.create 200 "Pierre"
            Buy.create 125 "Joja" ] )
        (SellPrice 80)
        (ProductList [ Product.oil ] )
      |> withHarvestedItem 1.0 (HarvestedCrop.create (Item.create "Sunflower Seeds" 20) (ProductList [ Product.oil ] ))

      createWithAmount
        (extraChance 0.05)
        (Some 4)
        "Tomato"
        [ Summer ]
        [ 2; 2; 2; 2; 3 ]
        (SeedSell 25)
        PierreAndJoja
        (SellPrice 60)
        Fruit

      createScythe
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

      createWithAmount
        (extraCrops 2 0.1)
        (Some 5)
        "Cranberries"
        [ Fall ]
        [ 1; 2; 1; 1; 2 ]
        (SeedSell 60)
        (Joja <| Price.create 240 "Pierre")
        (SellPrice 75)
        Fruit

      createWithAmount
        (extraChance 0.002)
        (Some 5)
        "Eggplant"
        [ Fall ]
        [ 1; 1; 1; 1; 1 ]
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

      createWithRegrow
        (Some 3)
        "Grape"
        [ Fall ]
        [ 1; 1; 2; 3; 3 ]
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

      createWithRegrow
        (Some 7)
        "Ancient Fruit"
        [ Spring; Summer; Fall ]
        [ 2; 7; 7; 7; 5 ]
        (Seed <| Item.create "Ancient Seeds" 30)
        NoPrice
        (SellPrice 550)
        Fruit
    ]

type Sell =
  | RawCrop
  | Product of NameOf<Processor>
  | SeedsFromSeedMaker

type Replant =
  | UseRawCrop of Quality
  | BuySeeds
  | SeedMaker of Quality

type HarvestedCacheCrop = Map<Quality, Set<Sell>>

type BaseCacheCrop =
  { BaseCrop: BaseCrop
    GrowthMultiplier: float
    SeedPrice: int
    SeedSources: Set<NameOf<Source>>
    Replant: Set<Replant> list }

// Haha... "Cash" Crop
type CacheCrop =
  | RegularCache of
      {| Base: BaseCacheCrop
         CropSell: Map<Quality, Set<Sell>>
         HarvestItemSell: Map<Quality, Set<Sell>> |}
  | GiantCahce of
      {| Base: BaseCacheCrop
         CropSell: Map<Quality, Set<Sell>> |}
  | ForageCache of
      {| Base: BaseCacheCrop
         ForageSell: Map<NameOf<Item>, Map<Quality, Set<Sell>>>
         SellForageSeeds: bool
         ForeageReplant: bool |}