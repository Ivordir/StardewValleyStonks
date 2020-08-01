namespace StardewValleyStonks

open Types

type HarvestAmount =
  | CropAmount of ExtraCrops: float * DoubleCropChance: bool
  | GiantCrop
  | Amount of float

module HarvestAmount =
  let common = CropAmount (0.0, true)
  let scythe = CropAmount (0.0, false)

  let extra cropYield extraChance = 1.0 / (1.0 - extraChance) + float cropYield - 2.0
  let withChance chance = extra 1 chance
  let withYield cropYield = extra cropYield 0.0

  let extraAmount cropYield extraChance = CropAmount (extra cropYield extraChance, true)
  let extraYield cropYield = CropAmount (withYield cropYield, true)
  let extraChance extraChance = CropAmount (withChance extraChance, true)

type HarvestedItem =
  { Item: Item
    Amount: HarvestAmount
    SellRawItemOverride: bool option
    Products: Map<NameOf<Processor>, Product> }

module HarvestedItem =
  let item harvestedItem = harvestedItem.Item

  let nameOfItem = item >> Item.nameOf

  let create item amount products =
    { Item = item
      Amount = amount
      SellRawItemOverride = None
      Products = Product.createAll item products |> listToMap Product.processor }

type Crop =
  { Name: string
    Selected: bool
    Seasons: Set<Season>
    SelectedSeasons: Set<Season>
    GrowthStages: int list
    TotalGrowthTime: int
    RegrowTime: int option
    GrowthMultipliers: NameOf<Multiplier> list
    Seed: Item
    PriceFrom: Map<NameOf<Source>, Price>
    BuySeedsOverride: bool option
    SeedMaker:
      {| Input: Item
         SellSeedsOverride: bool option
         ReplantOverride: bool option |} option
    SeedOrCropOverride: bool option
    HarvestedItems: Map<NameOf<Item>, HarvestedItem> }
  member this.Toggle = { this with Selected = not this.Selected }

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
    | Item of Item
    | SameAsSeed

module Crop =
  let name crop = crop.Name
  let selected crop = crop.Selected
  let seasons crop = crop.Seasons
  let totalGrowthTime crop = crop.TotalGrowthTime
  let regrowTime crop = crop.RegrowTime
  let priceFrom crop = crop.PriceFrom

  let nameOf = toNameOf name

  open Fable.Core.JsInterop

  let private convertToF32AndBack(f64: float): float = importMember "./util.js"

  let growthTimeWith speed crop =
    if speed = 0.0 then
      crop.TotalGrowthTime
    else
      let growthStages = Array.ofList crop.GrowthStages
      let mutable maxReduction = (convertToF32AndBack speed) * (float crop.TotalGrowthTime) |> ceil |> int
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
      crop.TotalGrowthTime - daysReduced

  let initial =
    { Name = "initial"
      Selected = true
      Seasons = Set.empty
      SelectedSeasons = Set.empty
      GrowthStages = List.empty
      TotalGrowthTime = -1
      RegrowTime = None //
      GrowthMultipliers = Multiplier.agri //
      Seed = Item.initial
      PriceFrom = Map.empty
      BuySeedsOverride = None
      SeedMaker = None
      SeedOrCropOverride = None
      HarvestedItems = Map.empty }

  let createSeedMaker cropItem =
    Some
      {| Input = cropItem
         SellSeedsOverride = None
         ReplantOverride = None |}

  let private createCrop
    seedMaker
    harvestAmount
    regrowTime
    name
    (seasons: Season list)
    growthStages
    seed
    prices
    cropItem
    products =
    let seasonsSet = set seasons
    let createdSeed =
      match seed with
      | SeedSell price ->
          { Name = name + " Seeds"
            BasePrice = price
            Multiplier = None }
      | Seed item -> item
    let createdCropItem =
      match cropItem with
      | SellPrice price ->
          { Name = name
            BasePrice = price
            Multiplier = Some (Name "Tiller") }
      | Item item -> item
      | SameAsSeed -> createdSeed
    { initial with
        Name = name
        Seasons = seasonsSet
        SelectedSeasons = seasonsSet
        GrowthStages = growthStages
        TotalGrowthTime = List.sum growthStages
        RegrowTime = regrowTime
        Seed = createdSeed
        PriceFrom = Price.createAll createdSeed.BasePrice prices |> listToMap Price.nameOf
        SeedMaker = if seedMaker then createSeedMaker createdCropItem else None
        HarvestedItems =
          [ HarvestedItem.create createdCropItem harvestAmount products ]
          |> listToMap HarvestedItem.nameOfItem }

  let mergeWith f a (b : Map<_,_>) =
    Map.fold (fun s k v ->
      match Map.tryFind k s with
      | Some v' -> Map.add k (f k (v, v')) s
      | None -> Map.add k v s)
      a
      b

  let merge a b = mergeWith (invalidArg "'a' or 'b'" "The maps had one or more of the same key(s).") a b

  let withOtherHarvestedItems items crop =
    { crop with
        HarvestedItems =
          [ for item, amount, products in items do
              HarvestedItem.create item amount products ]
          |> listToMap HarvestedItem.nameOfItem
          |> merge crop.HarvestedItems }

  let createWithoutSeedMaker = createCrop false
  let createWithAmount = createCrop true

  let createGiantCrop = createWithAmount GiantCrop None
  let createScythe = createWithAmount HarvestAmount.scythe None
  let createWithRegrow = createWithAmount HarvestAmount.common

  let create = createWithRegrow None

  open HarvestAmount

  let all =
    [ create
        "Blue Jazz"
        [ Spring ]
        [ 1; 2; 2; 2 ]
        (Seed (Item.create "Jazz Seeds" 15))
        PierreAndJoja
        (SellPrice 50)
        NoProduct

      createGiantCrop
        "Cauliflower"
        [ Spring ]
        [ 1; 2; 4; 4; 1 ]
        (SeedSell 40)
        PierreAndJoja
        (SellPrice 175)
        Vegetable

      createWithoutSeedMaker
        (extraAmount 4 0.02)
        (Some 2)
        "Coffee"
        [ Spring; Summer ]
        [ 1; 2; 2; 3; 2 ]
        (Seed (Item.create "Coffee Bean" 15))
        (PriceList [ Price.create 2500 "Traveling Merchant" ] )
        SameAsSeed
        (ProductList
          [ RatioProcess
              {| InputAmount = 5
                 Processor = Name "Keg"
                 Output = Item.create "Coffee" 150
                 OutputAmount = 1.0
                 Override = None |} ] )

      create
        "Garlic"
        [ Spring ]
        [ 1; 1; 1; 1 ]
        (SeedSell 20)
        (PriceList [ Price.createYear2 40 "Pierre" ] )
        (SellPrice 60)
        Vegetable

      createWithRegrow
        (Some 3)
        "Green Bean"
        [ Spring ]
        [ 1; 1; 1; 3; 4 ]
        (Seed (Item.create "Bean Starter" 30))
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
        (PriceList [ Price.create 100 "Pierre" ] )
        (SellPrice 120)
        Fruit

      create
        "Tulip"
        [ Spring ]
        [ 1; 1; 2; 2 ]
        (SeedSell 30)
        PierreAndJoja
        (Item (Item.createCrop "Tulip Bulb" 10))
        NoProduct

      { createWithAmount
          (CropAmount (withChance 0.1, false))
          None
          "Rice"
          [ Spring ]
          [ 1; 2; 2; 3 ]
          (Seed (Item.create "Rice Shoot" 20))
          Pierre
          (Item (Item.createCrop "Unmilled Rice" 30))
          (CreateAndList
            ( Vegetable,
              [ Process
                  {| Processor = Name "Mill"
                     Output = Item.create "Rice" 100
                     Override = None |} ] ))
        with
          GrowthMultipliers = Name "Irrigated"::Multiplier.agri }

      createWithAmount
        (extraAmount 3 0.02)
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
        (Seed (Item.create "Hops Starter" 30))
        PierreAndJoja
        (SellPrice 25)
        (CreateAndList (Jar Pickle, [ Product.createKeg "Pale Ale" 300 ] ))

      createWithAmount
        (extraChance 0.03)
        (Some 3)
        "Hot Pepper"
        [ Summer ]
        [ 1; 1; 1; 1; 1 ]
        (Seed (Item.create "Pepper Seeds" 20))
        PierreAndJoja
        (SellPrice 40)
        Fruit

      createGiantCrop
        "Melon"
        [ Summer ]
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
        (PriceList [ Price.createYear2 100 "Pierre" ] )
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
        (Seed (Item.create "Spangle Seeds" 25))
        PierreAndJoja
        (SellPrice 90)
        NoProduct

      createScythe
        "Sunflower"
        [ Summer; Fall ]
        [ 1; 2; 3; 2 ]
        (SeedSell 20)
        (PriceList
          [ Price.create 200 "Pierre"
            Price.create 125 "Joja" ] )
        (SellPrice 80)
        (ProductList [ Product.oil ] )
      |> withOtherHarvestedItems [ Item.create "Sunflower Seeds" 20, Amount 1.0, ProductList [ Product.oil ] ]

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
              Process
                {| Processor = Name "Mill"
                   Output = Item.create "Wheat Flour" 50
                   Override = None |} ] ))

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
        (PriceList [ Price.createYear2 30 "Pierre" ] )
        (SellPrice 160)
        Vegetable

      create
        "Beet"
        [ Fall ]
        [ 1; 1; 2; 2 ]
        (SeedSell 10)
        Oasis
        (SellPrice 100)
        (CreateAndList
          ( Vegetable,
            [ RatioProcess
                {| InputAmount = 1
                   Processor = Name "Mill"
                   Output = Item.create "Sugar" 50
                   OutputAmount = 3.0
                   Override = None |} ] ))

      create
        "Bok Choy"
        [ Fall ]
        [ 1; 1; 1; 1 ]
        (SeedSell 25)
        PierreAndJoja
        (SellPrice 80)
        Vegetable

      createWithAmount
        (extraAmount 2 0.1)
        (Some 5)
        "Cranberries"
        [ Fall ]
        [ 1; 2; 1; 1; 2 ]
        (SeedSell 60)
        (Joja (Price.create 240 "Pierre"))
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
        (Seed (Item.create "Fairy Seeds" 100))
        PierreAndJoja
        (SellPrice 290)
        NoProduct

      createWithRegrow
        (Some 3)
        "Grape"
        [ Fall ]
        [ 1; 1; 2; 3; 3 ]
        (Seed (Item.create "Grape Starter" 30))
        PierreAndJoja
        (SellPrice 30)
        Fruit

      createGiantCrop
        "Pumpkin"
        [ Fall ]
        [ 1; 2; 3; 4; 3 ]
        (SeedSell 50)
        PierreAndJoja
        (SellPrice 320)
        Vegetable

      create
        "Sweet Gem Berry"
        [ Fall ]
        [ 2; 4; 6; 6; 6 ]
        (Seed (Item.create "Rare Seed" 200))
        (PriceList [ Price.create 1000 "Traveling Merchant" ] )
        (Item (Item.create "Sweet Gem Berry" 3000))
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
        (Seed (Item.create "Ancient Seeds" 30))
        NoPrice
        (SellPrice 550)
        Fruit
    ]

type HarvestedItemCache =
  { HarvestItem: HarvestedItem
    Product: Product list }

// Haha... "Cash" Crop
type CacheCrop =
  { Crop: Crop
    GrowthMultiplier: float
    BestProducts: Map<Quality, Product list>
    SeedPrice: int option
    SeedSources: NameOf<Source> list
    HarvestedItemCache: Map<NameOf<Item>, HarvestedItemCache>
    BestReplants: NameOf<Item> list }