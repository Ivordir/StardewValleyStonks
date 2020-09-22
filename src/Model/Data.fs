module StardewValleyStonks.AppData

open Types

let skills =
    [ Skill.create
        Farming
        ("Tiller", CreatePair ("Agriculturist", "Artisan"))

      Skill.create
        Foraging
        ("Gatherer", CreateSingle "Botanist") ]

let buySources =
  [ "Pierre"
    "Joja"
    "Oasis"
    "Traveling Merchant"
    "Crafting" ]
  |> List.map Source.create

let priceMultipliers =
  [ PriceMultiplier.create
      "Joja Membership"
      1.25
      false ]

let fertilizers =
  [ Fertilizer.create
      "Basic Fertilizer"
      1
      0.0
      [ Buy.create 100 "Pierre" ]

    Fertilizer.create
      "Quality Fertilizer"
      2
      0.0
      [ Buy.create 150 "Pierre" ]

    Fertilizer.create
      "Speed-Gro"
      0
      0.1
      [ Buy.create 100 "Pierre" ]

    Fertilizer.create
      "Deluxe Speed-Gro"
      0
      0.25
      [ Buy.create 150 "Pierre"
        Buy.create 80 "Oasis" ] ]

let processors =
  [ Processor.create "Preserves Jar" Farming 4
    Processor.create "Keg" Farming 8
    Processor.create "Oil Maker" Farming 8
    Processor.createNoUnlock "Mill" ]

let rawMultipliers =
  [ RawMultiplier.create "Irrigated" 1.1
    RawMultiplier.create "Bear's Knowledge" 3.0 ]

let professionMultipliers =
  [ ProfessionMultiplier.create Farming "Tiller" 1.1
    ProfessionMultiplier.create Farming "Artisan" 1.4
    ProfessionMultiplier.create Farming "Agriculturist" 0.1
    ProfessionMultiplier.create Foraging "Gatherer" 1.2 ]


let oil = Product.create "Oil Maker" (Item.create "Oil" 100)

open Seasons
open Crop

let crops =
  [ create
      "Blue Jazz"
      (Seed <| Item.create "Jazz Seeds" 15)
      PierreAndJoja
      [ spring ]
      [ 1; 2; 2; 2 ]
      (SellPrice 50)
      NoProduct

    createGiantCrop
      "Cauliflower"
      (SeedSell 40)
      PierreAndJoja
      [ spring ]
      [ 1; 2; 4; 4; 1 ]
      (SellPrice 175)
      Vegetable

    createRegrowHarvestReplant
      (withExtraCrops 4 0.02)
      "Coffee"
      (Seed <| Item.create "Coffee Bean" 15)
      (PriceList [ Buy.create 2500 "Traveling Merchant" ] )
      [ spring; summer ]
      [ 1; 2; 2; 3; 2 ]
      2
      (ProductList [ Product.createRatio 5 "Keg" (Item.create "Coffee" 150) 1.0 ] )

    create
      "Garlic"
      (SeedSell 20)
      Pierre
      [ spring ]
      [ 1; 1; 1; 1 ]
      (SellPrice 60)
      Vegetable

    createTrelis
      "Green Bean"
      (Seed <| Item.create "Bean Starter" 30)
      PierreAndJoja
      [ spring ]
      [ 1; 1; 1; 3; 4 ]
      3
      (SellPrice 40)
      Vegetable

    createScythe
      "Kale"
      (SeedSell 35)
      PierreAndJoja
      [ spring ]
      [ 1; 2; 2; 1 ]
      (SellPrice 110)
      Vegetable

    create
      "Parsnip"
      (SeedSell 10)
      PierreAndJoja
      [ spring ]
      [ 1; 1; 1; 1 ]
      (SellPrice 35)
      Vegetable

    createAmount
      (withExtraChance 0.2)
      "Potato"
      (SeedSell 25)
      PierreAndJoja
      [ spring ]
      [ 1; 1; 1; 2; 1 ]
      (SellPrice 80)
      Vegetable

    create
      "Rhubarb"
      (SeedSell 50)
      Oasis
      [ spring ]
      [ 2; 2; 2; 3; 4]
      (SellPrice 220)
      Fruit

    createRegrow
      (withExtraChance 0.02)
      "Strawberry"
      (SeedSell 0)
      (PriceList [ Buy.create 100 "Pierre" ] )
      [ spring ]
      [ 1; 1; 2; 2; 2 ]
      4
      (SellPrice 120)
      Fruit

    create
      "Tulip"
      (Seed <| Item.create "Tulip Bulb" 10)
      PierreAndJoja
      [ spring ]
      [ 1; 1; 2; 2 ]
      (SellPrice 30)
      NoProduct

    createAmount
      (withExtraChance 0.1)
      "Rice"
      (Seed <| Item.create "Rice Shoot" 20)
      Pierre
      [ spring ]
      [ 1; 2; 2; 3 ]
      (CropItem <| Item.createCrop "Unmilled Rice" 30)
      (CreateAndAlso (Vegetable, [ Product.create "Mill" (Item.create "Rice" 100) ] ))
    |> addGrowthMultiplier (Multiplier.raw "Irrigated")

    createRegrow
      (withExtraCrops 3 0.02)
      "Blueberry"
      (SeedSell 40)
      Pierre
      [ summer ]
      [ 1; 3; 3; 4; 2 ]
      4
      (SellPrice 50)
      Fruit

    createRegrowNoExtra
      "Corn"
      (SeedSell 75)
      PierreAndJoja
      [ summer; fall ]
      [ 2; 3; 3; 3; 3 ]
      4
      (SellPrice 50)
      (CreateAndAlso (Vegetable, [ oil ] ))

    createTrelis
      "Hops"
      (Seed <| Item.create "Hops Starter" 30)
      PierreAndJoja
      [ summer ]
      [ 1; 1; 2; 3; 4 ]
      1
      (SellPrice 25)
      (CreateAndAlso (Jar Pickle, [ Product.createKeg "Pale Ale" 300 ] ))

    createRegrow
      (withExtraChance 0.03)
      "Hot Pepper"
      (Seed <| Item.create "Pepper Seeds" 20)
      PierreAndJoja
      [ summer ]
      [ 1; 1; 1; 1; 1 ]
      3
      (SellPrice 40)
      Fruit

    createGiantCrop
      "Melon"
      (SeedSell 40)
      PierreAndJoja
      [ summer ]
      [ 1; 2; 3; 3; 3 ]
      (SellPrice 250)
      Fruit

    create
      "Poppy"
      (SeedSell 50)
      PierreAndJoja
      [ summer ]
      [ 1; 2; 2; 2 ]
      (SellPrice 140)
      NoProduct

    create
      "Radish"
      (SeedSell 20)
      PierreAndJoja
      [ summer ]
      [ 2; 1; 2; 1 ]
      (SellPrice 90)
      Vegetable

    create
      "Red Cabbage"
      (SeedSell 50)
      Pierre
      [ summer ]
      [ 2; 1; 2; 2; 2 ]
      (SellPrice 260)
      Vegetable

    create
      "Starfruit"
      (SeedSell 200)
      Oasis
      [ summer ]
      [ 2; 3; 2; 3; 3 ]
      (SellPrice 750)
      Fruit

    create
      "Summer Spangle"
      (Seed <| Item.create "Spangle Seeds" 25)
      PierreAndJoja
      [ summer ]
      [ 1; 2; 3; 2 ]
      (SellPrice 90)
      NoProduct

    let oilProduct = ProductList [ oil ]
    createHarvestReplant
      1.0
      oilProduct
      "Sunflower"
      (SeedSell 20)
      (PriceList
        [ Buy.create 200 "Pierre"
          Buy.create 125 "Joja" ] )
      [ summer; fall ]
      [ 1; 2; 3; 2 ]
      (SellPrice 80)
      oilProduct

    createRegrow
      (withExtraChance 0.05)
      "Tomato"
      (SeedSell 25)
      PierreAndJoja
      [ summer ]
      [ 2; 2; 2; 2; 3 ]
      4
      (SellPrice 60)
      Fruit

    createHarvestItem
      (Item.create "Hay" 50)
      0.4
      NoProduct
      "Wheat"
      (SeedSell 5)
      PierreAndJoja
      [ summer; fall ]
      [ 1; 1; 1; 1 ]
      (SellPrice 25)
      (CreateAndAlso
        ( Jar Pickle,
          [ Product.createKeg "Beer" 200
            Product.create "Mill" (Item.create "Wheat Flour" 50) ] ))

    createScythe
      "Amaranth"
      (SeedSell 35)
      PierreAndJoja
      [ fall ]
      [ 1; 2; 2; 2 ]
      (SellPrice 150)
      Vegetable

    create
      "Artichoke"
      (SeedSell 15)
      Pierre
      [ fall ]
      [ 2; 2; 1; 2; 1 ]
      (SellPrice 160)
      Vegetable

    create
      "Beet"
      (SeedSell 10)
      Oasis
      [ fall ]
      [ 1; 1; 2; 2 ]
      (SellPrice 100)
      (CreateAndAlso (Vegetable, [ Product.createRatio 1 "Mill" (Item.create "Sugar" 50) 3.0 ] ))

    create
      "Bok Choy"
      (SeedSell 25)
      PierreAndJoja
      [ fall ]
      [ 1; 1; 1; 1 ]
      (SellPrice 80)
      Vegetable

    createRegrow
      (withExtraCrops 2 0.1)
      "Cranberries"
      (SeedSell 60)
      (JojaWith <| Price.create 240 "Pierre")
      [ fall ]
      [ 1; 2; 1; 1; 2 ]
      5
      (SellPrice 75)
      Fruit

    createRegrow
      (withExtraChance 0.002)
      "Eggplant"
      (SeedSell 10)
      PierreAndJoja
      [ fall ]
      [ 1; 1; 1; 1; 1 ]
      5
      (SellPrice 60)
      Vegetable

    create
      "Fairy Rose"
      (Seed <| Item.create "Fairy Seeds" 100)
      PierreAndJoja
      [ fall ]
      [ 1; 4; 4; 3 ]
      (SellPrice 290)
      NoProduct

    createTrelis
      "Grape"
      (Seed <| Item.create "Grape Starter" 30)
      PierreAndJoja
      [ fall ]
      [ 1; 1; 2; 3; 3 ]
      3
      (SellPrice 30)
      Fruit

    createGiantCrop
      "Pumpkin"
      (SeedSell 50)
      PierreAndJoja
      [ fall ]
      [ 1; 2; 3; 4; 3 ]
      (SellPrice 320)
      Vegetable

    create
      "Sweet Gem Berry"
      (Seed <| Item.create "Rare Seed" 200)
      (PriceList [ Buy.create 1000 "Traveling Merchant" ] )
      [ fall ]
      [ 2; 4; 6; 6; 6 ]
      (CropItem <| Item.create "Sweet Gem Berry" 3000)
      NoProduct

    create
      "Yam"
      (SeedSell 30)
      PierreAndJoja
      [ fall ]
      [ 1; 3; 3; 3 ]
      (SellPrice 160)
      Vegetable

    createRegrowNoExtra
      "Ancient Fruit"
      (Seed <| Item.create "Ancient Seeds" 30)
      NoPrice
      [ spring; summer; fall ]
      [ 2; 7; 7; 7; 5 ]
      7
      (SellPrice 550)
      Fruit

    createBush
      "Tea"
      (Seed <| Item.create "Tea Sapling" 500)
      (PriceList [ Buy.create 100 "Crafting" ] )
      [ spring; summer; fall ]
      [ 10; 10 ]
      1
      22
      28
      (CropItem <| Item.createCrop "Tea Leaves" 50)
      (CreateAndAlso (Vegetable, [ Product.createKeg "Green Tea" 100 ] )) ]

let initialModel page =
  { Page =
      match page with
      | Some x -> x
      | None -> Home
    SidebarTab = Skills
    SidebarOpen = false

    Skills = skills |> toMapByKey Skill.which
    IgnoreProfessionRelationships = false

    RawMultipliers = rawMultipliers |> toMapByKey RawMultiplier.nameOf
    ProfessionMultipliers = professionMultipliers |> toMapByKey ProfessionMultiplier.nameOf

    BuySourceList = buySources |> List.map Source.nameOf
    BuySources = buySources |> toMapByKey Source.nameOf
    PriceMultipliers = priceMultipliers |> toMapByKey PriceMultiplier.nameOf
    PriceMultiplierList = priceMultipliers |> List.map PriceMultiplier.nameOf

    Processors = processors |> toMapByKey Processor.nameOf
    ProcessorList = processors |> List.map Processor.nameOf

    SellRawCrop = true
    SellSeedsFromSeedMaker = true

    BuySeeds = true
    SeedMakerReplant = true
    HarvestReplant = true

    Crops = crops |> toMapByKey Crop.nameOf
    CropSort = Seasons
    CropSortAscending = true
    CropList = crops |> List.map Crop.nameOf
    SelectedCrop = None
    CropTab = Growth
    ShowOutOfSeasonCrops = false
    ShowInvalidCrops = true
    AllowCropClearings = false
    AllowCrossSeason = true
    AccountForReplant = true

    Fertilizers = fertilizers |> toMapByKey Fertilizer.nameOf
    FertilizerSort = Speed
    FertilizerSortAscending = true
    FertilizerList = fertilizers |> List.map Fertilizer.nameOf
    SelectedFertilizer = None
    AccountForFertilizerCost = true
    FertilizerLossProb = 0.1

    StartDate =
      { Season = spring
        Day = 1 }
    EndDate =
      { Season = fall
        Day = 28 }

    CompareMode = Combos
    ShowUnprofitableCombos = false
    SelectedCombo = None

    SelectedCompareCrop = None
    CompareCropsUsingFertilizer = None

    SelectedCompareFertilizer = None
    CompareFertilizersUsingCrop = Crop.nameOf crops.Head

    StartingFertilizer = None

    ProfitMode = NetProfit
    Greenhouse = false

    ShowTips = true
    SaveSettings = false
    SkillLevelPolicy = Enforce

    SpecialCharm = false
    LuckBuff = 0

    TrelisPenalty = false
    TrelisPercentage = 0.666
    AllowTrelisPair = false

    BaseGiantCropChance = 0.01
    GiantCropChecksPerTile = 8.0

    SeedMakerProb = 0.975
    AncientSeedProb = 0.005

    QualityProducts = false
    QualitySeedMaker = false
    QualitySeedMakerAmounts =
      Map.ofList
        [ Normal, 2.0
          Silver, 3.0
          Gold, 4.0
          Iridium, 5.0 ] },
    []