module StardewValleyStonks.AppData

open Fable.Core.JsInterop

//todo: documentation for types and construction
//create json file based off this


let skills =
  let create = Skill.create

  [ create Farming "Tiller" "Artisan" (Some "Agriculturist")
    create Foraging "Gatherer" "Botanist" None ]
  |> Map.ofValues Skill.which


let professionMultipliers =
  let farming = skills.[Farming]

  [ Farming, farming.Lvl5Profession, 1.1
    Farming, farming.Lvl10ProfessionA, 1.4
    Farming, farming.Lvl10ProfessionB.Value, 0.1 ]


let rawMultipliers, selectedRawMultipliers =
  let create = tuple2 >>| RawMultiplier

  let list =
    [ create "Irrigated" 0.25, false
      create "Bear's Knowledge" 3.0, false ]

  list |> List.map fst,
  list |> List.filter snd |> List.map fst |> set

let multiplierByName =
  (professionMultipliers |> Seq.map Profession |> set)
  |> Set.union (rawMultipliers |> Seq.map Raw |> set)
  |> Map.ofValues Multiplier.name


let priceMultipliers, selectedPriceMultipliers, priceMultiplierByName =
  let list =
    [ PriceMultiplier("Joja Membership", 1.25, false), false ]

  let all = list |> List.map fst

  all,
  list |> List.filter snd |> List.map fst |> set,
  all |> Map.ofValues PriceMultiplier.name



let fertilizers =
  let create = Fertilizer.create
  let price = Price.create

  [ create
      "Basic Fertilizer"
      1
      0.0
      [ price !!"Pierre" 100 ]

    create
      "Quality Fertilizer"
      2
      0.0
      [ price !!"Pierre" 150 ]

    create
      "Deluxe Fertilizer"
      3
      0.0
      [ price !!"Crafting" 500 ]

    create
      "Speed-Gro"
      0
      0.1
      [ price !!"Pierre" 100 ]

    create
      "Deluxe Speed-Gro"
      0
      0.25
      [ price !!"Pierre" 150
        price !!"Oasis" 80 ]
    
    create
      "Hyper Speed-Gro"
      0
      0.33
      [ price !!"Crafting" 500 ] ]


let seedMakerProb = 0.975
let seedMakerAmount = 2

let processors =
  let farmLvl = tuple2 Farming >> SkillUnlock >> Some
  let create = Processor.create

  [ create "Preserves Jar" true (farmLvl 4)
    create "Keg" true (farmLvl 8)
    Processor.createWith
      (Some <| QualityIndependent (seedMakerProb * float seedMakerAmount))
      (Always false)
      "Seed Maker"
      (farmLvl 9)
    create "Oil Maker" true (farmLvl 9)
    create "Mill" false None ]



let crops =
  let price = Price.create
  let seedPrice = (*) 2

  let pierreAndJojaPrices seedSell =
    let pierreValue = seedPrice seedSell
    [ price !!"Pierre" pierreValue
      Price.createMultiplier priceMultiplierByName.["Joja Membership"] !!"Joja" pierreValue ]

  let pierreAndJoja = pierreAndJojaPrices >> Crop.createPrices

  let pierrePrice, oasis =
    let singlePrice source = seedPrice >> price !!source >> Seq.singleton >> Crop.createPrices
    singlePrice "Pierre", singlePrice "Oasis"
  
  let prices list _ = Crop.createPrices list
  let noPrices _ = None


  let item = Item.create

  let artisanItem = Item.createMultiplier multiplierByName.["Artisan"]
  let product = Product.create
  let ratioProduct = Product.createRatio
  let kegProduct = artisanItem >>| product !&"Keg"
  let jarProduct = artisanItem >>| product !&"Preserves Jar"
  let jarProductPrice basePrice = basePrice * 2 + 50

  let seedMakerProduct _ = product !&"Seed Maker"

  let wineProduct cropItem _ =
    kegProduct (Item.name cropItem + " Wine") (cropItem.BasePrice * 3)

  let juiceProduct cropItem _ =
    kegProduct (Item.name cropItem + "Juice") (cropItem.BasePrice |> intMulti 2.25)

  let jamProduct cropItem _ =
    jarProduct (Item.name cropItem + " Jam") (jarProductPrice cropItem.BasePrice)

  let pickleProduct cropItem _ =
    jarProduct ("Pickeled " + Item.name cropItem) (jarProductPrice cropItem.BasePrice)
  
  let productsPreset list cropItem seed =
    list |> List.map (fun f -> f cropItem seed)

  let products list _ _ = list
  let noProducts _ _ = []

  let productsWith list others cropItem seed =
    [ yield! productsPreset list cropItem seed
      yield! others ]

  let fruitOnlyProducts =
    [ wineProduct
      jamProduct ]
  let fruitOnly = productsPreset fruitOnlyProducts
  let fruitProducts = seedMakerProduct :: fruitOnlyProducts
  let fruit = productsPreset fruitProducts

  let vegeOnlyProducts =
    [ juiceProduct
      pickleProduct ]
  let vegeProducts = seedMakerProduct :: vegeOnlyProducts
  let vege = productsPreset vegeProducts
  let pickleProducts =
    [ pickleProduct
      seedMakerProduct ]

  let seedMakerProducts = [ seedMakerProduct ]
  let seedMaker = productsPreset seedMakerProducts

  let oil = [ item "Oil" 100 |> product !!"Oil Maker" ]

  let auto seedPrice name = name, item (name + " Seeds") seedPrice
  let tillerItem = Item.createMultiplier multiplierByName.["Tiller"]
  let cropItem name price _ = tillerItem name price
  let cropSell = flip tillerItem

  let oneSeason season = season, season
  let springCrop = oneSeason Season.Spring
  let summerCrop = oneSeason Season.Summer
  let fallCrop = oneSeason Season.Fall

  let agri = set [ multiplierByName.["Agriculturist"] ]

  let create = Crop.createRegular agri
  let createExtra = Crop.createRegularExtra agri
  //let createGrowth = Crop.createRegularAmount agri
  let giant = Crop.createGiant agri
  let scythe = Crop.createScythe agri
  let regrow = Crop.createRegrow agri
  let regrowAmount = Crop.createRegrowAmount agri
  let regrowYield = Crop.createRegrowYield agri
  let regrowExtra = Crop.createRegrowExtra agri
  let trelis = Crop.createTrelis agri
  let trelisAmount = Crop.createTrelisAmount agri
  let multi = Crop.createMulti agri
  let bush = Crop.createBush
  let forage = Crop.createForage agri

  [ create
      ("Blue Jazz", item "Jazz Seeds" 15)
      springCrop
      [ 1; 2; 2; 2 ]
      pierreAndJoja
      (cropSell 50)
      seedMaker

    giant
      ("Cauliflower" |> auto 40)
      springCrop
      [ 1; 2; 4; 4; 1 ]
      pierreAndJoja
      (cropSell 175)
      vege

    let coffeeBean = item "Coffee Bean" 15
    regrowYield
      (4, 0.02)
      ("Coffee", coffeeBean)
      (Season.Spring, Season.Summer)
      [ 1; 2; 2; 3; 2 ]
      2
      (prices [ price !!"Traveling Cart" 2500 ] )
      (fun _ -> coffeeBean)
      (products [ item "Coffee" 150 |> ratioProduct !!"Keg" 5 1 ] )
      
    create
      ("Garlic" |> auto 20)
      springCrop
      [ 1; 1; 1; 1 ]
      pierrePrice
      (cropSell 60)
      vege
      
    trelis
      ("Green Bean", item "Bean Starter" 30)
      springCrop
      [ 1; 1; 1; 3; 4 ]
      3
      pierreAndJoja
      (cropSell 40)
      vege
    
    scythe
      ("Kale" |> auto 35)
      springCrop
      [ 1; 2; 2; 1 ]
      pierreAndJoja
      (cropSell 110)
      vege
      
    create
      ("Parsnip" |> auto 10)
      springCrop
      [ 1; 1; 1; 1 ]
      pierreAndJoja
      (cropSell 35)
      vege

    createExtra
      0.2
      ("Potato" |> auto 25)
      springCrop
      [ 1; 1; 1; 2; 1 ]
      pierreAndJoja
      (cropSell 80)
      vege

    create
      ("Rhubarb" |> auto 50)
      springCrop
      [ 2; 2; 2; 3; 4]
      oasis
      (cropSell 220)
      fruit
      
    regrowExtra
      0.02
      ("Strawberry" |> auto 0)
      springCrop
      [ 1; 1; 2; 2; 2 ]
      4
      (prices [ price !!"Pierre" 100 ] )
      (cropSell 120)
      fruit
    
    create
      ("Tulip", item "Tulip Bulb" 10)
      springCrop
      [ 1; 1; 2; 2 ]
      pierreAndJoja
      (cropSell 30)
      seedMaker
      
    Crop.createRegularWith
      (agri.Add multiplierByName.["Irrigated"])
      (ExtraChance 0.1)
      ("Rice", item "Rice Shoot" 20)
      springCrop
      [ 1; 2; 2; 3 ]
      pierrePrice
      (cropItem "Unmilled Rice" 30)
      (productsWith vegeProducts [ item "Rice" 100 |> product !!"Mill" ] )
    
    regrowYield
      (3, 0.02)
      ("Blueberry" |> auto 40)
      summerCrop
      [ 1; 3; 3; 4; 2 ]
      4
      pierrePrice
      (cropSell 50)
      fruit
      
    regrow
      ("Corn" |> auto 75)
      (Season.Summer, Season.Fall)
      [ 2; 3; 3; 3; 3 ]
      4
      pierreAndJoja
      (cropSell 50)
      (productsWith vegeProducts oil)
      
    trelis
      ("Hops", item "Hops Starter" 30)
      summerCrop
      [ 1; 1; 2; 3; 4 ]
      1
      pierreAndJoja
      (cropSell 25)
      (productsWith pickleProducts [ kegProduct "Pale Ale" 300 ] )
      
    regrowExtra
      0.03
      ("Hot Pepper", item "Pepper Seeds" 20)
      summerCrop
      [ 1; 1; 1; 1; 1 ]
      3
      pierreAndJoja
      (cropSell 40)
      fruit
      
    giant
      ("Melon" |> auto 40)
      summerCrop
      [ 1; 2; 3; 3; 3 ]
      pierreAndJoja
      (cropSell 250)
      fruit
      
    create
      ("Poppy" |> auto 50)
      summerCrop
      [ 1; 2; 2; 2 ]
      pierreAndJoja
      (cropSell 140)
      seedMaker
      
    create
      ("Red Cabbage" |> auto 50)
      summerCrop
      [ 2; 1; 2; 2; 2 ]
      pierrePrice
      (cropSell 260)
      vege
      
    create
      ("Starfruit" |> auto 200)
      summerCrop
      [ 2; 3; 2; 3; 3 ]
      oasis
      (cropSell 750)
      fruit
      
    create
      ("Summer Spangle", item "Spangle Seeds" 25)
      summerCrop
      [ 1; 2; 3; 2 ]
      pierreAndJoja
      (cropSell 90)
      seedMaker
      
    let sunflowerSeeds = item "Sunflower Seeds" 20
    multi
      ("Sunflower", sunflowerSeeds)
      (Season.Summer, Season.Fall)
      [ 1; 2; 3; 2 ]
      (prices
        [ price !!"Pierre" 200
          price !!"Joja" 125 ] )
      (cropSell 80)
      (productsWith seedMakerProducts oil)
      sunflowerSeeds
      (products oil)
      1.0
      
    regrowExtra
      0.05
      ("Tomato" |> auto 25)
      summerCrop
      [ 2; 2; 2; 2; 3 ]
      4
      pierreAndJoja
      (cropSell 60)
      fruit
    
    multi
      ("Wheat" |> auto 5)
      (Season.Summer, Season.Fall)
      [ 1; 1; 1; 1 ]
      pierreAndJoja
      (cropSell 25)
      (productsWith
        pickleProducts
        [ kegProduct "Beer" 200
          item "Wheat Flour" 50 |> product !!"Mill" ] )
      (item "Hay" 50)
      noProducts
      0.4

    scythe
      ("Amaranth" |> auto 35)
      fallCrop
      [ 1; 2; 2; 2 ]
      pierreAndJoja
      (cropSell 150)
      vege
      
    create
      ("Artichoke" |> auto 15)
      fallCrop
      [ 2; 2; 1; 2; 1 ]
      pierrePrice
      (cropSell 160)
      vege
      
    create
      ("Beet" |> auto 10)
      fallCrop
      [ 1; 1; 2; 2 ]
      oasis
      (cropSell 100)
      (productsWith vegeProducts [ item "Sugar" 50 |> ratioProduct !!"Mill" 1 3 ] )
      
    create
      ("Bok Choy" |> auto 25)
      fallCrop
      [ 1; 1; 1; 1 ]
      pierreAndJoja
      (cropSell 80)
      vege
      
    regrowYield
      (2, 0.1)
      ("Cranberries" |> auto 60)
      fallCrop
      [ 1; 2; 1; 1; 2 ]
      5
      (prices <| pierreAndJojaPrices 240)
      (cropSell 75)
      fruit
      
    regrowExtra
      0.002
      ("Eggplant" |> auto 10)
      fallCrop
      [ 1; 1; 1; 1; 1 ]
      5
      pierreAndJoja
      (cropSell 60)
      vege
      
    create
      ("Fairy Rose", item "Fairy Seeds" 100)
      fallCrop
      [ 1; 4; 4; 3 ]
      pierreAndJoja
      (cropSell 290)
      seedMaker
      
    trelis
      ("Grape", item "Grape Starter" 30)
      fallCrop
      [ 1; 1; 2; 3; 3 ]
      3
      pierreAndJoja
      (cropSell 30)
      fruit
      
    giant
      ("Pumpkin" |> auto 50)
      fallCrop
      [ 1; 2; 3; 4; 3 ]
      pierreAndJoja
      (cropSell 320)
      vege
      
    Crop.createRegularWith
      Set.empty
      DoubleChance
      ("Sweet Gem Berry", item "Rare Seed" 200)
      fallCrop
      [ 2; 4; 6; 6; 6 ]
      (prices [ price !!"Traveling Cart" 1000 ] )
      (flip item 3000)
      seedMaker
      
    create
      ("Yam" |> auto 30)
      fallCrop
      [ 1; 3; 3; 3 ]
      pierreAndJoja
      (cropSell 160)
      vege
      
    regrow
      ("Ancient Fruit", item "Ancient Seeds" 30)
      (Season.Spring, Season.Fall)
      [ 2; 7; 7; 7; 5 ]
      7
      noPrices
      (cropSell 550)
      (productsPreset
        [ fun _ -> Product.createWith (Some <| Additional 0.05) (Some !&"Seed Maker")
          wineProduct
          jamProduct ] )
      
    bush
      ("Tea", item "Tea Sapling" 500)
      (Season.Spring, Season.Fall)
      [ 10; 10 ]
      1
      22
      28
      (prices [ price !!"Crafting" 100 ] )
      (cropItem "Tea Leaves" 50)
      (productsWith
        [ pickleProduct ]
        [ kegProduct "Green Tea" 100 ] )
        
    forage
      Season.Spring
      35
      1
      [ price !!"Traveling Cart" 500 ]
      [ item "Daffodil" 30, noProducts
        item "Dandelion" 40, noProducts
        item "Leek" 60, noProducts
        item "Wild Horseradish" 50, seedMaker ]
        
    forage
      Season.Summer
      55
      4
      [ price !!"Traveling Cart" 500 ]
      [ item "Grape" 80, productsWith fruitOnlyProducts [ item "Grape Starter" 30 |> product !&"Seed Maker" ]
        item "Sweet Pea" 50, noProducts
        item "Spice Berry" 80, fruit ]
    
    forage
      Season.Fall
      45
      6
      [ price !!"Traveling Cart" 500 ]
      [ Item.createMultiplier multiplierByName.["Bear's Knowledge"] "Blackberry" 20, fruitOnly
        item "Common Mushroom" 40, seedMaker
        item "Hazelnut" 90, noProducts
        item "Wild Plum" 80, fruitOnly ]
        
    forage
      Season.Winter
      40
      7
      [ price !!"Traveling Cart" 500 ]
      [ item "Crocus" 60, noProducts //tiller multiplier??
        item "Crystal Fruit" 150, fruitOnly
        item "Snow Yam" 100, noProducts
        item "Winter Root" 70, seedMaker ] ]


let sortByCountDescending projection items =
  let count = Cache<_,_>()
  items |> Seq.iter (fun item ->
    projection item |> Seq.iter (fun key ->
      if count.ContainsKey key
      then count.[key] <- count.[key] + 1
      else count.Add(key, 1)))
  count.Keys
  |> Seq.sortByDescending count.Find

let initialModel page =
  let baseGiantProb = 0.01
  let giantYield = 2.0
  let giantChecksPerTile = 8.0
  let specialCharm = false
  let luckBuff = 0

  let ui =
    { Page = defaultValue Home page
      SettingsTab = Skills
      CropTableTab = SelectCrops
      CropTab = Growth
      FertilizerTableTab = SelectFertilizers
      IgnoreProfessionRelationships = false
      CropSort = Seasons
      CropSortAscending = true
      // InSeasonFilter = false
      // ProfitableFilter = false
      // SeasonsFilter = set Season.all
      // CrossSeasonFilter = false
      // CropTypeFilter = None
      FertilizerSort = Speed
      FertilizerSortAscending = false
      //ShowUnprofitable = false
      //ShowInvalid: bool
      SelectedPair = None }

  let data =
    { Skills = skills

      PriceMultipliers = priceMultipliers
      SelectedPriceMultipliers = selectedPriceMultipliers

      ProfessionMultipliers = professionMultipliers
      RawMultipliers = rawMultipliers
      SelectedRawMultipliers = selectedRawMultipliers
      ProductMultipliers =
        crops
        |> Seq.collect Crop.rawCrops
        |> Seq.collect (RawCrop.products >> Map.values)
        |> Seq.choose (Product.item >> Item.multiplier)
        |> set

      Processors = processors |> Map.ofValues Processor.nameKey
      ProductReplants =
        None ::
          (crops
          |> sortByCountDescending (Crop.rawCrops >> Seq.choose RawCrop.replant >> Seq.choose Product.processor)
          |> Seq.map Some
          |> Seq.toList)
      ProductProcessors =
        None ::
          (crops
            |> sortByCountDescending (Crop.rawCrops >> Seq.collect (RawCrop.products >> Map.keys >> Seq.choose id))
            |> Seq.map Some
            |> Seq.toList)

      Crops = crops |> Map.ofValues Crop.nameKey
      SeedSources = crops |> Seq.choose Crop.seedPrices |> sortByCountDescending (Prices.from >> Map.keys)
      BaseGiantProb = baseGiantProb
      GiantYield = giantYield
      GiantChecksPerTile = giantChecksPerTile
      SpecialCharm = specialCharm
      LuckBuff = luckBuff

      NoGiantProb = CropAmount.noGiantCropProb baseGiantProb giantChecksPerTile
      DoubleCropProb = CropAmount.doubleCropProb luckBuff specialCharm

      CropSelection = crops |> Seq.map Crop.nameKey |> set
      AccountForReplant = true
      //AllowCropClearings: bool
      //AllowCrossSeason: bool

      Fertilizers = fertilizers |> Map.ofValues Fertilizer.nameKey
      FertilizerSources = fertilizers |> sortByCountDescending (Fertilizer.prices >> Prices.from >> Map.keys)

      FertilizerSelection = fertilizers |> Seq.map Fertilizer.nameKey |> set
      NoneFertilizerSelected = true
      AccountForFertilizerCost = true
      AccountForReplacementFertilizer = true
      FertilizerLossProb = 0.1

      StartDate = Date(Season.Spring, 1)
      EndDate = Date(Season.Fall, 28)
      Greenhouse = false
      //Island = false

      CompareMode = NetProfit

      StartingFertilizer = None

      SkillLevelPolicy = Enforce
      //TrelisPenalty = false
      //TrelisPercentage = 0.666
      //AllowTrelisPair = false

      QualityProducts = false
      QualityProcessors = processors |> Seq.filter Processor.qualityProcessor |> Seq.map Processor.nameKey |> Seq.toArray
      QualitySeedMaker = false
      QualitySeedMakerAmounts = [| 2; 3; 4; 5 |]
      SeedMakerSeedProb = 0.975
      SeedMakerSeedAmount = 2
      
      Combos = Lazy.CreateFromValue Array.empty }

  AppCache.newProfitCalc <| DataModel.rawCropUnitProfit data
  AppCache.newLowestCalc <| DataModel.lowestPrice data

  (ui, data), []