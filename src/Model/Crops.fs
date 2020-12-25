namespace StardewValleyStonks

open FSharp.Data.Adaptive
open Adaptive
open Util

type CropAmount =
  | FarmingAmounts
  | Single of Quality * float
  | Giant
  | DoubleChance
  | ExtraChance of float
  //| MoreYield of int
  | DoubleAndExtra of float
  //| DoubleAndYield of int
  | DoubleYieldExtra of int * float
  | ForagingAmounts

type CropItem =
  { Item: Item
    Amount: CropAmount
    Products: Product list
    BestProfits: Map<Quality, float option aval>
    BestProducts: Map<Quality, Product alist>
    OneProduct: bool aval }

module CropItem =
  let item cropItem = cropItem.Item
  let name = item >> Item.name
  let amount cropItem = cropItem.Amount
  let products cropItem = cropItem.Products
  let oneProduct cropItem = cropItem.OneProduct
  let bestProfits cropItem = cropItem.BestProfits
  let bestProducts cropItem = cropItem.BestProducts

  let createWith processorActive item amount products =
    let prods = (Product.createRawWith processorActive item) :: products
    let bestProfits, bestProducts = Product.bestProfitAndProducts prods
    { Item = item
      Amount = amount
      Products = prods
      BestProfits = bestProfits
      BestProducts = Map.empty //bestProducts
      OneProduct = !@true } //w!%Option.isSome bestProfits.[Normal] }



type SeedPrice =
  { Prices: Map<string, Price>
    BestPrice: int option aval
    BestSources: string alist
    Override: Override cval
    Selected: bool aval }

module SeedPrice =
  let buySeeds = cval true

  let create = function
    | [] -> None
    | list ->
        let bestPrice, bestSources = Price.bestPriceAndSources list
        let o = cval None
        Some
          { Prices = mapOfValues Price.source list
            BestPrice = bestPrice
            BestSources = bestSources
            Override = o
            Selected = aSelectedOverride o buySeeds .&& !%Option.isSome bestPrice }

type ProductReplant =
  { CropItem: CropItem
    Product: Product
    Override: Override cval
    Selected: bool aval }

type Replant =
  | BuySeeds of SeedPrice
  | ProductReplant of ProductReplant

module Replant =
  let selected = function
    | BuySeeds b -> b.Selected
    | ProductReplant p -> p.Selected
  
  let createWith replantSelected item product =
    let over = cval None
    ProductReplant
      { CropItem = item
        Product = product
        Override = over
        Selected =
          let processor = Product.processor product
          let selected = replantSelected processor |> aSelectedOverride over
          match processor with
          | Some p -> Processor.unlocked p .&& selected
          | None -> selected }



type CropBase =
  { Name: string
    Seasons: Set<Season>
    InSeason: bool aval
    GrowthStages: int list
    TotalGrowthTime: int
    GrowthMultipliers: Multiplier list
    GrowthMultiplier: float aval
    GrowthPeriods: int alist
    GrowthTime: int aval
    Harvests: int aval
    Seed: Item
    SeedPrice: SeedPrice option
    CropItems: CropItem list
    OneProduct: bool aval
    Replants: Replant list
    OneReplant: bool aval
    Valid: bool aval }

type Crop =
  | RegularCrop of CropBase
  | RegrowCrop of
      {| Base: CropBase
         RegrowTime: int
         Trelis: bool
         IndoorsOnly: bool |}
  | GiantCrop of CropBase
  | ForageCrop of
      {| Base: CropBase
         SellForageSeeds: Override cval
         ForageSeedsReplant: Override cval |}
  | Bush of
      {| Base: CropBase
         RegrowTime: int
         HarvestStartDay: int
         HarvestEndDay: int |}
// recheck every function for forage crop


type CropSort =
  | CropName
  | Seasons
  | TotalGrowthTime
  | RegrowTime
  | SeedPrice

module Crop =
  let private baseGiantCropChance = 0.01
  let giantCropChecksPerTile = cval 8.0
  let private seedMakerProb = 0.975
  let private seedMakerAmount = 2
  let private qualityAmount amount =
    adaptive {
      let! quality = Mod.qualitySeedMaker
      if quality
      then return! aFloat amount .* !@seedMakerProb
      else return float seedMakerAmount * seedMakerProb }

  let private seedMakerAmounts = Mod.qualitySeedMakerAmounts |> mapUsingValues qualityAmount

  let private nameBase crop = crop.Name
  let private seasonsBase crop = crop.Seasons
  let private growthStagesBase crop = crop.GrowthStages
  let private totalGrowthTimeBase crop = crop.TotalGrowthTime
  let private growthMultipliersBase crop = crop.GrowthMultipliers
  let private growthMultiplierBase crop = crop.GrowthMultiplier
  let private growthPeriodsBase crop = crop.GrowthPeriods
  let private growthTimeBase crop = crop.GrowthTime
  let private harvestsBase crop = crop.Harvests
  let private seedPriceBase crop = crop.SeedPrice
  let private seedBase crop = crop.Seed
  let private cropItemsBase crop = crop.CropItems
  let private replantsBase crop = crop.Replants

  let cropBase = function
    | RegularCrop c -> c
    | RegrowCrop r -> r.Base
    | GiantCrop g -> g
    | ForageCrop f -> f.Base
    | Bush b -> b.Base

  let name = cropBase >> nameBase
  let seasons = cropBase >> seasonsBase
  let growthStages = cropBase >> growthStagesBase
  let totalGrowthTime = cropBase >> totalGrowthTimeBase
  let growthMultipliers = cropBase >> growthMultipliersBase
  let growthMultiplier = cropBase >> growthMultiplierBase
  let growthPeriods = cropBase >> growthPeriodsBase
  let growthTime = cropBase >> growthTimeBase
  let harvests = cropBase >> harvestsBase
  let seedPrice = cropBase >> seedPriceBase
  let seed = cropBase >> seedBase
  let cropItems = cropBase >> cropItemsBase
  let replants = cropBase >> replantsBase

//   let description = function
//     | RegularCrop _ -> None
//     | RegrowCrop r -> if r.Trelis then Some "Trelis" else None
//     | GiantCrop _ -> Some "Giant Crop"
//     | ForageCrop _ -> Some "Forage Crop"
//     | Bush _ -> Some "Bush"

  let regrowTime = function
    | RegrowCrop r -> Some r.RegrowTime
    | Bush t -> Some t.RegrowTime
    | _ -> None

  // Stardew Valley passes speed around as a float32. This then gets coverted to a float, introducing some small error.
  // And this small error is sometimes enough to give an extra day of reduction since the value is later passed into the ceiling function.
  // Some case studies:
  //       Case 1: 0.2f
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
  //      Case 2: 0.25f (all numbers 1/(2^n))
  //  float32        -> float
  //  0.25           0.25
  //  ...   Same    ...
  //
  //      Case 3: 0.35f
  //  float32               -> float
  //  0.35                  0.349999994039536
  //  * 20.0 growth days    * 20.0 growth days
  //  = 7.0                 = 6.99999988079071
  //  |> ceil |> int        |> ceil |> int
  //  = 7                   = 7 (wouldn't be equal if floor was used instead of ceil)

  // But since (x: float) |> float32 |> float doesn't actually do a conversion (thanks javascript...), then here we use typed arrays to achieve the same effect.
  let private toF32 = Fable.Core.JS.Constructors.Float32Array.Create 1
  let private toF32AndBack (value: float) =
    toF32.[0] <- float32 value
    float toF32.[0]

  let private daysReduced total = AVal.map (fun speed ->
    (toF32AndBack speed) * (float total) |> ceil |> int)

  let private growthTimeCalc total stages = daysReduced total >> AVal.map (fun reductionDays ->
    if reductionDays = 0 then
      total
    else
      let growthStages = List.toArray stages
      let mutable reduceDays = reductionDays
      let mutable daysReduced = 0
      let mutable traverses = 0
      while daysReduced < reduceDays && traverses < 3 do
        if growthStages.[0] > 1 then
          growthStages.[0] <- growthStages.[0] - 1
          daysReduced <- daysReduced + 1
        let mutable stage = 1
        while daysReduced < reduceDays && stage < growthStages.Length do
          if growthStages.[stage] > 0 then
            growthStages.[stage] <- growthStages.[stage] - 1
            daysReduced <- daysReduced + 1
          else
            // A reduction day was wasted reducing a stage below 0 days. Potentially possible; the game code does not prevent this from happening.
            reduceDays <- reduceDays - 1
          stage <- stage + 1
        traverses <- traverses + 1
      total - daysReduced)

  let growthTimeWith fertilizer = function
    | Bush b -> b.Base.GrowthTime
    | crop ->
        let speed = Fertilizer.speedOption fertilizer
        if speed = 0.0 then
          growthTime crop
        else
          let total = totalGrowthTime crop
          growthTimeCalc total (growthStages crop) (growthMultiplier crop .+ !@speed)

  let private consecutiveGrowthPeriods = memoize <| (fun (seasons: _ Set) ->
    AVal.map2 (fun startDate endDate ->
      if startDate.Season = endDate.Season && startDate.Day < endDate.Day then
        if seasons.Contains startDate.Season then
          [ endDate.Day - startDate.Day + 1 ]
        else
          []
      else
        let mutable growthPeriods = []
        let mutable days =
          if seasons.Contains startDate.Season
          then 29 - startDate.Day
          else 0
        
        let mutable season = Season.next startDate.Season
        while season <> endDate.Season do
          if seasons.Contains season then
            days <- days + 28
          elif days > 1 then
            growthPeriods <- (days - 1) :: growthPeriods
            days <- 0
          season <- Season.next season

        if seasons.Contains endDate.Season then
          days <- days + endDate.Day
          growthPeriods <- (days - 1) :: growthPeriods
        elif days > 1 then
          growthPeriods <- (days - 1) :: growthPeriods
        
        growthPeriods)
      Date.startDate
      Date.endDate
    |> AList.ofAVal)

  let private regularHarvests (growthTime: int aval) = AList.sumByA (fun growthPeriod ->
    !@growthPeriod ./ growthTime)

  let private regrowHarvest regrowTime growthPeriod = AVal.map (fun growthTime ->
    if growthPeriod < growthTime
    then 0
    else (growthPeriod - growthTime) / regrowTime + 1)

  let private regrowHarvests regrowTime growthTime = AList.sumByA (fun period -> regrowHarvest regrowTime period growthTime)

  let private bushHarvests (seasons: _ Set) regrowTime harvestStartDay harvestEndDay =
    AVal.map3 (fun startDate endDate growthTime ->
      if growthTime > Date.daysBetween startDate endDate then // check for infinite mode
        0
      else
        let matureDate = startDate + growthTime
        let mutable harvests =
          if seasons.Contains matureDate.Season && matureDate.Day <= harvestEndDay
          then (harvestEndDay - (max matureDate.Day harvestStartDay) + 1) / regrowTime
          else 0
        
        let mutable season = Season.next matureDate.Season
        while season <> endDate.Season do
          if seasons.Contains season then
            harvests <- harvests + (harvestEndDay - harvestStartDay + 1) / regrowTime
          season <- Season.next season

        if seasons.Contains endDate.Season && harvestStartDay <= endDate.Day then
          harvests <- harvests + ((min endDate.Day harvestEndDay) - harvestStartDay + 1) / regrowTime

        harvests)
      Date.startDate
      Date.endDate

  let private harvestsCalc growthTime = function
    | RegrowCrop r -> r.Base.GrowthPeriods |> regrowHarvests r.RegrowTime growthTime
    | Bush b -> bushHarvests b.Base.Seasons b.RegrowTime b.HarvestStartDay b.HarvestEndDay growthTime
    | crop ->
        growthPeriods crop |> regularHarvests growthTime

  let harvestsWith fertilizer growthTime = function
    | Bush b -> b.Base.Harvests
    | crop ->
        if Fertilizer.speedOption fertilizer = 0.0
        then harvests crop
        else harvestsCalc growthTime crop

  let doubleCropProb =
    let term1 = aFloat Settings.luckBuff ./ !@1500.0
    adaptive {
      let! charm = Settings.specialCharm
      if charm
      then return! term1 .+ !@(0.025 / 1200.0)
      else return! term1 }

  let noGiantCropProb = !@(1.0 - baseGiantCropChance) .** giantCropChecksPerTile
  let giantCropProb = !@1.0 .- noGiantCropProb
  let giantCrops = giantCropProb .* !@2.0


  let inline private track x = trackUsed (fun () -> cval true) x
  let private sourceSelected, usedSources = track Sources.all
  let private processorSelected, usedProcessors = track Processors.allOption
  let private replantSelected, usedReplants = track Processors.allOption

  let private rawCrop name price = (fun _ -> Item.createCrop name price)
  let private cropSell = flip Item.createCrop

  let private auto seedPrice name = name, Item.create (name + " Seeds") seedPrice
  let private item = Item.create

  type private CreatePrices =
    | Pierre
    | JojaWith of PierrePrice: PriceBase
    | PierreAndJoja
    | Oasis
    | PriceList of Price list
    | NoPrice

  let private createSeedPrice =
    let jojaRelative = Price.createRelativeWith sourceSelected "Joja" PriceMultipliers.jojaMembership
    let buy = Price.createSeedWith sourceSelected
    let price = Price.createBaseSeedWith sourceSelected
    let create seedsellPrice = function
      | Pierre -> [ buy seedsellPrice "Pierre" ]
      | JojaWith pierrePrice ->
          [ BuyPrice pierrePrice
            jojaRelative pierrePrice ]
      | PierreAndJoja ->
          let pierrePrice = price seedsellPrice "Pierre"
          [ BuyPrice pierrePrice
            jojaRelative pierrePrice ]
      | Oasis -> [ buy seedsellPrice "Oasis" ]
      | PriceList prices -> prices
      | NoPrice -> []
    (create >>| SeedPrice.create)

  let priceBase = Price.createBaseWith sourceSelected
  let price = Price.createWith sourceSelected


  type private KegProduct =
    | Wine
    | Juice

  type private JarProduct =
    | Jam
    | Pickle

  type private CreateProducts =
    | Fruit
    | Vegetable
    | Keg of KegProduct
    | Jar of JarProduct
    | ProductList of Product list
    | CreateAndAlso of CreateProducts * Product list
    | NoProduct

  open Processors
  let private product = Product.createItemWith processorSelected
  let private kegProduct = Product.createArtisanWith processorSelected keg

  let private seedMakerWith = Product.createQualityWith processorSelected seedMaker

  let private createProducts =
    let aArtisan = Product.aCreateArtisanWith processorSelected
    let aKeg = aArtisan keg
    let aJar = aArtisan preservesJar

    let aKegProduct (cropItem: Item) = function
      | Wine -> aKeg (cropItem.Name + " Wine") (cropItem.BasePrice .* !@3)
      | Juice -> aKeg (cropItem.Name + " Juice") (cropItem.BasePrice |> aApply !@2.25)

    let jarPrice = AVal.map (fun basePrice -> basePrice * 2 + 50)
    let aJarProduct (cropItem: Item) = function
      | Jam -> aJar (cropItem.Name + " Jam") (jarPrice cropItem.BasePrice)
      | Pickle -> aJar ("Pickeled " + cropItem.Name) (jarPrice cropItem.BasePrice)

    let createSeedMaker = seedMakerWith seedMakerAmounts

    let rec create rawCrop = function
      | Fruit ->
          [ aKegProduct rawCrop Wine
            aJarProduct rawCrop Jam ]
      | Vegetable ->
          [ aKegProduct rawCrop Juice
            aJarProduct rawCrop Pickle ]
      | Keg product -> [ aKegProduct rawCrop product ]
      | Jar product -> [ aJarProduct rawCrop product ]
      | ProductList list -> list
      | CreateAndAlso (products, list) -> create rawCrop products @ list
      | NoProduct -> []
    
    let createWithSeedMaker rawCrop products = function
      | Some seed -> createSeedMaker seed :: create rawCrop products
      | None -> create rawCrop products

    createWithSeedMaker

  let private ratioProduct = Product.createRatioItemWith processorSelected

  let private fruit = Fruit, true
  let private vege = Vegetable, true
  let private withSeedMaker = flip tuple2 true
  let private productList = ProductList >> withSeedMaker
  let private createAndAlso = CreateAndAlso >> withSeedMaker
  let private noProduct = NoProduct, true

  let private cropItemWith = CropItem.createWith processorSelected
  let private createCropItem (name, seed) item amount (products, seedMaker) =
    let rawCrop = item name
    cropItemWith rawCrop amount (createProducts rawCrop products (if seedMaker then Some seed else None))

  let private createReplant = Replant.createWith replantSelected
  let private createReplants cropItems seedPrice (seed: Item) =
    let products =
        cropItems |> List.collect (fun cropItem ->
          cropItem.Products |> List.choose (fun product ->
            if Product.item product |> Item.name = seed.Name
            then Some <| createReplant cropItem product
            else None))
    match seedPrice with
    | Some data -> (BuySeeds data) :: products
    | None -> products

  let private createBaseWith harvestFun growthMultipliers (name, seed) (seasons: Season list) growthStages prices cropItems =
    let seedPrice = prices |> createSeedPrice seed.Price
    let total = List.sum growthStages
    let growthMultiplier = Multiplier.sum growthMultipliers
    let seasonSet, inSeason = Seasons.intersectsWithDateSpan seasons
    let growthPeriods = consecutiveGrowthPeriods seasonSet
    let growthTime = growthTimeCalc total growthStages growthMultiplier
    let harvests = harvestFun growthTime growthPeriods
    let replants = createReplants cropItems seedPrice seed
    let oneProduct = cropItems |> AList.ofList |> AList.existsA CropItem.oneProduct
    let oneReplant = replants |> AList.ofList |> AList.existsA Replant.selected
    { Name = name
      Seasons = seasonSet
      InSeason = inSeason
      GrowthStages = growthStages
      TotalGrowthTime = total
      GrowthMultipliers = growthMultipliers
      GrowthMultiplier = growthMultiplier
      GrowthPeriods = growthPeriods
      GrowthTime = growthTime
      Harvests = harvests
      Seed = seed
      SeedPrice = seedPrice
      CropItems = cropItems
      OneProduct = oneProduct
      Replants = replants
      OneReplant = oneReplant
      Valid =
        (adaptive {
          match! Fertilizers.fastestFert with
          | Some speed when speed > 0.0 ->
              let growthTime = growthTimeCalc total growthStages (!@speed .+ growthMultiplier)
              return! growthPeriods |> AList.existsA (fun period -> !@period .>= growthTime)
          | _ -> return! harvests .> !@0 } )
        .&& oneProduct
        .&& oneReplant }

  let private agri = [ Multipliers.agri ]
  let private createRegularBase = createBaseWith regularHarvests

  let private createTypeSingle cropType growthMultipliers amount nameSeed seasons growthStages prices item products =
    cropType <| createRegularBase growthMultipliers nameSeed seasons growthStages prices [ createCropItem nameSeed item amount products ]

  let private createRegularGrowth = createTypeSingle RegularCrop
  let private createGiant = createTypeSingle GiantCrop agri Giant

  let private createRegularAmount = createRegularGrowth agri

  let private createScythe = createRegularAmount FarmingAmounts
  //let createRegularYield = tuple2 >>| DoubleYieldExtra >>| createRegularWith
  let private createRegularExtra = DoubleAndExtra >> createRegularAmount
  let private createRegular = createRegularAmount DoubleChance

  let private createRegular2 nameSeed seasons growthStages prices item products otherItem amount harvestProducts =
    let cropItem = createCropItem nameSeed item FarmingAmounts products
    let otherItem = cropItemWith otherItem (Single amount) (createProducts otherItem harvestProducts (Some <| snd nameSeed))
    RegularCrop <| createRegularBase agri nameSeed seasons growthStages prices [ cropItem; otherItem ]

  let private createRegular2Seed nameSeed seasons growthStages prices item products amount harvestProducts =
    let cropItem = createCropItem nameSeed item FarmingAmounts products
    let seed = snd nameSeed
    let otherItem = cropItemWith seed (Single amount) (createProducts seed harvestProducts None)
    RegularCrop <| createRegularBase agri nameSeed seasons growthStages prices [ cropItem; otherItem ]

  let private createRegrowBase = flip (regrowHarvests >> createBaseWith) agri
  let private createRegrowWith indoorsOnly trelis amount nameSeed seasons growthStages regrowTime prices item products =
    let cropItem = createCropItem nameSeed item amount products
    RegrowCrop
      {| Base = createRegrowBase regrowTime nameSeed seasons growthStages prices [ cropItem ]
         RegrowTime = regrowTime
         Trelis = trelis
         IndoorsOnly = indoorsOnly |}

  let private createOutDoors = createRegrowWith true

  let private createTrelisAmount = createOutDoors true
  //let private createTrelisYield = DoubleYieldExtra >> createTrelisAmount
  //let private createTrelisExtra = DoubleAndExtra >> createTrelisAmount
  let private createTrelis = createTrelisAmount DoubleChance

  let private createRegrowAmount = createOutDoors false
  let private createRegrowYield = DoubleYieldExtra >> createRegrowAmount
  let private createRegrowExtra = DoubleAndExtra >> createRegrowAmount
  let private createRegrow = createRegrowAmount DoubleChance

  let private createRegrowSeedWith indoorsOnly trelis amount nameSeed seasons growthStages regrowTime prices products =
    let cropItem = createCropItem nameSeed (fun _ -> snd nameSeed) amount products
    RegrowCrop
      {| Base = createRegrowBase regrowTime nameSeed seasons growthStages prices [ cropItem ]
         RegrowTime = regrowTime
         Trelis = trelis
         IndoorsOnly = indoorsOnly |}

  let private createOutDoorsSeed = createRegrowSeedWith true
  let private createRegrowSeedAmount = createOutDoorsSeed false
  let private createRegrowSeedYield = DoubleYieldExtra >> createRegrowSeedAmount
  //let private createRegrowSeedExtra = DoubleAndExtra >> createRegrowSeedAmount
  //let private createRegrowSeed = createRegrowSeedAmount DoubleChance

  let private bushGrowthPeriod = !%List.singleton Date.totalDays |> AList.ofAVal

  let private createBushBaseGrowth growthMultipliers (name, seed) (seasons: Season list) growthStages regrowTime harvestDayStart harvestDayEnd prices cropItems =
    let seedPrice = prices |> createSeedPrice seed.Price
    let growthMultiplier = Multiplier.sum growthMultipliers
    let total = List.sum growthStages
    let growthTime = growthTimeCalc total growthStages growthMultiplier
    let seasonsSet, inSeason = Seasons.intersectsWithDateSpan seasons
    let harvests = bushHarvests seasonsSet regrowTime harvestDayStart harvestDayEnd growthTime
    let replants = createReplants cropItems seedPrice seed
    let oneProduct = cropItems |> AList.ofList |> AList.existsA CropItem.oneProduct
    let oneReplant = replants |> AList.ofList |> AList.existsA Replant.selected
    { Name = name
      Seasons = seasonsSet
      InSeason = inSeason
      GrowthStages = growthStages
      TotalGrowthTime = total
      GrowthMultipliers = growthMultipliers
      GrowthMultiplier = growthMultiplier
      GrowthPeriods = bushGrowthPeriod
      GrowthTime = growthTime
      Harvests = harvests
      Seed = seed
      SeedPrice = seedPrice
      CropItems = cropItems
      OneProduct = oneProduct
      Replants = replants
      OneReplant = oneReplant
      Valid =
        oneProduct
        .&& oneReplant
        .&& (harvests .> !@0) }

  let private createBushBase = createBushBaseGrowth []

  let private createBush nameSeed seasons growthStages regrowTime harvestDayStart harvestDayEnd prices item products =
    let cropItem = createCropItem nameSeed item (Single (Normal, 1.0)) products
    Bush
      {| Base = createBushBase nameSeed seasons growthStages regrowTime harvestDayStart harvestDayEnd prices [ cropItem ]
         RegrowTime = regrowTime
         HarvestStartDay = harvestDayStart
         HarvestEndDay = harvestDayEnd |}

  let private springCrop = [ Season.Spring ]
  let private summerCrop = [ Season.Summer ]
  let private fallCrop = [ Season.Fall ]

  let all =
    [ createRegular
        ("Blue Jazz", item "Jazz Seeds" 15)
        springCrop
        [ 1; 2; 2; 2 ]
        PierreAndJoja
        (cropSell 50)
        noProduct

      createGiant
        ("Cauliflower" |> auto 40)
        springCrop
        [ 1; 2; 4; 4; 1 ]
        PierreAndJoja
        (cropSell 175)
        vege

      createRegrowSeedYield
        (4, 0.02)
        ("Coffee", item "Coffee Bean" 15)
        [ Season.Spring; Season.Summer ]
        [ 1; 2; 2; 3; 2 ]
        2
        (PriceList [ price 2500 "Traveling Merchant" ] )
        (productList [ ratioProduct keg "Coffee" 150 5 1 ]) //, false)

      createRegular
        ("Garlic" |> auto 20)
        springCrop
        [ 1; 1; 1; 1 ]
        Pierre
        (cropSell 60)
        vege

      createTrelis
        ("Green Bean", item "Bean Starter" 30)
        springCrop
        [ 1; 1; 1; 3; 4 ]
        3
        PierreAndJoja
        (cropSell 40)
        vege

      createScythe
        ("Kale" |> auto 35)
        springCrop
        [ 1; 2; 2; 1 ]
        PierreAndJoja
        (cropSell 110)
        vege

      createRegular
        ("Parsnip" |> auto 10)
        springCrop
        [ 1; 1; 1; 1 ]
        PierreAndJoja
        (cropSell 35)
        vege

      createRegularExtra
        0.2
        ("Potato" |> auto 25)
        springCrop
        [ 1; 1; 1; 2; 1 ]
        PierreAndJoja
        (cropSell 80)
        vege

      createRegular
        ("Rhubarb" |> auto 50)
        springCrop
        [ 2; 2; 2; 3; 4]
        Oasis
        (cropSell 220)
        fruit

      createRegrowExtra
        0.02
        ("Strawberry" |> auto 0)
        springCrop
        [ 1; 1; 2; 2; 2 ]
        4
        (PriceList [ price 100 "Pierre" ] )
        (cropSell 120)
        fruit

      createRegular
        ("Tulip", item "Tulip Bulb" 10)
        springCrop
        [ 1; 1; 2; 2 ]
        PierreAndJoja
        (cropSell 30)
        noProduct

      createRegularGrowth
        (Multipliers.irrigated :: agri)
        (ExtraChance 0.1)
        ("Rice", item "Rice Shoot" 20)
        springCrop
        [ 1; 2; 2; 3 ]
        Pierre
        (rawCrop "Unmilled Rice" 30)
        (createAndAlso (Vegetable, [ product mill "Rice" 100 ] ))

      createRegrowYield
        (3, 0.02)
        ("Blueberry" |> auto 40)
        summerCrop
        [ 1; 3; 3; 4; 2 ]
        4
        Pierre
        (cropSell 50)
        fruit

      let oil = [ product oilMaker "Oil" 100 ]
      createRegrow
        ("Corn" |> auto 75)
        [ Season.Summer; Season.Fall ]
        [ 2; 3; 3; 3; 3 ]
        4
        PierreAndJoja
        (cropSell 50)
        (createAndAlso (Vegetable, oil))

      createTrelis
        ("Hops", item "Hops Starter" 30)
        summerCrop
        [ 1; 1; 2; 3; 4 ]
        1
        PierreAndJoja
        (cropSell 25)
        (createAndAlso (Jar Pickle, [ kegProduct "Pale Ale" 300 ] ))

      createRegrowExtra
        0.03
        ("Hot Pepper", item "Pepper Seeds" 20)
        summerCrop
        [ 1; 1; 1; 1; 1 ]
        3
        PierreAndJoja
        (cropSell 40)
        fruit

      createGiant
        ("Melon" |> auto 40)
        summerCrop
        [ 1; 2; 3; 3; 3 ]
        PierreAndJoja
        (cropSell 250)
        fruit

      createRegular
        ("Poppy" |> auto 50)
        summerCrop
        [ 1; 2; 2; 2 ]
        PierreAndJoja
        (cropSell 140)
        noProduct

      createRegular
        ("Radish" |> auto 20)
        summerCrop
        [ 2; 1; 2; 1 ]
        PierreAndJoja
        (cropSell 90)
        vege

      createRegular
        ("Red Cabbage" |> auto 50)
        summerCrop
        [ 2; 1; 2; 2; 2 ]
        Pierre
        (cropSell 260)
        vege

      createRegular
        ("Starfruit" |> auto 200)
        summerCrop
        [ 2; 3; 2; 3; 3 ]
        Oasis
        (cropSell 750)
        fruit

      createRegular
        ("Summer Spangle", item "Spangle Seeds" 25)
        summerCrop
        [ 1; 2; 3; 2 ]
        PierreAndJoja
        (cropSell 90)
        noProduct

      createRegular2Seed
        ("Sunflower" |> auto 20)
        [ Season.Summer; Season.Fall ]
        [ 1; 2; 3; 2 ]
        (PriceList
          [ price 200 "Pierre"
            price 125 "Joja" ] )
        (cropSell 80)
        (productList oil)
        (Normal, 1.0)
        (ProductList oil)

      createRegrowExtra
        0.05
        ("Tomato" |> auto 25)
        summerCrop
        [ 2; 2; 2; 2; 3 ]
        4
        PierreAndJoja
        (cropSell 60)
        fruit

      createRegular2
        ("Wheat" |> auto 5)
        [ Season.Summer; Season.Fall ]
        [ 1; 1; 1; 1 ]
        PierreAndJoja
        (cropSell 25)
        (createAndAlso
          ( Jar Pickle,
            [ kegProduct "Beer" 200
              product mill "Wheat Flour" 50 ] ))
        (Item.create "Hay" 50)
        (Normal, 0.4)
        NoProduct
    
      createScythe
        ("Amaranth" |> auto 35)
        fallCrop
        [ 1; 2; 2; 2 ]
        PierreAndJoja
        (cropSell 150)
        vege

      createRegular
        ("Artichoke" |> auto 15)
        fallCrop
        [ 2; 2; 1; 2; 1 ]
        Pierre
        (cropSell 160)
        vege

      createRegular
        ("Beet" |> auto 10)
        fallCrop
        [ 1; 1; 2; 2 ]
        Oasis
        (cropSell 100)
        (createAndAlso (Vegetable, [ ratioProduct mill "Sugar" 50 1 3 ] ))

      createRegular
        ("Bok Choy" |> auto 25)
        fallCrop
        [ 1; 1; 1; 1 ]
        PierreAndJoja
        (cropSell 80)
        vege

      createRegrowYield
        (2, 0.1)
        ("Cranberries" |> auto 60)
        fallCrop
        [ 1; 2; 1; 1; 2 ]
        5
        (JojaWith <| priceBase 240 "Pierre")
        (cropSell 75)
        fruit

      createRegrowExtra
        0.002
        ("Eggplant" |> auto 10)
        fallCrop
        [ 1; 1; 1; 1; 1 ]
        5
        PierreAndJoja
        (cropSell 60)
        vege
    
      createRegular
        ("Fairy Rose", item "Fairy Seeds" 100)
        fallCrop
        [ 1; 4; 4; 3 ]
        PierreAndJoja
        (cropSell 290)
        noProduct

      createTrelis
        ("Grape", item "Grape Starter" 30)
        fallCrop
        [ 1; 1; 2; 3; 3 ]
        3
        PierreAndJoja
        (cropSell 30)
        fruit

      createGiant
        ("Pumpkin" |> auto 50)
        fallCrop
        [ 1; 2; 3; 4; 3 ]
        PierreAndJoja
        (cropSell 320)
        vege

      createRegularGrowth
        []
        DoubleChance
        ("Sweet Gem Berry", item "Rare Seed" 200)
        fallCrop
        [ 2; 4; 6; 6; 6 ]
        (PriceList [ price 1000 "Traveling Merchant" ] )
        (fun _ -> item "Sweet Gem Berry" 3000)
        noProduct

      createRegular
        ("Yam" |> auto 30)
        fallCrop
        [ 1; 3; 3; 3 ]
        PierreAndJoja
        (cropSell 160)
        vege
    
      let ancientSeedProb = !@0.005
      let ancientSeedMakerAmounts = seedMakerAmounts |> mapUsingValues ((.+) ancientSeedProb)
      let ancientSeed = item "Ancient Seeds" 30
      createRegrow
        ("Ancient Fruit", ancientSeed)
        [ Season.Spring; Season.Summer; Season.Fall ]
        [ 2; 7; 7; 7; 5 ]
        7
        NoPrice
        (cropSell 550)
        (CreateAndAlso (Fruit, [ seedMakerWith ancientSeedMakerAmounts ancientSeed ] ), false)

      createBush
        ("Tea", item "Tea Sapling" 500)
        [ Season.Spring; Season.Summer; Season.Fall ]
        [ 10; 10 ]
        1
        22
        28
        (PriceList [ price 100 "Crafting" ] )
        (rawCrop "Tea Leaves" 50)
        (CreateAndAlso (Vegetable, [ kegProduct "Green Tea" 100 ] ), false) ]

  let seedSources = usedSources()
  let processors = usedProcessors()
  let replantSources = usedReplants()



type CropItemProfit =
  { CropItem: CropItem
    Amount: QualityDistribution
    Profit: float aval }

type ProductReplantQuality =
  { Replant: ProductReplant
    CropItem: CropItemProfit
    Quality: Quality }

type ReplantQuality =
  | BuySeeds of SeedPrice
  | ProductReplant of ProductReplantQuality

type CropFertPair =
  { Crop: Crop
    Fertilizer: Fertilizer option
    Products: CropItemProfit list
    Replants: Replant list
    GrowthTime: int aval
    GrowthDays: int aval
    Harvests: int aval
    ProfitPerHarvest: float aval
    Profit: float aval
    ReplantCost: float option aval
    ReplacementFertilizer: float aval option
    NetProfit: float option aval
    Active: bool aval //
    GoldPerDay: float option aval }

    //unit replant cost
    //max amount for replant

//replant, fertilizer, number of replants
module CropFertPair =
  let accountForReplant = cval true
  
  let private distributionMapping mapping =
    let zeroQuality = Map.map mapping Skills.farming.QualityDistribution
    (fun fertilizer ->
      if Fertilizer.qualityOption fertilizer = 0
      then zeroQuality
      else Fertilizer.distributionOption fertilizer |> Map.map mapping)

  let private distributionMap = distributionMapping >> mapOfKeys

  let private giantAmount quality amount =
    match quality with
    | Normal -> (amount .* Crop.noGiantCropProb) .+ Crop.giantCrops
    | _ -> amount .* Crop.noGiantCropProb
  let giantAmounts = distributionMap giantAmount Fertilizers.allOption

  let private additionalAmount additional quality amount =
    match quality with
    | Normal -> amount .+ additional
    | _ -> amount

  let private doubleAmountWith amount = !@amount .* Crop.doubleCropProb .+ !@(amount - 1.0)
  let private doubleAmounts = distributionMap (additionalAmount Crop.doubleCropProb) Fertilizers.allOption

  let private cropsFromExtraChance extraChance = 1.0 / (1.0 - min extraChance 0.9) - 1.0
  let private extraChanceAmounts = cropsFromExtraChance >> (!@) >> additionalAmount >> distributionMapping
  let private doubleExtraAmounts = cropsFromExtraChance >> doubleAmountWith >> additionalAmount >> distributionMapping

  let private doubleYieldExtra cropYield extraChance = float cropYield + cropsFromExtraChance extraChance
  let private doubleYieldExtraAmounts = doubleYieldExtra >>| (doubleAmountWith >> additionalAmount >> distributionMapping)


  let private cropItemAmount (cropItem: CropItem) =
    match cropItem.Amount with
    | FarmingAmounts -> Fertilizer.distributionOption
    | Single (q, a) -> fun _ -> Map.ofList [ q, !@a ]
    | Giant -> mapGet giantAmounts
    | DoubleChance -> mapGet doubleAmounts
    | ExtraChance chance -> extraChanceAmounts chance
    | DoubleAndExtra chance -> doubleExtraAmounts chance
    | DoubleYieldExtra (cropYield, chance) -> doubleYieldExtraAmounts cropYield chance
    | ForagingAmounts -> fun _ -> Skills.foraging.QualityDistribution

  let private createCropItemProfits amounts cropItem =
    { CropItem = cropItem
      Amount = amounts
      Profit = amounts |> Map.toList |> AList.ofList |> AList.sumByA (fun (q,a) -> a .* aDefaultValue 0.0 cropItem.BestProfits.[q]) }

  let createGroup crop =
    let cropItems = Crop.cropItems crop |> List.map (fun cropItem -> 
      let amounts = cropItemAmount cropItem
      (fun fertilizer ->
        createCropItemProfits (amounts fertilizer) cropItem))

    Fertilizers.allOption |> List.map (fun fertilizer ->
      let growthTime = Crop.growthTimeWith fertilizer crop
      let products = cropItems |> List.map (fun f -> f fertilizer)
      let prods = products |> AList.ofList
      let profitPerHarvest = prods |> AList.sumByA (fun p -> p.Profit)
      let harvests = Crop.harvestsWith fertilizer growthTime crop
      let growthDays = !@0
      let profit = profitPerHarvest .* aFloat harvests
      let replantCost = !@None
      let afterFertCost =
        match fertilizer with
        | Some fert ->
            adaptive {
              let! account = Fertilizer.accountForCost
              if account
              then return! AVal.map2 (fun p -> Option.map (fun cost -> p - float cost)) profit fert.BestPrice
              else return! !%Some profit }
        | None -> !%Some profit
      let netProfit = AVal.map2 (Option.map2 (-)) afterFertCost replantCost
      { Crop = crop
        Fertilizer = fertilizer
        Products = products
        Replants = []
        GrowthTime = growthTime
        GrowthDays = growthDays
        Harvests = harvests
        ProfitPerHarvest = profitPerHarvest
        Profit = profit
        ReplantCost = replantCost
        ReplacementFertilizer = None
        NetProfit = netProfit
        Active = !%Option.isSome netProfit
        GoldPerDay = AVal.map2 (fun days -> Option.bind (fun n -> if days = 0 then None else Some (n / float days))) growthDays netProfit } )

  let all =
    Crop.all |> List.collect createGroup //function | Bush b -> ? | crop -> createGroup crop
