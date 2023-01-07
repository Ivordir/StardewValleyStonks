namespace StardewValleyStonks

type Category =
  | Fruit
  | Vegetable
  | Flower
  | ArtisanGood
  | Forage
  | Seeds
  | Other

module [<RequireQualifiedAccess>] Category =
  let multiplier skills = function
    | Fruit | Vegetable | Flower when skills |> Skills.professionActive Tiller -> Multiplier.tiller
    | ArtisanGood when skills |> Skills.professionActive Artisan -> Multiplier.artisan
    | _ -> 1.0


type [<Measure>] SeedNum

type SeedId = uint<SeedNum>

type [<Measure>] ItemNum

type ItemId = uint<ItemNum>


type Multipliers = {
  ProfitMargin: float // 0.25 | 0.5 | 0.75 | 1.0
  BearsKnowledge: bool
  TillerForForagedFruit: bool
}

module Multipliers =
  let common = {
    ProfitMargin = 1.0
    BearsKnowledge = false
    TillerForForagedFruit = false
  }


type Item = {
  Id: ItemId
  Name: string
  SellPrice: nat
  Category: Category
}

module [<RequireQualifiedAccess>] Item =
  let [<Literal>] ancientSeeds = 499u<ItemNum>
  let [<Literal>] sweetGemBerry = 417u<ItemNum>
  let [<Literal>] blackberry = 410u<ItemNum>
  let [<Literal>] grape = 398u<ItemNum>

  let id item = item.Id
  let name item = item.Name
  let sellPrice item = item.SellPrice
  let category item = item.Category

  let foragedFruitTillerPossible = [| blackberry; grape |]

  let foragedFruitTillerActive multipliers item =
    multipliers.TillerForForagedFruit && foragedFruitTillerPossible |> Array.contains item.Id

  let multiplier skills multipliers forage item =
    let categoryMultiplier =
      if forage && item.Category = Fruit && not <| foragedFruitTillerActive multipliers item
      then 1.0
      else Category.multiplier skills item.Category
    categoryMultiplier
    * multipliers.ProfitMargin
    * if multipliers.BearsKnowledge && item.Id = blackberry then Multiplier.bearsKnowledge else 1.0

  let private finalPrice qualityMultiplier multiplier basePrice =
    basePrice
    |> withMultiplier qualityMultiplier
    |> withMultiplier multiplier
    |> max 1u

  let internal priceCalc multiplier basePrice (quality: Quality) =
    if basePrice = 0u then 0u else
    basePrice |> finalPrice Qualities.multipliers[quality] multiplier

  let price skills multipliers forage item quality =
    priceCalc (multiplier skills multipliers forage item) item.SellPrice quality

  let internal priceByQualityCalc multiplier basePrice =
    if basePrice = 0u then Qualities.zero else
    Qualities.multipliers |> Qualities.map (fun quality ->
      basePrice |> finalPrice quality multiplier)

  let priceByQuality skills multipliers forage item =
    priceByQualityCalc (multiplier skills multipliers forage item) item.SellPrice


type [<Fable.Core.Erase>] Processor = ProcessorName of string

type ModData = {
  QualityProducts: bool
  QualityProcessors: Processor Set
}

module Processor =
  let [<Literal>] seedMakerSeedAmount = 2u
  let [<Literal>] seedMakerSeedProb = 0.975
  let [<Literal>] seedMakerAncientSeedProb = 0.005

  let preservesJar = ProcessorName "Preserves Jar"
  let keg = ProcessorName "Keg"
  let seedMaker = ProcessorName "Seed Maker"

  let preservesQuality modData processor =
    processor <> seedMaker
    && modData.QualityProducts
    && modData.QualityProcessors.Contains processor

  let outputQuality modData quality processor =
    if preservesQuality modData processor
    then quality
    else Quality.Normal

  let seedMakerExpectedAmount (seed: SeedId) =
    seedMakerSeedProb * float seedMakerSeedAmount
    + if nat seed = nat Item.ancientSeeds then seedMakerAncientSeedProb else 0.0

  let seedMakerInputNeededForOneSeed (seed: SeedId) =
    1.0 / seedMakerExpectedAmount seed


module [<RequireQualifiedAccess>] ModData =
  let common = {
    QualityProducts = false
    QualityProcessors = Set.ofArray [| Processor.preservesJar; Processor.keg; ProcessorName "Oil Maker" |]
  }


type ProcessedItem = {
  Item: ItemId
  Processor: Processor
  Ratio: (nat * nat) option
}

type Product =
  | Jam
  | Pickles
  | Wine
  | Juice
  | SeedsFromSeedMaker of seed: ItemId
  | Processed of ProcessedItem

module [<RequireQualifiedAccess>] Product =
  let item = function
    | Jam | Pickles | Wine | Juice -> None
    | SeedsFromSeedMaker seed -> Some seed
    | Processed p -> Some p.Item

  let name getItem item = function
    | Jam -> (getItem item |> Item.name) + " " + nameof Jam
    | Pickles -> "Pickled " + (getItem item |> Item.name)
    | Wine -> (getItem item |> Item.name) + " " + nameof Wine
    | Juice -> (getItem item |> Item.name) + " " + nameof Juice
    | SeedsFromSeedMaker seed -> getItem seed |> Item.name
    | Processed p -> getItem p.Item |> Item.name

  let processor = function
    | Jam | Pickles -> Processor.preservesJar
    | Wine | Juice -> Processor.keg
    | SeedsFromSeedMaker _ -> Processor.seedMaker
    | Processed p -> p.Processor

  let outputQuality modData quality product = processor product |> Processor.outputQuality modData quality

  let private artisanMultiplier skills multipliers =
    Category.multiplier skills ArtisanGood * multipliers.ProfitMargin

  let preservesJarPrice basePrice = basePrice * 2u + 50u
  let winePrice basePrice = basePrice * 3u
  let juicePrice basePrice = basePrice |> withMultiplier 2.25

  let private priceCalc modData multiplier basePrice processor quality =
    Item.priceCalc multiplier basePrice (processor |> Processor.outputQuality modData quality)

  let price getItem skills multipliers modData item quality product =
    match product with
    | Jam | Pickles ->
      priceCalc
        modData
        (artisanMultiplier skills multipliers)
        (getItem item |> Item.sellPrice |> preservesJarPrice)
        Processor.preservesJar
        quality
    | Wine ->
      priceCalc
        modData
        (artisanMultiplier skills multipliers)
        (getItem item |> Item.sellPrice |> winePrice)
        Processor.keg
        quality
    | Juice ->
      priceCalc
        modData
        (artisanMultiplier skills multipliers)
        (getItem item |> Item.sellPrice |> juicePrice)
        Processor.keg
        quality
    | SeedsFromSeedMaker seedId ->
      let item = getItem seedId
      priceCalc
        modData
        (Item.multiplier skills multipliers false item)
        item.SellPrice
        Processor.seedMaker
        quality
    | Processed p ->
      let item = getItem p.Item
      priceCalc
        modData
        (Item.multiplier skills multipliers false item)
        item.SellPrice
        p.Processor
        quality

  let private priceByQualityCalc modData multiplier basePrice processor =
    if processor |> Processor.preservesQuality modData
    then Item.priceByQualityCalc multiplier basePrice
    else Item.priceCalc multiplier basePrice Quality.Normal |> Qualities.create

  let priceByQuality getItem skills multipliers modData item = function
    | Jam | Pickles ->
      let item = getItem item
      priceByQualityCalc
        modData
        (artisanMultiplier skills multipliers)
        (preservesJarPrice item.SellPrice)
        Processor.preservesJar
    | Wine ->
      let item = getItem item
      priceByQualityCalc
        modData
        (artisanMultiplier skills multipliers)
        (winePrice item.SellPrice)
        Processor.keg
    | Juice ->
      let item = getItem item
      priceByQualityCalc
        modData
        (artisanMultiplier skills multipliers)
        (juicePrice item.SellPrice)
        Processor.keg
    | SeedsFromSeedMaker seedId ->
      let item = getItem seedId
      priceByQualityCalc
        modData
        (Item.multiplier skills multipliers false item)
        item.SellPrice
        Processor.seedMaker
    | Processed p ->
      let item = getItem p.Item
      priceByQualityCalc
        modData
        (Item.multiplier skills multipliers false item)
        item.SellPrice
        p.Processor

  let amountPerItem product =
    match product with
    | SeedsFromSeedMaker seedId -> Processor.seedMakerExpectedAmount (seedId * 1u<_>)
    | Processed { Ratio = Some (i, o) } -> float o / float i
    | _ -> 1.0

  let normalizedPrice getItem skills multipliers modData item quality product =
    float (price getItem skills multipliers modData item quality product) * amountPerItem product

  let normalizedPriceByQuality getItem skills multipliers modData item product =
    let amount = amountPerItem product
    priceByQuality getItem skills multipliers modData item product |> Qualities.map (fun price -> float price * amount)
