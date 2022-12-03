namespace StardewValleyStonks

type Category =
  | Fruit
  | Vegetable
  | Flower
  | ArtisanGood
  | Forage
  | Seeds
  | Other

module Category =
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

module Item =
  let [<Literal>] ancientSeeds = 499u<ItemNum>
  let [<Literal>] blackberry = 410u<ItemNum>
  let [<Literal>] grape = 398u<ItemNum>

  let id item = item.Id
  let name item = item.Name
  let sellPrice item = item.SellPrice
  let category item = item.Category

  let private multiplierValue multipliers item =
    multipliers.ProfitMargin * if multipliers.BearsKnowledge && item.Id = blackberry then Multiplier.bearsKnowledge else 1.0

  let multiplier skills multipliers item =
    Category.multiplier skills item.Category * multiplierValue multipliers item

  let priceCalc multiplier basePrice (quality: Quality) =
    if basePrice = 0u then 0u else
    basePrice
    |> withMultiplier Qualities.multipliers[quality]
    |> withMultiplier multiplier
    |> max 1u

  let price skills multipliers item quality = priceCalc (multiplier skills multipliers item) item.SellPrice quality

  let pricesCalc multiplier basePrice =
    if basePrice = 0u then Qualities.zero else
    Qualities.multipliers |> Qualities.map (fun quality ->
      basePrice
      |> withMultiplier quality
      |> withMultiplier multiplier
      |> max 1u
      |> float)

  let prices skills multipliers item = pricesCalc (multiplier skills multipliers item) item.SellPrice

  module Forage =
    let fruitTillerPossible = [| blackberry; grape |]

    let fruitTillerActive multipliers item = multipliers.TillerForForagedFruit && fruitTillerPossible |> Array.contains item.Id

    let multipler skills multipliers item =
      let multiplier =
        if item.Category <> Fruit || fruitTillerActive multipliers item
        then Category.multiplier skills item.Category
        else 1.0
      multiplier * multiplierValue multipliers item

    let price skills multipliers item quality = priceCalc (multiplier skills multipliers item) item.SellPrice quality
    let prices skills multipliers item = pricesCalc (multiplier skills multipliers item) item.SellPrice


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

  let seedMakerAmount = seedMakerSeedProb * float seedMakerSeedAmount

  let seedMakerAmountWith (seed: SeedId) =
    seedMakerAmount + if nat seed = nat Item.ancientSeeds then seedMakerAncientSeedProb else 0.0


module ModData =
  let common = {
    QualityProducts = false
    QualityProcessors = Set.ofArray [| Processor.preservesJar; Processor.keg; ProcessorName "Oil Maker" |]
  }


type Product =
  | Jam
  | Pickles
  | Wine
  | Juice
  | SeedsFromSeedMaker of seed: ItemId
  | Processed of {|
      Item: ItemId
      Processor: Processor
      Ratio: (nat * nat) option
    |}

module Product =
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

  let ratioAmount (i: nat, o: nat) = float o / float i

  let private artisanMultiplier skills multipliers =
    Category.multiplier skills ArtisanGood * multipliers.ProfitMargin

  let preservesJarPrice basePrice = basePrice * 2u + 50u
  let winePrice basePrice = basePrice * 3u
  let juicePrice basePrice = basePrice |> withMultiplier 2.25

  let priceCalc modData multiplier basePrice processor quality =
    Item.priceCalc multiplier basePrice (processor |> Processor.outputQuality modData quality)

  let price getItem skills multipliers modData item quality product =
    match product with
    | Jam | Pickles ->
      let item = getItem item
      priceCalc
        modData
        (artisanMultiplier skills multipliers)
        (preservesJarPrice item.SellPrice)
        Processor.preservesJar
        quality
    | Wine ->
      let item = getItem item
      priceCalc
        modData
        (artisanMultiplier skills multipliers)
        (winePrice item.SellPrice)
        Processor.keg quality
    | Juice ->
      let item = getItem item
      priceCalc
        modData
        (artisanMultiplier skills multipliers)
        (juicePrice item.SellPrice)
        Processor.keg
        quality
    | SeedsFromSeedMaker seedId ->
      let item = getItem seedId
      priceCalc
        modData
        (Item.multiplier skills multipliers item)
        item.SellPrice
        Processor.seedMaker
        quality
    | Processed p ->
      let item = getItem p.Item
      priceCalc
        modData
        (Item.multiplier skills multipliers item)
        item.SellPrice
        p.Processor
        quality

  let pricesCalc modData multiplier basePrice processor =
    if processor |> Processor.preservesQuality modData
    then Item.pricesCalc multiplier basePrice
    else Item.priceCalc multiplier basePrice Quality.Normal |> float |> Qualities.create

  let prices getItem skills multipliers modData item = function
    | Jam | Pickles ->
      let item = getItem item
      pricesCalc
        modData
        (artisanMultiplier skills multipliers)
        (preservesJarPrice item.SellPrice)
        Processor.preservesJar
    | Wine ->
      let item = getItem item
      pricesCalc
        modData
        (artisanMultiplier skills multipliers)
        (winePrice item.SellPrice)
        Processor.keg
    | Juice ->
      let item = getItem item
      pricesCalc
        modData
        (artisanMultiplier skills multipliers)
        (juicePrice item.SellPrice)
        Processor.keg
    | SeedsFromSeedMaker seedId ->
      let item = getItem seedId
      pricesCalc
        modData
        (Item.multiplier skills multipliers item)
        item.SellPrice
        Processor.seedMaker
    | Processed p ->
      let item = getItem p.Item
      pricesCalc
        modData
        (Item.multiplier skills multipliers item)
        item.SellPrice
        p.Processor

  let amountPerItem product =
    match product with
    | SeedsFromSeedMaker seedId ->
      Processor.seedMakerAmountWith (seedId * 1u<_>)
    | Processed p ->
      match p.Ratio with
      | Some ratio -> ratioAmount ratio
      | None -> 1.0
    | _ -> 1.0

  let profit getItem skills multipliers modData item quality product =
    float (price getItem skills multipliers modData item quality product) * amountPerItem product

  let profits getItem skills multipliers modData item product =
    let prices = prices getItem skills multipliers modData item product
    let amount = amountPerItem product
    if amount = 1.0
    then prices
    else prices |> Qualities.map ((*) amount)
