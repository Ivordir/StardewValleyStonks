namespace StardewValleyStonks

open Fable.Core

type Category =
  | Fruit
  | Vegetable
  | Flower
  | Artisan
  | Forage
  | Seeds
  | Other

module Category =
  let multiplier skills = function
    | Fruit | Vegetable | Flower when Skills.tillerActive skills -> 1.1
    | Artisan when Skills.artisanActive skills -> 1.4
    | _ -> 1.0

type [<Measure>] ItemNum
type ItemId = int<ItemNum>
type Item = {
  Id: ItemId
  Name: string
  SellPrice: nat
  Category: Category
}

type Multipliers = {
  ProfitMargin: float // 0.25 | 0.5 | 0.75 | 1.0
  BearsKnowledge: bool
  ForagedFruitTillerOverrides: ItemId Set
}

module Item =
  let [<Literal>] ancientSeeds = 499<ItemNum>
  let [<Literal>] blackberry = 410<ItemNum>

  let inline id item = item.Id
  let inline name item = item.Name
  let inline sellPrice item = item.SellPrice
  let inline category item = item.Category

  let private multiplierValue multipliers item =
    multipliers.ProfitMargin * if item.Id = blackberry && multipliers.BearsKnowledge then 3.0 else 1.0

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
    let multipler skills multipliers item =
      let multiplier =
        match item.Category with
        | Fruit when not <| multipliers.ForagedFruitTillerOverrides.Contains item.Id -> 1.0
        | category -> Category.multiplier skills category
      multiplier * multiplierValue multipliers item

    let price skills multipliers item quality = priceCalc (multiplier skills multipliers item) item.SellPrice quality
    let prices skills multipliers item = pricesCalc (multiplier skills multipliers item) item.SellPrice




type [<Erase>] Processor = ProcessorName of string

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

  let seedMakerAmount seed =
    seedMakerSeedProb * float seedMakerSeedAmount
    + if seed = Item.ancientSeeds then seedMakerAncientSeedProb else 0.0


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

  let ratioAmount (i: nat, o: nat) = float o / float i

  let ratioValid (i: nat, o: nat) = nonZero i && nonZero o

  let private artisanMultiplier skills multipliers =
    Category.multiplier skills Artisan * multipliers.ProfitMargin

  let inline preservesJarPrice basePrice = basePrice * 2u + 50u
  let inline winePrice basePrice = basePrice * 3u
  let inline juicePrice basePrice = basePrice |> withMultiplier 2.25

  let priceCalc modData multiplier basePrice processor quality =
    let quality =
      if processor |> Processor.preservesQuality modData
      then quality
      else Quality.Normal
    Item.priceCalc multiplier basePrice quality

  let price getItem skills multipliers modData item quality product =
    match product with
    | Jam | Pickles -> priceCalc modData (artisanMultiplier skills multipliers) (preservesJarPrice item.SellPrice) Processor.preservesJar quality
    | Wine -> priceCalc modData (artisanMultiplier skills multipliers) (winePrice item.SellPrice) Processor.keg quality
    | Juice -> priceCalc modData (artisanMultiplier skills multipliers) (juicePrice item.SellPrice) Processor.keg quality
    | SeedsFromSeedMaker seedId ->
      let item = getItem seedId
      priceCalc modData (Item.multiplier skills multipliers item) item.SellPrice Processor.seedMaker quality
    | Processed p ->
      let item = getItem p.Item
      priceCalc modData (Item.multiplier skills multipliers item) item.SellPrice p.Processor quality

  let pricesCalc modData multiplier basePrice processor =
    if processor |> Processor.preservesQuality modData
    then Item.pricesCalc multiplier basePrice
    else Item.priceCalc multiplier basePrice Quality.Normal |> float |> Qualities.create

  let prices getItem skills multipliers modData item = function
    | Jam | Pickles -> pricesCalc modData (artisanMultiplier skills multipliers) (preservesJarPrice item.SellPrice) Processor.preservesJar
    | Wine -> pricesCalc modData (artisanMultiplier skills multipliers) (winePrice item.SellPrice) Processor.keg
    | Juice -> pricesCalc modData (artisanMultiplier skills multipliers) (juicePrice item.SellPrice) Processor.keg
    | SeedsFromSeedMaker seedId ->
      let item = getItem seedId
      pricesCalc modData (Item.multiplier skills multipliers item) item.SellPrice Processor.seedMaker
    | Processed p ->
      let item = getItem p.Item
      pricesCalc modData (Item.multiplier skills multipliers item) item.SellPrice p.Processor

  let amountPerItem product =
    match product with
    | SeedsFromSeedMaker seedId ->
      Processor.seedMakerAmount seedId
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
