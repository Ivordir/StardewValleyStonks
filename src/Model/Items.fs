namespace StardewValleyStonks

type Category =
  | Seeds
  | Vegetable
  | Fruit
  | Flower
  | Forage
  | ArtisanGood
  | Other

[<RequireQualifiedAccess>]
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

[<RequireQualifiedAccess>]
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

[<RequireQualifiedAccess>]
module Item =
  let [<Literal>] ancientSeeds = 499u<ItemNum>
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
      if forage && item.Category = Fruit && not (foragedFruitTillerActive multipliers item)
      then 1.0
      else Category.multiplier skills item.Category

    categoryMultiplier
    * multipliers.ProfitMargin
    * if multipliers.BearsKnowledge && item.Id = blackberry then Multiplier.bearsKnowledge else 1.0

  let private sellPriceAfterMultipliers qualityMultiplier multiplier basePrice =
    basePrice
    |> withMultiplier qualityMultiplier
    |> withMultiplier multiplier
    |> max 1u

  let internal priceCalc multiplier basePrice quality =
    if basePrice = 0u then 0u else
    basePrice |> sellPriceAfterMultipliers Qualities.multipliers[quality] multiplier

  let price skills multipliers forage item quality =
    priceCalc (multiplier skills multipliers forage item) item.SellPrice quality

  let internal priceByQualityCalc multiplier basePrice =
    if basePrice = 0u then Qualities.create 0u else
    Qualities.multipliers |> Qualities.map (fun quality ->
      basePrice |> sellPriceAfterMultipliers quality multiplier)

  let priceByQuality skills multipliers forage item =
    priceByQualityCalc (multiplier skills multipliers forage item) item.SellPrice


[<Fable.Core.Erase>]
type Processor = ProcessorName of string

type ModData = {
  QualityProducts: bool
  QualityProcessors: Processor Set
}

[<RequireQualifiedAccess>]
module Processor =
  let [<Literal>] seedMakerExpectedSeedOutput = 2.0
  let [<Literal>] seedMakerSeedProb = 0.975
  let [<Literal>] seedMakerAncientSeedProb = 0.005

  let preservesJar = ProcessorName "Preserves Jar"
  let keg = ProcessorName "Keg"
  let seedMaker = ProcessorName "Seed Maker"
  let mill = ProcessorName "Mill"

  let preservesQuality modData processor =
    processor <> seedMaker
    && modData.QualityProducts
    && modData.QualityProcessors.Contains processor

  let outputQuality modData quality processor =
    if preservesQuality modData processor
    then quality
    else Quality.Normal

  let seedMakerExpectedQuantity (seed: SeedId) =
    seedMakerSeedProb * seedMakerExpectedSeedOutput
    + if nat seed = nat Item.ancientSeeds then seedMakerAncientSeedProb else 0.0


[<RequireQualifiedAccess>]
module ModData =
  let common = {
    QualityProducts = false
    QualityProcessors = Set.ofArray [|
      Processor.preservesJar
      Processor.keg
      ProcessorName "Oil Maker"
    |]
  }


type ProcessedItem = {
  Item: ItemId
  Processor: Processor
  Ratio: (nat * nat) option
}

[<RequireQualifiedAccess>]
module ProcessedItem =
  let item processsed = processsed.Item
  let processor processsed = processsed.Processor
  let ratio processsed = processsed.Ratio


type Product =
  | Jam of ItemId
  | Pickles of ItemId
  | Wine of ItemId
  | Juice of ItemId
  | SeedsFromSeedMaker of seed: ItemId
  | Processed of ProcessedItem

[<RequireQualifiedAccess>]
module Product =
  let item = function
    | Jam item
    | Pickles item
    | Wine item
    | Juice item
    | SeedsFromSeedMaker item
    | Processed { Item = item } -> item

  let name getItem = function
    | Jam item -> (item |> getItem |> Item.name) + " " + nameof Jam
    | Pickles item -> "Pickled " + (item |> getItem |> Item.name)
    | Wine item -> (item |> getItem |> Item.name) + " " + nameof Wine
    | Juice item -> (item |> getItem |> Item.name) + " " + nameof Juice
    | SeedsFromSeedMaker seed -> seed |> getItem |> Item.name
    | Processed p -> p.Item |> getItem |> Item.name

  let processor = function
    | Jam _ | Pickles _ -> Processor.preservesJar
    | Wine _ | Juice _ -> Processor.keg
    | SeedsFromSeedMaker _ -> Processor.seedMaker
    | Processed p -> p.Processor

  let outputQuality modData quality product =
    product |> processor |> Processor.outputQuality modData quality

  let private artisanMultiplier skills multipliers =
    Category.multiplier skills ArtisanGood * multipliers.ProfitMargin

  let preservesJarPrice basePrice = basePrice * 2u + 50u
  let winePrice basePrice = basePrice * 3u
  let juicePrice basePrice = basePrice |> withMultiplier 2.25

  let private productPrice = function
    | Jam _ | Pickles _ -> preservesJarPrice
    | Wine _ -> winePrice
    | Juice _ -> juicePrice
    | SeedsFromSeedMaker _ | Processed _ -> id

  let private multiplierAndPrice getItem skills multipliers product =
    match product with
    | Jam item | Pickles item | Wine item | Juice item ->
      artisanMultiplier skills multipliers,
      item |> getItem |> Item.sellPrice |> productPrice product

    | SeedsFromSeedMaker itemId
    | Processed { Item = itemId } ->
      let item = getItem itemId
      Item.multiplier skills multipliers false item,
      item.SellPrice

  let priceAndQuality getItem skills multipliers modData quality product =
    let multiplier, price = multiplierAndPrice getItem skills multipliers product
    let quality = outputQuality modData quality product
    Item.priceCalc multiplier price quality, quality

  let price getItem skills multipliers modData quality product =
    priceAndQuality getItem skills multipliers modData quality product |> fst

  let priceByQuality getItem skills multipliers modData product =
    let multiplier, price = multiplierAndPrice getItem skills multipliers product
    if product |> processor |> Processor.preservesQuality modData
    then Item.priceByQualityCalc multiplier price
    else Item.priceCalc multiplier price Quality.Normal |> Qualities.create

  let quantityPerInput product =
    match product with
    | SeedsFromSeedMaker seed -> seed |> convertUnit |> Processor.seedMakerExpectedQuantity
    | Processed { Ratio = Some (i, o) } -> float o / float i
    | _ -> 1.0

  let normalizedPrice getItem skills multipliers modData quality product =
    float (price getItem skills multipliers modData quality product) * quantityPerInput product

  let normalizedPriceByQuality getItem skills multipliers modData product =
    let quantity = quantityPerInput product
    priceByQuality getItem skills multipliers modData product |> Qualities.map (fun price -> float price * quantity)
