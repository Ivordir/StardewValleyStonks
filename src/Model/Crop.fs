namespace StardewValleyStonks

open Types

type Processor =
  { Name: string
    Selected: bool
    Requirements: Requirement list
    PreservesQuality: bool }
  member this.Toggle = { this with Selected = not this.Selected }
  member this.TogglePreservesQuality = { this with PreservesQuality = not this.PreservesQuality }

module Processor =
  let initial =
    { Name = "Initial"
      Selected = true
      Requirements = List.empty
      PreservesQuality = false }
  
  let name processor = processor.Name

  let nameOf = toNameOf name

type Quality =
  | Normal
  | Silver
  | Gold
  | Iridium
  member this.Multiplier =
    match this with
    | Normal -> 1.0
    | Silver -> 1.25
    | Gold -> 1.5
    | Iridium -> 2.0

module Quality =
  let all =
    [ Normal
      Silver
      Gold
      Iridium ]

type Multiplier =
  | Multiplier of
      {| Name: string
         Value: float
         Selected: bool |}
  | Profession of
      {| Skill: NameOf<Skill>
         Profession: NameOf<Profession>
         Value: float |}

module Multiplier =
  let name = function
    | Multiplier m -> m.Name
    | Profession p -> ofName p.Profession

  let nameOf = toNameOf name

  let isRawMultiplier = function
    | Multiplier _ -> true
    | Profession _ -> false

type Item =
  { Name: string
    BasePrice: int
    Multiplier: NameOf<Multiplier> option }

module Item =
  let name item = item.Name

  let nameOf = toNameOf name

  let create name basePrice =
    { Name = name
      BasePrice = basePrice
      Multiplier = None }

type Product =
  | Process of
      {| Processor: NameOf<Processor>
         Output: Item
         ProcessorOverride: bool option |}
  | RatioProcess of
      {| InputAmount: int
         Processor: NameOf<Processor>
         Output: Item
         OutputAmount: float
         ProcessorOverride: bool option |}

module Product =
  let processor = function
    | Process p -> p.Processor
    | RatioProcess r -> r.Processor
  
  let inputAmount = function
    | RatioProcess r -> r.InputAmount
    | _ -> 1
  
  let outputAmount = function
    | RatioProcess r -> r.OutputAmount
    | _ -> 1.0
  
  let sourceOverride = function
    | Process p -> p.ProcessorOverride
    | RatioProcess r -> r.ProcessorOverride

type Replant =
  | SeedMaker of SellSeedsOverride: bool option * ReplantOverride: bool option
  | SeedOrCrop of Override: bool option
  | NoReplant

type HarvestedItem =
  { Item: Item
    Amount: float
    SellRawItemOverride: bool option
    Products: Map<NameOf<Processor>, Product>
    Replant: Replant }

module HarvestedItem =
  let item harvestedItem = harvestedItem.Item

  let nameOfItem = item >> Item.nameOf

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
    Item: Item
    SellRawCropOverride: bool option
    Products: Map<NameOf<Processor>, Product>
    PriceFrom: Map<NameOf<Source>, Price>
    BuySeedsOverride: bool option
    Replant: Replant
    IsGiantCrop: bool
    HasDoubleCropChance: bool
    ExtraCrops: float
    OtherHarvestedItems: Map<NameOf<Item>, HarvestedItem> }
  member this.Toggle = { this with Selected = not this.Selected }

  type CropSort =
    | ByName
    | Selected
    | Seasons
    | TotalGrowthTime
    | RegrowTime
    | SeedPrice

  type CreateCropItem =
    | SellPrice of int
    | Item of Item

  type CreateSeed =
    | SeedSell of int
    | Seed of Item
    | CropItem

  type KegProduct =
    | Wine
    | Juice

  type JarProduct =
    | Jam
    | Pickle

  type CreateProducts =
    | Fruit
    | Vegetable
    | Keg of KegProduct
    | Jar of JarProduct
    | ProductList of Product list
    | CreateAndList of CreateProducts * Product list
    | NoProduct

  type CreatePrices =
    | Pierre
    | Joja of PierrePrice: Price
    | Oasis
    | PierreAndJoja
    | PriceList of Price list
    | NoPrice

type HarvestedItemDataCache =
  { HarvestItem: HarvestedItem
    Product: Product list }

// Haha... "Cash" Crop
type CacheCrop =
  { Crop: Crop
    GrowthMultiplier: float
    BestProducts: Map<Quality, Product list>
    SeedPrice: int option
    SeedSources: NameOf<Source> list
    HarvestedItems: Map<NameOf<Item>, HarvestedItemDataCache>
    BestReplants: NameOf<Item> list }

module Crop =
  let private createProcess processor outputName price =
     Process
       {| Processor = Name processor
          Output =
            { Name = outputName
              BasePrice = price
              Multiplier = Some (Name "Artisan") }
          ProcessorOverride = None |}

  let private createKegProduct (cropItem: Item) = function
    | Wine -> createProcess "Keg" (cropItem.Name + " Wine") (cropItem.BasePrice * 3)
    | Juice -> createProcess "Keg" (cropItem.Name + " Juice") (float cropItem.BasePrice * 2.25 |> int)

  let private createJarProduct (cropItem: Item) = function
    | Jam -> createProcess "Preserves Jar" (cropItem.Name + " Jam") (cropItem.BasePrice * 2 + 50)
    | Pickle -> createProcess "Preserves Jar" ("Pickeled " + cropItem.Name) (cropItem.BasePrice * 2 + 50)

  let rec private createProducts cropItem = function
    | Fruit -> [ createKegProduct cropItem Wine; createJarProduct cropItem Jam ]
    | Vegetable -> [ createKegProduct cropItem Juice; createJarProduct cropItem Pickle ]
    | Keg product -> [ createKegProduct cropItem product ]
    | Jar product -> [ createJarProduct cropItem product ]
    | ProductList list -> list
    | CreateAndList (products, list) -> createProducts cropItem products @ list
    | NoProduct -> List.empty

  let private createPrice seedSellPrice name =
    BuyPrice
      {| Value = seedSellPrice * 2
         Source = Name name
         Requirements = List.empty
         SourceOverride = None |}

  let private createJojaPrice pierrePrice =
    MatchPrice
      {| Value = pierrePrice |> Price.value |> float |> (*) 1.25 |> int
         Source = Name "Joja"
         Requirements = List.empty
         SourceOverride = None
         MatchSource = Name "Pierre"
         MatchCondition = Name "Joja Membership" |}

  let private createPrices seedSellPrice = function
    | Pierre -> [ createPrice seedSellPrice "Pierre" ]
    | Joja pierrePrice -> [ pierrePrice; createJojaPrice pierrePrice ]
    | Oasis -> [ createPrice seedSellPrice "Oasis" ]
    | PierreAndJoja ->
      let pierrePrice = createPrice seedSellPrice "Pierre"
      [ pierrePrice
        createJojaPrice pierrePrice ]
    | PriceList list -> list
    | NoPrice -> List.empty

  let private initialItem =
    { Name = "initial"
      BasePrice = -1
      Multiplier = None }

  let agri: NameOf<Multiplier> list = [ Name "Agriculturist" ]

  let private initialCrop =
    { Name = "initial"
      Selected = true
      Seasons = Set.empty
      SelectedSeasons = Set.empty
      GrowthStages = List.empty
      TotalGrowthTime = -1
      RegrowTime = None
      GrowthMultipliers = agri
      Seed = initialItem
      Item = initialItem
      SellRawCropOverride = None
      Products = Map.empty
      PriceFrom = Map.empty
      BuySeedsOverride = None
      Replant = NoReplant
      IsGiantCrop = false
      HasDoubleCropChance = true
      ExtraCrops = 0.0
      OtherHarvestedItems = Map.empty }

  let private createCrop
    seedMaker
    name
    (seasons: Season list)
    growthStages
    cropItem
    seed
    products
    prices =
    let seasonsSet = set seasons
    let createdCropItem =
      match cropItem with
      | SellPrice price ->
          { Name = name
            BasePrice = price
            Multiplier = Some (Name "Tiller") }
      | Item item -> item
    let createdSeed =
      match seed with
      | SeedSell price ->
          { Name = name + " Seeds"
            BasePrice = price
            Multiplier = None }
      | Seed item -> item
      | CropItem -> createdCropItem
    { initialCrop with
        Name = name
        Seasons = seasonsSet
        SelectedSeasons = seasonsSet
        GrowthStages = growthStages
        TotalGrowthTime = List.sum growthStages
        Item = createdCropItem
        Seed = createdSeed
        Products =
          createProducts createdCropItem products
          |> listToMap Product.processor
        PriceFrom = createPrices createdSeed.BasePrice prices |> listToMap Price.nameOf
        Replant =
          if seedMaker then
            SeedMaker (None, None)
          elif seed = CropItem then
            SeedOrCrop None
          else
            NoReplant }

  let create = createCrop true
  let createWithoutSeedMaker = createCrop false

  let name crop = crop.Name
  let selected crop = crop.Selected
  let seasons crop = crop.Seasons
  let totalGrowthTime crop = crop.TotalGrowthTime
  let regrowTime crop = crop.RegrowTime
  let priceFrom crop = crop.PriceFrom

  let nameOf = toNameOf name

  let extraCrops cropYield extraChance = 1.0 / (1.0 - extraChance) + float cropYield - 2.0

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