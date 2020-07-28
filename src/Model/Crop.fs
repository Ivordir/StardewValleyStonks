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
  static member List =
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

type Item =
  { Name: string
    BasePrice: int
    Multiplier: NameOf<Multiplier> option }

type ProductSource =
  | RawCrop
  | Processor of NameOf<Processor>
  | SeedMaker

module ProductSource =
  let name = function
    | RawCrop -> "Raw Crop"
    | Processor p -> ofName p
    | SeedMaker -> "Seed Maker"

  let nameOf = toNameOf name

type Product =
  | RawItem of Override: bool option
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
  | SeedsFromSeedMaker of Override: bool option

module Product =
  let source = function
    | RawItem -> RawCrop
    | Process p -> Processor p.Processor
    | RatioProcess r -> Processor r.Processor
    | SeedsFromSeedMaker -> SeedMaker
  
  let inputAmount = function
    | RatioProcess r -> r.InputAmount
    | _ -> 1
  
  let outputAmount = function
    | RatioProcess r -> r.OutputAmount
    | _ -> 1.0
  
  let sourceOverride = function
    | RawItem o -> o
    | Process p -> p.ProcessorOverride
    | RatioProcess r -> r.ProcessorOverride
    | SeedsFromSeedMaker o -> o

type Replant =
  | SeedMaker
  | SeedOrCrop

module Replant =
  let all =
    [ SeedMaker
      SeedOrCrop ]

  let name = function
    | SeedMaker -> "Seed Maker"
    | SeedOrCrop -> "Harvested Crop or Seed"


type HarvestedItemData =
  { Item: Item
    Amount: float
    Products: Map<ProductSource, Product>
    Replant: Replant option
    ReplantOverride: bool option }

type Crop =
  { Name: string
    Selected: bool
    Seasons: Set<Season>
    SelectedSeasons: Set<Season>
    GrowthStages: int list
    TotalGrowthTime: int
    RegrowTime: int option
    GrowthMultipliers: NameOf<Multiplier> list
    Item: Item
    Seed: Item
    Products: Map<ProductSource, Product>
    PriceFrom: Map<NameOf<Source>, Price>
    BuySeedsOverride: bool option
    Replant: Replant option
    ReplantOverride: bool option
    IsGiantCrop: bool
    HasDoubleCropChance: bool
    ExtraCrops: float
    HarvestedItems: Map<NameOf<Item>, HarvestedItemData> }
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
  { HarvestItem: HarvestedItemData
    Product: Product }

// Haha... "Cash" Crop
type CacheCrop =
  { Crop: Crop
    GrowthMultiplier: float
    BestProducts: Map<Quality, Product>
    SeedPrice: int
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
    Price.Price
      {| Value = seedSellPrice * 2
         Source = Name name
         Requirements = List.empty
         SourceOverride = None |}

  let private createJojaPrice pierrePrice =
    MatchPrice
      {| Value = (pierrePrice |> Price.value |> float) * 1.25 |> int
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
    { Name = "Initial"
      BasePrice = -1
      Multiplier = None }

  let agri: NameOf<Multiplier> list = [ Name "Agriculturist" ]

  let private initialCrop =
    { Name = "Initial"
      Selected = true
      Seasons = Set.empty
      SelectedSeasons = Set.empty
      GrowthStages = List.empty
      TotalGrowthTime = -1
      RegrowTime = None
      GrowthMultipliers = agri
      Item = initialItem
      Seed = initialItem
      PriceFrom = Map.empty
      Products = Map.empty
      BuySeedsOverride = None
      Replant = None
      ReplantOverride = None
      IsGiantCrop = false
      HasDoubleCropChance = true
      ExtraCrops = 0.0
      HarvestedItems = Map.empty }

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
          [ RawItem None
            yield! createProducts createdCropItem products
            if seedMaker then
              SeedsFromSeedMaker None ]
          |> listToMap Product.source
        PriceFrom = createPrices createdSeed.BasePrice prices |> listToMap Price.nameOf
        Replant =
          if seedMaker then
            Some SeedMaker
          elif seed = CropItem then
            Some SeedOrCrop
          else
            None }

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