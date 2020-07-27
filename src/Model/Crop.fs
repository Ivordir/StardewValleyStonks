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
      {| Skill: Name<Skill>
         Profession: Name<Profession>
         Value: float |}
  member this.Name =
    match this with
    | Multiplier m -> m.Name
    | Profession p -> ofName p.Profession

type Item =
  { Name: string
    BasePrice: int
    Multiplier: Name<Multiplier> option }

type ProductSource =
  | RawCrop
  | Processor of Name<Processor>
  | SeedMaker
  member this.Name =
    match this with
    | RawCrop -> "Raw Crop"
    | Processor p -> ofName p
    | SeedMaker -> "Seed Maker"

type Product =
  | RawItem of Override: bool option
  | Process of
      {| Processor: Name<Processor>
         Output: Item
         Override: bool option |}
  | RatioProcess of
      {| InputAmount: int
         Processor: Name<Processor>
         Output: Item
         OutputAmount: float
         Override: bool option |}
  | SeedsFromSeedMaker of Override: bool option
  member this.Source =
    match this with
    | RawItem -> RawCrop
    | Process p -> Processor p.Processor
    | RatioProcess r -> Processor r.Processor
    | SeedsFromSeedMaker -> SeedMaker
  member this.InputAmount =
    match this with
    | RatioProcess r -> r.InputAmount
    | _ -> 1
  member this.OutputAmount =
    match this with
    | RatioProcess r -> r.OutputAmount
    | _ -> 1.0

type Replant =
  | SeedMaker
  | SeedOrCrop
  member this.Name =
    match this with
    | SeedMaker -> "Seed Maker"
    | SeedOrCrop -> "Harvested Crop or Seed"
  static member List =
    [ SeedMaker
      SeedOrCrop ]

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
    GrowthMultipliers: Name<Multiplier> list
    Item: Item
    Seed: Item
    Products: Map<ProductSource, Product>
    PriceFrom: Map<Name<Source>, Price>
    BuySeedsOverride: bool option
    Replant: Replant option
    ReplantOverride: bool option
    IsGiantCrop: bool
    HasDoubleCropChance: bool
    ExtraCrops: float
    HarvestedItems: Map<Name<Item>, HarvestedItemData> }
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
    | NoPrices

module Crop =
  let private createProcess processor outputName price =
     Process
       {| Processor = Name processor
          Output =
            { Name = outputName
              BasePrice = price
              Multiplier = Some (Name "Artisan") }
          Override = None |}

  let private createKegProduct product (cropItem: Item) =
    match product with
    | Wine -> createProcess "Keg" (cropItem.Name + " Wine") (cropItem.BasePrice * 3)
    | Juice -> createProcess "Keg" (cropItem.Name + " Juice") (float cropItem.BasePrice * 2.25 |> int)

  let private createJarProduct product (cropItem: Item) =
    match product with
    | Jam -> createProcess "Preserves Jar" (cropItem.Name + " Jam") (cropItem.BasePrice * 2 + 50)
    | Pickle -> createProcess "Preserves Jar" ("Pickeled " + cropItem.Name) (cropItem.BasePrice * 2 + 50)

  let rec private createProducts products (cropItem: Item) =
    match products with
    | Fruit -> [ createKegProduct Wine cropItem; createJarProduct Jam cropItem ]
    | Vegetable -> [ createKegProduct Juice cropItem; createJarProduct Pickle cropItem ]
    | Keg product -> [ createKegProduct product cropItem ]
    | Jar product -> [ createJarProduct product cropItem ]
    | ProductList list -> list
    | CreateAndList (products, list) -> createProducts products cropItem @ list
    | NoProduct -> List.empty

  let private createPrice seedSellPrice name =
    Price.Price
      {| Value = seedSellPrice * 2
         Source = Name name
         Requirements = List.empty
         OverrideSource = None |}

  let private createJojaPrice (pierrePrice: Price) =
    MatchPrice
      {| Value = (pierrePrice |> Price.value |> float) * 1.25 |> int
         Source = Name "Joja"
         Requirements = List.empty
         OverrideSource = None
         MatchSource = Name "Pierre"
         MatchCondition = Name "Joja Membership" |}

  let private createPrices prices seedSellPrice =
    match prices with
    | Pierre -> [ createPrice seedSellPrice "Pierre" ]
    | Joja pierrePrice -> [ pierrePrice; createJojaPrice pierrePrice ]
    | Oasis -> [ createPrice seedSellPrice "Oasis" ]
    | PierreAndJoja ->
      let pierrePrice = createPrice seedSellPrice "Pierre"
      [ pierrePrice
        createJojaPrice pierrePrice ]
    | PriceList list -> list
    | NoPrices -> List.empty

  let private initialItem =
    { Name = "Initial"
      BasePrice = -1
      Multiplier = None }

  let agri: Name<Multiplier> list = [ Name "Agriculturist" ]

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
            yield! createProducts products createdCropItem
            if seedMaker then
              SeedsFromSeedMaker None ]
          |> List.map (fun p -> p.Source, p)
          |> Map.ofList
        PriceFrom = createdSeed.BasePrice |> createPrices prices |> listToMap Price.key
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

  let extraCrops cropYield extraChance =
    1.0 / (1.0 - extraChance) + float cropYield - 2.0

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