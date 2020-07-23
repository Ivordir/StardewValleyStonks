module Crop

open Types

// let private union (map1: Map<'a, 'b>) (map2: Map<'a, 'b>) =
//   if map1.IsEmpty then
//     map2
//   elif map2.IsEmpty then
//     map1
//   else
//     Map.fold (fun s k v -> Map.add k v s) map1 map2

type Replant =
  | BuySeeds
  | SeedOrCrop
  | SeedMaker

type HarvestedItemData =
  { Item: Item
    Amount: float
    Processes: Map<Name<Processor>, Process>
    Replants: Map<Name<Processor>, Replant> }

open Fable.Core.JsInterop

let private convertToF32AndBack(f64: float): float = importMember "./util.js"

type Crop =
  { Name: string
    Selected: bool
    Seasons: Set<Season>
    GrowthStages: int list
    TotalGrowthTime: int
    RegrowTime: int option
    GrowthMultipliers: Name<Multiplier> list
    Item: Item
    Seed: Item
    PriceFrom: Map<Name<Source>, Price>
    Processes: Map<Name<Processor>, Process>
    Replants: Map<Replant, bool option>
    IsGiantCrop: bool
    HasDoubleCropChance: bool
    ExtraCrops: float
    HarvestItems: Map<Name<Item>, HarvestedItemData> }
  member this.Toggle = { this with Selected = not this.Selected }
  member this.GrowthTimeWith speed =
    if speed = 0.0 then
      this.TotalGrowthTime
    else
      let growthStages = Array.ofList this.GrowthStages
      let mutable maxReduction = ((convertToF32AndBack speed) * (float this.TotalGrowthTime)) |> ceil |> int
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
      this.TotalGrowthTime - daysReduced

type GenerateCropItem =
  | SellPrice of int
  | Item of Item

type GenerateSeed =
  | SeedSell of int
  | Seed of Item
  | CropItem

let private genProcess processor outputName price =
   Process
     {| Processor = Name processor
        Output =
          { Name = outputName
            BasePrice = price
            Multiplier = Some (Name "Artisan") }
        Override = None |}

type KegProduct =
  | Wine
  | Juice
  member this.Generate (cropItem: Item) =
    match this with
    | Wine -> genProcess "Keg" (cropItem.Name + " Wine") (cropItem.BasePrice * 3)
    | Juice -> genProcess "Keg" (cropItem.Name + " Juice") (float cropItem.BasePrice * 2.25 |> int)

type JarProduct =
  | Jam
  | Pickle
  member this.Generate (cropItem: Item) =
    match this with
    | Jam -> genProcess "Preserves Jar" (cropItem.Name + " Jam") (cropItem.BasePrice * 2 + 50)
    | Pickle -> genProcess "Preserves Jar" ("Pickeled " + cropItem.Name) (cropItem.BasePrice * 2 + 50)

type GenerateProducts =
  | Fruit
  | Vegetable
  | Keg of KegProduct
  | Jar of JarProduct
  | ProductList of Process list
  | GenerateAndList of GenerateProducts * Process list
  | NoProduct
  member this.Generate cropItem =
    match this with
    | Fruit -> [ Wine.Generate cropItem; Jam.Generate cropItem ]
    | Vegetable -> [ Juice.Generate cropItem; Pickle.Generate cropItem ]
    | Keg product -> [ product.Generate cropItem ]
    | Jar product -> [ product.Generate cropItem ]
    | ProductList list -> list
    | GenerateAndList (gen, list) -> gen.Generate cropItem @ list
    | NoProduct -> []

let private genPrice basePrice name =
  Price
    {| Value = basePrice * 2
       Source = Name name
       Conditions = List.empty
       Override = None |}

let private genJoja (pierrePrice: Price) =
  MatchPrice
    {| Value = float pierrePrice.Value * 1.25 |> int
       Source = Name "Joja"
       Conditions = List.empty
       Override = None
       MatchSource = Name "Pierre"
       MatchCondition = Name "Joja Membership" |}

type GeneratePrices =
  | Pierre
  | Joja of PierrePrice: Price
  | Oasis
  | PierreAndJoja
  | PriceList of Price list
  | NoPrices
  member this.Generate basePrice =
    match this with
    | Pierre -> [ genPrice basePrice "Pierre" ]
    | Joja pierrePrice -> [ pierrePrice; genJoja pierrePrice ]
    | Oasis -> [ genPrice basePrice "Oasis" ]
    | PierreAndJoja ->
      let pierrePrice = genPrice basePrice "Pierre"
      [ pierrePrice
        genJoja pierrePrice ]
    | PriceList list -> list
    | NoPrices -> []

let agri: Name<Multiplier> list = [ Name "Agriculturist" ]

let private initialCrop =
  { Name = "Initial"
    Selected = true
    Seasons = Set.empty
    GrowthStages = List.empty
    TotalGrowthTime = -1
    RegrowTime = None //can edit later
    GrowthMultipliers = agri //can edit later
    Item = Item.Initial
    Seed = Item.Initial
    PriceFrom = Map.empty
    Processes = Map.empty
    Replants = Map.empty
    IsGiantCrop = false //can edit later
    HasDoubleCropChance = true //can edit later
    ExtraCrops = 0.0 //can edit later
    HarvestItems = Map.empty //can add to later
     }

let genCrop
  name
  seasons
  growthStages
  genCropItem
  genSeed
  (genProducts: GenerateProducts)
  (genPrices: GeneratePrices)
  seedMaker
  : Crop =
  let cropItem =
    match genCropItem with
    | SellPrice price ->
        { Name = name
          BasePrice = price
          Multiplier = Some (Name "Tiller") }
    | Item item -> item
  let seed =
    match genSeed with
      | SeedSell price ->
          { Name = name + " Seeds"
            BasePrice = price
            Multiplier = None }
      | Seed item -> item
      | CropItem -> cropItem
  let priceFrom = seed.BasePrice |> genPrices.Generate |> priceListToMap
  { initialCrop with
      Name = name
      Seasons = set seasons
      GrowthStages = growthStages
      TotalGrowthTime = List.sum growthStages
      Item = cropItem
      Seed = seed
      Processes = //add seedmaker
        UseItem
          {| Processor = Name "Raw Crop"
             Override = None |}
        :: (genProducts.Generate cropItem)
        |> List.map (fun p -> p.Processor, p)
        |> Map.ofList
      PriceFrom = priceFrom
      Replants =
        [ if not priceFrom.IsEmpty then BuySeeds
          if genSeed = CropItem then SeedOrCrop
          if seedMaker then SeedMaker ]
        |> List.map (fun r -> r, None)
        |> Map.ofList }

let extraCrops cropYield extraChance =
  1.0 / (1.0 - extraChance) + float cropYield - 2.0