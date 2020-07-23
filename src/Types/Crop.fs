module Crop

open Types

type Processor =
  { Name: string
    Selected: bool
    Conditions: Condition list
    PreservesQuality: bool }
  member this.Toggle = { this with Selected = not this.Selected }
  static member Initial =
    { Name = "Initial"
      Selected = true
      Conditions = List.empty
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

type Multiplier =
  | Multiplier of Name: string * Value: float * Selected: bool
  | Profession of Skill: Name<Skill> * Profession: Name<Profession> * Value: float

let applyMultiplier value multiplier =
  int (float value * multiplier)

type Item =
  { Name: string
    BasePrice: int
    Multiplier: Name<Multiplier> option }

type Process =
  | UseItem of Override: bool option // * Name<Processor>?
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
  | SeedMaker of Override: bool option
  member this.InputAmount =
    match this with
    | UseItem _ | Process _ | SeedMaker _ -> 1
    | RatioProcess r -> r.InputAmount
   member this.Processor =
    match this with
    | UseItem _ -> Name "Raw Crop"
    | Process p -> p.Processor
    | RatioProcess r -> r.Processor
    | SeedMaker _ -> Name "Seed Maker"
  // member this.Output =
  //   match this with
  //   | UseItem _ -> Item.Initial
  //   | Process p -> p.Output
  //   | RatioProcess r -> r.Output
  // member this.OutputAmount =
  //   match this with
  //   | UseItem _ | Process _ -> 1.0
  //   | RatioProcess r -> r.OutputAmount

let seedMakerConditions = [ SkillLevel (Name "Farming", 9) ]

type Replant =
  | BuySeeds
  | SeedMaker
  | SeedOrCrop //of Name<Item>?
  member this.Name =
    match this with
    | BuySeeds -> "Buy Seeds"
    | SeedMaker -> "Seed Maker"
    | SeedOrCrop -> "Harvested Crop or Seed"
  static member List =
    [ BuySeeds
      SeedMaker
      SeedOrCrop ]

type HarvestedItemData =
  { Item: Item
    Amount: float
    Processes: Map<Name<Processor>, Process>
    Replants: Map<Replant, bool option> }

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
    HarvestedItems: Map<Name<Item>, HarvestedItemData> }
  member this.Toggle = { this with Selected = not this.Selected }
  member this.GrowthTimeWith speed =
    if speed = 0.0 then
      this.TotalGrowthTime
    else
      let growthStages = Array.ofList this.GrowthStages
      let mutable maxReduction = (convertToF32AndBack speed) * (float this.TotalGrowthTime) |> ceil |> int
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

let private initialItem =
  { Name = "Initial"
    BasePrice = -1
    Multiplier = None }

let private initialCrop =
  { Name = "Initial"
    Selected = true
    Seasons = Set.empty
    GrowthStages = List.empty
    TotalGrowthTime = -1
    RegrowTime = None //safe to edit later in generation
    GrowthMultipliers = agri //safe to edit later
    Item = initialItem
    Seed = initialItem
    PriceFrom = Map.empty
    Processes = Map.empty
    Replants = Map.empty
    IsGiantCrop = false //safe to edit later
    HasDoubleCropChance = true //safe to edit later
    ExtraCrops = 0.0 //safe to edit later
    HarvestedItems = Map.empty //safe to add to later
     }

let genCrop
  name
  seasons
  growthStages
  genCropItem
  genSeed
  (genProducts: GenerateProducts)
  (genPrices: GeneratePrices)
  seedMaker =
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
        UseItem None::(genProducts.Generate cropItem)
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