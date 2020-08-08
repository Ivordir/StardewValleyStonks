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
  let name processor = processor.Name

  let nameOf = toNameOf name

  let create name requirements =
    { Name = name
      Selected = true
      Requirements = requirements
      PreservesQuality = false }

  let all =
    [ create "Preserves Jar" [ SkillLevel (Name "Farming", 4) ]
      create "Keg" [ SkillLevel (Name "Farming", 8) ]
      create "Oil Maker" [ SkillLevel (Name "Farming", 8) ]
      create "Mill" List.empty ]

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
  | RawMultiplier of
      {| Name: string
         Value: float
         Selected: bool |}
  | Profession of
      {| Skill: NameOf<Skill>
         Profession: NameOf<Profession>
         Value: float |}

module Multiplier =
  let name = function
    | RawMultiplier m -> m.Name
    | Profession p -> ofName p.Profession

  let nameOf = toNameOf name

  let isRawMultiplier = function
    | RawMultiplier _ -> true
    | Profession _ -> false

  let create name value =
    RawMultiplier
      {| Name = name
         Selected = false
         Value = value |}

  let createProfession skill name value =
    Profession
      {| Skill = Name skill
         Profession = Name name
         Value = value |}

  let all =
    [ createProfession "Farming" "Tiller" 1.1
      createProfession "Farming" "Artisan" 1.4
      createProfession "Farming" "Agriculturist" 0.1
      createProfession "Foraging" "Gatherer" 1.2
      create "Irrigated" 1.1
      create "Bear's Knowledge" 3.0 ]

  let agri: Set<NameOf<Multiplier>> = set [ Name "Agriculturist" ]

type Item =
  { Name: string
    BasePrice: int
    Multiplier: NameOf<Multiplier> option }

module Item =
  let name item = item.Name

  let nameOf = toNameOf name

  let initial =
    { Name = "initial"
      BasePrice = -1
      Multiplier = None }

  let createWith multiplier name basePrice =
    { Name = name
      BasePrice = basePrice
      Multiplier = multiplier }

  let create = createWith None
  let createCrop = createWith (Some <| Name "Tiller")
  let createArtisan = createWith (Some <| Name "Artisan")

type Process =
  { Processor: NameOf<Processor>
    Output: Item
    ProcessorOverride: Override }

module Process =
  let create processor output =
    { Processor = Name processor
      Output = output
      ProcessorOverride = None }

type Product =
  | FromProcess of Process
  | RatioProcess of
      {| InputAmount: int
         Process: Process
         OutputAmount: float |}

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

module Product =
  let output = function
    | FromProcess p -> p.Output
    | RatioProcess r -> r.Process.Output

  let nameOf = output >> Item.nameOf

  let processor = function
    | FromProcess p -> p.Processor
    | RatioProcess r -> r.Process.Processor

  let inputAmount = function
    | FromProcess _ -> 1
    | RatioProcess r -> r.InputAmount

  let outputAmount = function
    | FromProcess _ -> 1.0
    | RatioProcess r -> r.OutputAmount

  let processorOverride = function
    | FromProcess p -> p.ProcessorOverride
    | RatioProcess r -> r.Process.ProcessorOverride

  let create processor output = FromProcess <| Process.create processor output

  let createRatio inputAmount processor output outputAmount =
    RatioProcess
      {| InputAmount = inputAmount
         Process = Process.create processor output
         OutputAmount = outputAmount |}

  let createArtisan processor outputName price = create processor (Item.createArtisan outputName price)

  let createKeg = createArtisan "Keg"
  let createJar = createArtisan "Preserves Jar"

  let createKegProduct (cropItem: Item) = function
    | Wine -> createKeg (cropItem.Name + " Wine") (cropItem.BasePrice * 3)
    | Juice -> createKeg (cropItem.Name + " Juice") (float cropItem.BasePrice * 2.25 |> int)

  let createJarProduct (cropItem: Item) = function
    | Jam -> createJar (cropItem.Name + " Jam") (cropItem.BasePrice * 2 + 50)
    | Pickle -> createJar ("Pickeled " + cropItem.Name) (cropItem.BasePrice * 2 + 50)

  let rec createAll cropItem = function
    | Fruit ->
        [ createKegProduct cropItem Wine
          createJarProduct cropItem Jam ]
    | Vegetable ->
        [ createKegProduct cropItem Juice
          createJarProduct cropItem Pickle ]
    | Keg product -> [ createKegProduct cropItem product ]
    | Jar product -> [ createJarProduct cropItem product ]
    | ProductList list -> list
    | CreateAndList (products, list) -> createAll cropItem products @ list
    | NoProduct -> List.empty

  let oil = create "Oil Maker" (Item.create "Oil" 100)