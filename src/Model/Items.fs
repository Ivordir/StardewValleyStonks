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
  
  let all =
    [ Profession
        {| Skill = Name "Farming"
           Profession = Name "Tiller"
           Value = 1.1 |}
      Profession
        {| Skill = Name "Farming"
           Profession = Name "Artisan"
           Value = 1.4 |}
      Profession
        {| Skill = Name "Farming"
           Profession = Name "Agriculturist"
           Value = 0.1 |}
      Profession
        {| Skill = Name "Foraging"
           Profession = Name "Gatherer"
           Value = 1.2 |}
      Multiplier
        {| Name = "Irrigated"
           Selected = false
           Value = 1.1 |} ]

  let agri: NameOf<Multiplier> list = [ Name "Agriculturist" ]

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

  let create name basePrice =
    { Name = name
      BasePrice = basePrice
      Multiplier = None }

  let createCrop name sellPrice =
    { Name = name
      BasePrice = sellPrice
      Multiplier = Some (Name "Tiller") }

type Product =
  | Process of
      {| Processor: NameOf<Processor>
         Output: Item
         Override: bool option |}
  | RatioProcess of
      {| InputAmount: int
         Processor: NameOf<Processor>
         Output: Item
         OutputAmount: float
         Override: bool option |}

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
    | Process p -> p.Override
    | RatioProcess r -> r.Override

  let create processor output =
    Process
      {| Processor = Name processor
         Output = output
         Override = None |}

  let createArtisan processor outputName price =
     Process
       {| Processor = Name processor
          Output =
            { Name = outputName
              BasePrice = price
              Multiplier = Some (Name "Artisan") }
          Override = None |}

  let createKeg = createArtisan "Keg"
  let createJar = createArtisan "Preserves Jar"

  let createKegProduct (cropItem: Item) = function
    | Wine -> createKeg (cropItem.Name + " Wine") (cropItem.BasePrice * 3)
    | Juice -> createKeg (cropItem.Name + " Juice") (float cropItem.BasePrice * 2.25 |> int)

  let createJarProduct (cropItem: Item) = function
    | Jam -> createJar (cropItem.Name + " Jam") (cropItem.BasePrice * 2 + 50)
    | Pickle -> createJar ("Pickeled " + cropItem.Name) (cropItem.BasePrice * 2 + 50)

  let rec createAll cropItem = function
    | Fruit -> [ createKegProduct cropItem Wine; createJarProduct cropItem Jam ]
    | Vegetable -> [ createKegProduct cropItem Juice; createJarProduct cropItem Pickle ]
    | Keg product -> [ createKegProduct cropItem product ]
    | Jar product -> [ createJarProduct cropItem product ]
    | ProductList list -> list
    | CreateAndList (products, list) -> createAll cropItem products @ list
    | NoProduct -> List.empty

  let oil = create "Oil Maker" (Item.create "Oil" 100)