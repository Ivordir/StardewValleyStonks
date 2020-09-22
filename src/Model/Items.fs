namespace StardewValleyStonks

open Types

[<Fable.Core.StringEnum>]
type RequirementPolicy =
  | [<CompiledName("Enforce")>] Enforce
  | [<CompiledName("Warn")>] Warn
  | [<CompiledName("Ignore")>] Ignore

module RequirementPolicy =
  let all =
    [ Enforce
      Warn
      Ignore ]

type Processor =
  { Name: string
    Selected: bool
    SkillUnlock: SkillUnlock option
    PreservesQuality: bool }
  member this.Toggle = { this with Selected = not this.Selected }
  member this.TogglePreservesQuality = { this with PreservesQuality = not this.PreservesQuality }

module Processor =
  let name processor = processor.Name

  let nameOf = toNameOf name

  let private createWith unlock name =
    { Name = name
      Selected = true
      SkillUnlock = unlock
      PreservesQuality = false }

  let create name skill level = createWith (Some <| SkillUnlock.create skill level) name
  let createNoUnlock = createWith None


type RawMultiplier =
  { Name: string
    Value: float
    Selected: bool }

module RawMultiplier =
  let name (multiplier: RawMultiplier) = multiplier.Name
  
  let nameOf = toNameOf name

  let create name value =
    { Name = name
      Value = value 
      Selected = false }

type ProfessionMultiplier =
  { Skill: Skills
    Profession: NameOf<Profession>
    Value: float }

module ProfessionMultiplier =
  let name multiplier = ofName multiplier.Profession

  let nameOf multiplier = multiplier.Profession

  let create skill name value =
      { Skill = skill
        Profession = Name name
        Value = value }

type Multiplier =
  | Raw of NameOf<RawMultiplier>
  | Profession of Skills * NameOf<Profession>

module Multiplier =
  let name = function
    | Raw r -> ofName r
    | Profession (_, p) -> ofName p

  let nameOf = toNameOf name

  let raw = Name >> Raw

  let profession skill profession = Profession (skill, Name profession)

  let tiller = profession Farming "Tiller"
  let agri = profession Farming "Agriculturist"
  let artisan = profession Farming "Artisan"

type Item =
  { Name: string
    BasePrice: int
    Multiplier: Multiplier option }

module Item =
  let name item = item.Name

  let nameOf = toNameOf name

  let initial =
    { Name = "initial"
      BasePrice = -1
      Multiplier = None }

  let private createWith multiplier name basePrice =
    { Name = name
      BasePrice = basePrice
      Multiplier = multiplier }

  let create = createWith None
  let createCrop = createWith (Some Multiplier.tiller)
  let createArtisan = createWith (Some Multiplier.artisan)

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
  | CreateAndAlso of CreateProducts * Product list
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

  let unitOutputAmount = function
    | FromProcess _ -> 1.0
    | RatioProcess r -> r.OutputAmount

  let outputAmount inputAmount = function
    | FromProcess _ -> 1.0
    | RatioProcess r -> inputAmount * r.OutputAmount / float r.InputAmount

  let processorOverride = function
    | FromProcess p -> p.ProcessorOverride
    | RatioProcess r -> r.Process.ProcessorOverride

  let create = Process.create >>| FromProcess

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
    | Juice -> createKeg (cropItem.Name + " Juice") (cropItem.BasePrice |> apply 2.25)

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
    | CreateAndAlso (products, list) -> createAll cropItem products @ list
    | NoProduct -> []