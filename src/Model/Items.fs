namespace StardewValleyStonks

open FSharp.Data.Adaptive
open Adaptive
open Util

module Mod =
  let qualityProducts = cval false
  let qualitySeedMaker = cval false
  let qualitySeedMakerAmounts =
    [ Normal, cval 2
      Silver, cval 3
      Gold, cval 4
      Iridium, cval 5 ]
    |> Map.ofList

type OutputQuality =
  | Constant of bool
  | QualityProducts of bool cval

[<CustomEquality; CustomComparison>]
type Processor =
  { Name: string
    SkillUnlock: SkillUnlock option
    OutputQuality: OutputQuality
    PreservesQuality: bool aval }
  override this.Equals(x) =
    match x with
    | :? Processor as processor -> this.Name = processor.Name
    | _ -> false
  interface System.IEquatable<Processor> with
    member this.Equals(x) = this.Name = x.Name
  interface System.IComparable with
    member this.CompareTo(x) =
      match x with
      | :? Processor as processor -> compare this.Name processor.Name
      | _ -> invalidArg "x" "x is not a processor."
  interface System.IComparable<Processor> with
    member this.CompareTo(x) = compare this.Name x.Name
  override this.GetHashCode() = hash this.Name

module Processor =
  let name processor = processor.Name
  let skillUnlock processor = processor.SkillUnlock
  let outputQuality processor = processor.OutputQuality
  let preservesQuality processor = processor.PreservesQuality
  let preservesQualityOption = defaultMap !@true preservesQuality
  let unlocked = skillUnlock >> defaultMap !@true SkillUnlock.unlocked

  let private createWithQuality preservesQuality outputQuality name unlock =
    { Name = name
      SkillUnlock = unlock
      OutputQuality = outputQuality
      PreservesQuality = preservesQuality }

  let createConstant name preservesQuality = createWithQuality !@preservesQuality (Constant preservesQuality) name
  let createQuality name preservesQuality =
    let qualityProducts = cval preservesQuality
    createWithQuality (qualityProducts .&& Mod.qualityProducts) (QualityProducts qualityProducts) name

module Processors =
  open Processor
  let private withFarmLvl = SkillUnlock.create Skills.farming >> Some

  let preservesJar = createQuality "Preserves Jar" true (withFarmLvl 4)
  let keg = createQuality "Keg" true (withFarmLvl 8)
  let oilMaker = createQuality "Oil Maker" true (withFarmLvl 8)
  let mill = createQuality "Mill" false None
  let seedMaker = createConstant "Seed Maker" false (withFarmLvl 9)

  let all =
    [ preservesJar
      keg
      oilMaker
      mill
      seedMaker ]
  
  let allOption = listWithNone all

  let qualityProducts = all |> List.filter (fun p ->
    match p.OutputQuality with
    | QualityProducts _ -> true
    | _ -> false)


type RawMultiplier =
  { Name: string
    Value: float aval
    Selected: bool cval }

module RawMultiplier =
  let name multiplier = multiplier.Name
  let value multiplier = multiplier.Value
  let selected multiplier = multiplier.Selected

  let create name value =
    { Name = name
      Value = !@value
      Selected = cval false }



type ProfessionMultiplier =
  { Profession: Profession
    Value: float aval }

module ProfessionMultiplier =
  let profession multiplier = multiplier.Profession
  let name = profession >> Profession.name
  let active = profession >> Profession.active
  let value multiplier = multiplier.Value

  let create profession value =
    { Profession = profession
      Value = !@value }



type Multiplier =
  | Raw of RawMultiplier
  | Profession of ProfessionMultiplier

module Multiplier =
  let name = function
    | Raw r -> r.Name
    | Profession p -> ProfessionMultiplier.name p
  let value = function
    | Raw r -> r.Value
    | Profession p -> p.Value
  let active = function
    | Raw r -> !>r.Selected
    | Profession p -> ProfessionMultiplier.active p

  let sum = memoize <| AList.ofList >> AList.filterA active >> AList.sumByA value

  let createRaw = RawMultiplier.create >>| Raw
  let createProfession = ProfessionMultiplier.create >>| Profession

module Multipliers =
  open Skills
  open Multiplier

  let tiller = createProfession farming.Lvl5Profession 1.1
  let agri = createProfession farming.Lvl10ProfessionA 0.1
  let artisan = createProfession (Option.get farming.Lvl10ProfessionB) 1.4

  let irrigated = createRaw "Irrigated" 0.25
  let bearsKnowledge = createRaw "Bear's Knowledge" 3.0

  let growth =
    [ irrigated ]

  let raw =
    [ bearsKnowledge ]


type Item =
  { Name: string
    BasePrice: int aval
    Multiplier: Multiplier option
    Price: int aval }

module Item =
  let name item = item.Name
  let basePrice item = item.BasePrice
  let multiplier item = item.Multiplier
  let price item = item.Price

  let aCreateMultiplier multiplier name basePrice =
    { Name = name
      BasePrice = basePrice
      Multiplier = multiplier
      Price =
        match multiplier with
        | Some m -> aCondApply basePrice (Multiplier.value m) (Multiplier.active m)
        | None -> basePrice }

  open Multipliers
  type private CreateItem = string -> int aval -> Item

  let aCreate: CreateItem = aCreateMultiplier None
  let aCreateCrop: CreateItem = aCreateMultiplier (Some tiller)
  let aCreateArtisan: CreateItem = aCreateMultiplier (Some artisan)

  let createMultiplier multiplier name basePrice = aCreateMultiplier multiplier name !@basePrice

  let create = createMultiplier None
  let createCrop = createMultiplier (Some tiller)
  let createArtisan = createMultiplier (Some artisan)



type ProductBase =
  { Processor: Processor option
    Item: Item
    ProcessorOverride: Override cval
    Selected: bool aval }

type Product =
  | Product of ProductBase
  | RatioProduct of
      {| Product: ProductBase
         InputAmount: int
         OutputAmount: int
         OutputPerInput: float |}
  | QualityProduct of
      {| //InputAmount: int aval = 1
         Product: ProductBase
         OutputAmount: Map<Quality, float aval> |}

module Product =
  let private itemBase product = product.Item
  let private processorBase product = product.Processor
  let private processorOverrideBase product = product.ProcessorOverride
  let private selectedBase product = product.Selected

  let productBase = function
    | Product p -> p
    | RatioProduct r -> r.Product
    | QualityProduct q -> q.Product

  let item = productBase >> itemBase
  let name = item >> Item.name
  let processor = productBase >> processorBase
  let processorOverride = productBase >> processorOverrideBase
  let selected = productBase >> selectedBase

  let inputAmount = function
    | Product _ -> 1
    | RatioProduct r -> r.InputAmount
    | QualityProduct _ -> 1

  let unitOutputAmount quality = function
    | Product _ -> !@1.0
    | RatioProduct r -> !@r.OutputPerInput
    | QualityProduct q -> q.OutputAmount.[quality]

  let private qualityPrice product = function
    | Normal -> product.Item.Price
    | quality -> aCondApply product.Item.Price !@(Quality.multiplier quality) (Processor.preservesQualityOption product.Processor)

  let unitProfit quality = function
    | Product p -> qualityPrice p quality |> aFloat
    | RatioProduct r -> (qualityPrice r.Product quality |> aFloat) .* !@r.OutputPerInput
    | QualityProduct q -> (qualityPrice q.Product quality |> aFloat) .* q.OutputAmount.[quality]

  let setOverride = processorOverride >> setValue

  let private bestProfit quality = AList.filterA selected >> AList.mapA (unitProfit quality) >> AList.tryMax
  let test quality bestProfit product =
    match product with
    | RatioProduct r ->
        printf "%A" r.Product.Item
    | _ -> ()
    let p = unitProfit quality product
    p .= aDefaultValue -1.0 bestProfit
  let private bestProducts bestProfit quality = AList.filterA (unitProfit quality >> (.=) (aDefaultValue -1.0 bestProfit))
  let bestProfitAndProducts list =
    let products = AList.ofList list
    let bestProfits = Quality.all |> mapOfKeys (flip bestProfit products)
    bestProfits,
    Quality.all |> mapOfKeys (fun quality -> bestProducts bestProfits.[quality] quality products)

  let private createBaseWith processorSelected processor item =
    let over = cval None
    { Processor = processor
      Item = item
      ProcessorOverride = over
      Selected =
        let selected = processorSelected processor |> aSelectedOverride over
        match processor with
        | Some p -> Processor.unlocked p .&& selected
        | None -> selected }

  let private createWith selected = Some >> createBaseWith selected >>| Product
  let createRawWith selected = createBaseWith selected None >> Product
  let createItemWith selected processor = Item.create >>| createWith selected processor
  let createTillerWith selected processor = Item.createCrop >>| createWith selected processor
  let createArtisanWith selected processor = Item.createArtisan >>| createWith selected processor

  let aCreateArtisanWith selected processor = Item.aCreateArtisan >>| createWith selected processor


  let private createRatioWith processorSelected processor output inputAmount outputAmount =
    RatioProduct
      {| Product = createBaseWith processorSelected (Some processor) output
         InputAmount = inputAmount
         OutputAmount = outputAmount
         OutputPerInput = float outputAmount / float inputAmount |}
  //let createRatioItemWith selected processor = Item.create >>| createRatioWith selected processor
  //the runtime did not like the version above for some reason, gave undefined errors
  let createRatioItemWith selected processor name price = createRatioWith selected processor (Item.create name price)


  let createQualityWith selected processor outputAmounts output =
    QualityProduct
      {| Product = createBaseWith selected (Some processor) output
         OutputAmount = outputAmounts |}
