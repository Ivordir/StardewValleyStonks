namespace StardewValleyStonks

[<Fable.Core.Erase>]
type RawMultiplier = RawMultiplier of name: string * value: float

module RawMultiplier =
  let inline name (RawMultiplier(name, _)) = name
  let inline value (RawMultiplier(_, value)) = value

type Multiplier =
  | Raw of RawMultiplier
  | Profession of skill: Skills * profession: Profession * value: float

module Multiplier =
  let name = function
    | Raw (RawMultiplier (name, _)) -> name
    | Profession (_, ProfessionName p, _) -> p

  let value = function
    | Raw (RawMultiplier (_, v)) -> v
    | Profession (_,_, v) -> v

  let apply multiplierActive multiplier =
    if multiplierActive multiplier
    then intMulti (value multiplier)
    else id

  let create = tuple2 >>| RawMultiplier
  let createProfession = tuple3 >>>| Profession


type Item =
  { Name: string
    BasePrice: int
    Multiplier: Multiplier option }

module Item =
  let name item = item.Name
  let basePrice item = item.BasePrice
  let multiplier item = item.Multiplier
  let price multiplierActive item =
    item.BasePrice |> Option.foldBack (Multiplier.apply multiplierActive) item.Multiplier

  let validPrice = positive

  let createWith multiplier name basePrice =
    { Name = name
      BasePrice = validPrice basePrice
      Multiplier = multiplier }

  let createMultiplier = Some >> createWith
  let create = createWith None



type PreservesQuality =
  | Always of bool
  | QualityProcessor of bool

[<Fable.Core.Erase>]
type ProcessorAmount =
  | QualityIndependent of float
  | QualityDependent of float array

type Processor =
  { Name: string
    PerservesQuality: PreservesQuality
    SkillUnlock: SkillUnlock option
    Amount: ProcessorAmount option }

module Processor =
  let name processor = processor.Name
  let nameKey: _ -> Processor name = name >> Name
  let nameOption = defaultMap "Raw Crop" name
  let unlock processor = processor.SkillUnlock
  let unlocked skills = unlock >> Option.forall (SkillUnlock.unlocked skills)
  let unlockedOption = unlocked >> Option.forall

  let preservesQualityData processor = processor.PerservesQuality
  let qualityProcessor = preservesQualityData >> function
    | QualityProcessor _ -> true
    | Always _ -> false
  let qualityProcessorOption = Option.exists qualityProcessor
  let preservesQuality qualityProducts = preservesQualityData >> function
    | Always b -> b
    | QualityProcessor q -> qualityProducts && q
  let preservesQualityOption = preservesQuality >> Option.forall

  let amountData processor = processor.Amount
  let amount (quality: Quality) = function
    | QualityIndependent a -> a
    | QualityDependent dist -> dist.[int quality]
  let amountOption quality = amountData >> Option.map (amount quality)

  let qualityProductsSelected = preservesQualityData >> function
    | QualityProcessor q -> q
    | Always _ -> false

  let toggleQualityProcessor processor =
    { processor with
        PerservesQuality =
          match processor.PerservesQuality with
          | QualityProcessor b -> QualityProcessor <| not b
          | _ -> invalidArg "processor" "The processor is not a quality processor." }

  let withAmount amount processor =
    { processor with Amount = amount }

  let createWith amount preservesQuality name unlock =
    { Name = name
      SkillUnlock = unlock
      PerservesQuality = preservesQuality
      Amount = amount }

  let create name preservesQuality = createWith None (QualityProcessor preservesQuality) name



// Override/modify the processor amount.
type ProductAmount =
  | Ratio of input: int * output: int
  | Additional of float

type Product =
  { Item: Item
    Processor: Processor name option
    Amount: ProductAmount option }

module Product =
  let item product = product.Item
  let name = item >> Item.name
  let processor product = product.Processor
  //let valid processorActive = processor >> Option.forall processorActive
  let amount product = product.Amount

  let price itemPrice preservesQuality quality product =
    itemPrice product.Item
    |>  if preservesQuality product.Processor
        then Quality.multiply quality
        else id

  let private outputAmount processors quality =
    processor
    >> Option.bind (processors >> Processor.amountOption quality)
    >> defaultValue 1.0

  let unitOutput processors quality product =
    match product.Amount with
    | Some (Ratio (i, o)) -> float o / float i
    | Some (Additional a) -> outputAmount processors quality product + a
    | None -> outputAmount processors quality product

  let unitProfit priceFun outputFun (quality: Quality) (product: Product) =
    (priceFun quality product |> float) * outputFun quality product

  let validInputAmount = max 1
  let validOutputAmount = max 1

  let createWith amount processor item =
    { Item = item
      Processor = processor
      Amount = amount }
  let create = Some >> createWith None
  let createRaw = createWith None None

  let createRatio processor inputAmount outputAmount =
    createWith (Some <| Ratio (validInputAmount inputAmount, validOutputAmount outputAmount)) (Some processor)
