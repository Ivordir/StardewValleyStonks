module StardewValleyStonks.Json

#nowarn "25"

#if FABLE_COMPILER
open Thoth.Json
#else
open Thoth.Json.Net
#endif

let [<Literal>] private SeedPriceVendorField = "Vendor"
let [<Literal>] private SeedPricePriceField = "Price"
let [<Literal>] private SeedPriceTypeField = "Type"
let [<Literal>] private FixedSeedPrice = "Fixed"
let [<Literal>] private ScalingSeedPrice = "Scaling"

module Encode =
  let mapSeq encoder (seq: _ seq) = seq |> Seq.map encoder |> Encode.seq

  let vendor (VendorName vendor) = Encode.string vendor

  let fertilizerName (FertName name) = Encode.string name

  let fertilizer = Encode.Auto.generateEncoder<Fertilizer> ()

  let quality = Encode.Auto.generateEncoder<Quality> ()

  let farming = Encode.Auto.generateEncoder<Farming> ()

  let foraging = Encode.Auto.generateEncoder<Foraging> ()

  let skills = Encode.Auto.generateEncoder<Skills> ()

  let itemId (item: ItemId) = uint item |> Encode.uint32

  let processor (ProcessorName name) = Encode.string name

  let product = function
    | Jam -> Encode.string (nameof Jam)
    | Wine -> Encode.string (nameof Wine)
    | Pickles -> Encode.string (nameof Pickles)
    | Juice -> Encode.string (nameof Juice)
    | SeedsFromSeedMaker seed ->
      Encode.object [ nameof SeedsFromSeedMaker, itemId seed ]
    | Processed p ->
      Encode.object [
        nameof p.Item, itemId p.Item
        nameof p.Processor, processor p.Processor
        if p.Ratio <> None then
          nameof p.Ratio, Encode.tuple2 Encode.uint32 Encode.uint32 (Option.get p.Ratio)
      ]

  let category = Encode.Auto.generateEncoder<Category> ()

  let multipliers (multipliers: Multipliers) =
    Encode.object [
      nameof multipliers.ProfitMargin, Encode.float multipliers.ProfitMargin
      nameof multipliers.BearsKnowledge, Encode.bool multipliers.BearsKnowledge
      nameof multipliers.ForagedFruitTillerOverrides, multipliers.ForagedFruitTillerOverrides |> mapSeq itemId
    ]

  let modData (modData: ModData) =
    Encode.object [
      nameof modData.QualityProducts, Encode.bool modData.QualityProducts
      nameof modData.QualityProcessors, modData.QualityProcessors |> mapSeq processor
    ]

  let item = Encode.Auto.generateEncoder<Item> ()

  let season (s: Season) = Season.name s |> Encode.string

  // Fable can't parse flag enums, transform into a sequence instead.
  let seasons (seasons: Seasons) =
    Season.all
    |> Seq.filter (flip Seasons.contains seasons)
    |> mapSeq season

  let date (Date (season, day)) = Encode.tuple2 Encode.string Encode.uint32 (Season.name season, day)

  let seedId (seed: SeedId) = uint seed |> Encode.uint32

  let seedItemIdPair (seed: SeedId, item: ItemId) = $"{seed},{item}"

  let seedPrice = function
    | FixedPrice (v, p) ->
      Encode.object [
        SeedPriceVendorField, vendor v
        SeedPricePriceField, Encode.uint32 p
        SeedPriceTypeField, Encode.string FixedSeedPrice
      ]
    | ScalingPrice (v, None) ->
      Encode.object [
        SeedPriceVendorField, vendor v
        SeedPriceTypeField, Encode.string ScalingSeedPrice
      ]
    | ScalingPrice (v, Some p) ->
      Encode.object [
        SeedPriceVendorField, vendor v
        SeedPricePriceField, Encode.uint32 p
        SeedPriceTypeField, Encode.string ScalingSeedPrice
      ]

  let private growthDataFields (data: GrowthData) = [
    nameof data.Seasons, seasons data.Seasons
    nameof data.Stages, data.Stages |> mapSeq Encode.uint32
    if data.Paddy then nameof data.Paddy, Encode.bool true
    nameof data.Seed, seedId data.Seed
  ]

  let growthData = growthDataFields >> Encode.object

  let cropAmount (amount: CropAmount) =
    let single = CropAmount.singleAmount
    Encode.object [
      if amount.MinCropYield <> single.MinCropYield then
        nameof amount.MinCropYield, Encode.uint32 amount.MinCropYield
      if amount.MaxCropYield <> single.MaxCropYield then
        nameof amount.MaxCropYield, Encode.uint32 amount.MaxCropYield
      if amount.ExtraCropChance <> single.ExtraCropChance then
        nameof amount.ExtraCropChance, Encode.float amount.ExtraCropChance
      if amount.CanDouble <> single.CanDouble then
        nameof amount.CanDouble, Encode.bool amount.CanDouble
      if amount.Giant <> single.Giant then
        nameof amount.Giant, Encode.bool amount.Giant
      if amount.FarmLevelsPerYieldIncrease <> single.FarmLevelsPerYieldIncrease then
        nameof amount.FarmLevelsPerYieldIncrease, Encode.uint32 amount.FarmLevelsPerYieldIncrease
      if amount.FarmingQualities <> single.FarmingQualities then
        nameof amount.FarmingQualities, Encode.bool amount.FarmingQualities
    ]

  let cropAmountSettings = Encode.Auto.generateEncoder<CropAmountSettings> ()

  let farmCrop (crop: FarmCrop) =
    Encode.object [
      yield! growthDataFields crop.Growth
      nameof crop.Item, itemId crop.Item
      if crop.Amount <> CropAmount.singleAmount then
        nameof crop.Amount, cropAmount crop.Amount
      if crop.ExtraItem <> None then
        nameof crop.ExtraItem, Encode.tuple2 itemId Encode.float (Option.get crop.ExtraItem)
    ]

  let forageCrop (crop: ForageCrop) =
    Encode.object [
      yield! growthDataFields crop.Growth
      nameof crop.Items, mapSeq itemId crop.Items
      nameof crop.SeedRecipeUnlockLevel, Encode.uint32 crop.SeedRecipeUnlockLevel
    ]


module Decode =
  module Helpers =
    let validate desc predicate decoder : _ Decoder = fun path value ->
      match decoder path value with
      | Ok y ->
        if predicate y
        then Ok y
        else Error (path, BadPrimitive (desc, value))
      | Error e -> Error e

    let checkOk validator decoder : _ Decoder = fun path value ->
      match decoder path value with
      | Ok y ->
        match validator y with
        | Ok y -> Ok y
        | Error e -> Error (path, BadPrimitive (e, value))
      | Error e -> Error e

    let greater desc lowerBound =
      validate $"%s{desc} greater than {lowerBound}" (fun x -> x > lowerBound)

    let greaterOrEqual desc lowerBound =
      validate $"%s{desc} greater than or equal to {lowerBound}" (fun x -> x >= lowerBound)

    let less desc upperBound =
      validate $"%s{desc} less than {upperBound}" (fun x -> x < upperBound)

    let lessOrEqual desc upperBound =
      validate $"%s{desc} less than or equal to {upperBound}" (fun x -> x <= upperBound)

    let inRange desc min max =
      validate $"%s{desc} in the range [{min}, {max}]" (fun x -> min <= x && x <= max)

    let natLessOrEqual upperBound = lessOrEqual "an integer" upperBound Decode.uint32

    let natGreaterOrEqual lowerBound = greaterOrEqual "an integer" lowerBound Decode.uint32

    let nonZeroNat = greater "an integer" 0u Decode.uint32

    let natInRange min max = inRange "a natural number" min max Decode.uint32

    let nonNegativeFloat = greaterOrEqual "a float" 0.0 Decode.float

    let nonZeroFloat = greater "a float" 0.0 Decode.float

    let floatInRange min max = inRange "a float" min max Decode.float

    let inline private tryParseInt<[<Measure>] ^u> (str: string): uint<'u> option =
      match System.UInt32.TryParse str with
      | true, value -> Some (value * 1u<_>)
      | _ -> None

    let parseItemId = tryParseInt<ItemNum>

    let parseSeedId = tryParseInt<SeedNum>


  open Helpers

  let private wrap (wrapper: 'a -> 'b) (decoder: 'a Decoder) =
    #if FABLE_COMPILER
    !!decoder : 'b Decoder
    #else
    decoder |> Decode.map wrapper
    #endif

  let private natMeasure<[<Measure>] 'u> =
    #if FABLE_COMPILER
    !!Decode.uint32 : uint<'u> Decoder
    #else
    Decode.uint32 |> Decode.map LanguagePrimitives.UInt32WithMeasure<'u>
    #endif

  let set decoder = Decode.array decoder |> Decode.map Set.ofArray

  let wrapKeys mapping (keyWrap: string -> 'k) (decodeValue: 'v Decoder) =
    #if FABLE_COMPILER
    (!!Decode.keyValuePairs decodeValue: ('k * 'v) list Decoder) |> Decode.map mapping
    #else
    Decode.keyValuePairs decodeValue |> Decode.map (List.map (fun (k, v) -> keyWrap k, v) >> mapping)
    #endif

  let private parseKeysWith (mapping: _ seq -> _) parseKey decodeValue =
    Decode.keyValuePairs decodeValue |> Decode.andThen (fun kvps ->
      let acc = ResizeArray ()
      let rec parseKeys = function
        | [] -> true
        | (key, value) :: rest ->
          match parseKey key with
          | Some key ->
            acc.Add (key, value)
            parseKeys rest
          | None -> false
      if parseKeys kvps
      then mapping acc |> Decode.succeed
      else fun path value -> Error (path, BadPrimitive ("a valid key", value)))

  let table keyWrap decodeValue = wrapKeys Table.ofSeq keyWrap decodeValue
  let tableParse parseKey decodeValue = parseKeysWith Table.ofSeq parseKey decodeValue
  let tableOfValues getKey decodeValue = Decode.array decodeValue |> Decode.map (Table.ofValues getKey)

  let mapTree keyWrap decodeValue = wrapKeys Map.ofSeq keyWrap decodeValue
  let mapTreeParse parseKey decodeValue = parseKeysWith Map.ofSeq parseKey decodeValue
  let mapOfValues getKey decodeValue = Decode.array decodeValue |> Decode.map (Map.ofValues getKey)

  let fertilizerName = Decode.string |> wrap FertName

  let fertilizer: Fertilizer Decoder =
    let u = Unchecked.defaultof<Fertilizer>
    Decode.object (fun get -> {
      Name = get.Required.Field (nameof u.Name) fertilizerName
      Quality = get.Optional.Field (nameof u.Quality) Decode.uint32 |> Option.defaultValue 0u
      Speed = get.Optional.Field (nameof u.Speed) nonNegativeFloat |> Option.defaultValue 0.0
    } )

  let quality: Quality Decoder = Decode.Enum.int |> validate "a defined quality" (fun q -> System.Enum.IsDefined (typeof<Quality>, q))

  let skills = Decode.Auto.generateDecoder<Skills> ()

  let vendor = Decode.Auto.generateDecoder<Vendor> ()

  let processor = Decode.Auto.generateDecoder<Processor> ()

  let itemId = natMeasure<ItemNum>
  let seedId = natMeasure<SeedNum>

  let product: Product Decoder =
    let (Processed processed) = Processed Unchecked.defaultof<_>
    Decode.oneOf [
      Decode.string |> Decode.andThen (function
        | nameof Jam -> Decode.succeed Jam
        | nameof Wine -> Decode.succeed Wine
        | nameof Pickles -> Decode.succeed Pickles
        | nameof Juice -> Decode.succeed Juice
        | _ -> Decode.fail $"Was not '{nameof Jam}', '{nameof Wine}', '{nameof Pickles}', or '{nameof Juice}'.")

      Decode.object (fun get ->
        SeedsFromSeedMaker (get.Required.Field (nameof SeedsFromSeedMaker) itemId))

      Decode.object (fun get -> Processed {|
        Item = get.Required.Field (nameof processed.Item) itemId
        Processor = get.Required.Field (nameof processed.Processor) processor
        Ratio = get.Optional.Field (nameof processed.Ratio) (Decode.tuple2 nonZeroNat nonZeroNat)
      |} )
    ]

  let item = Decode.Auto.generateDecoder<Item> ()

  let season =
    Decode.string |> Decode.andThen (fun str ->
      match System.Enum.TryParse str with
      | true, season ->
        match season with
        | Seasons.Spring
        | Seasons.Summer
        | Seasons.Fall
        | Seasons.Winter -> Season.ofSeasons season |> Decode.succeed
        | _ -> Decode.fail $"Undefined season: {season}"
      | _ -> Decode.fail $"Failed to parse season: {str}")

  // Fable can't parse flag enums, parse Season Block instead.
  let seasons = Decode.array season |> Decode.map Seasons.ofSeq

  let date: Date Decoder = Decode.tuple2 season (natInRange Date.firstDay Date.lastDay) |> wrap Date

  let seedPrice: SeedPrice Decoder =
    Decode.map3 tuple3
      (Decode.field SeedPriceVendorField vendor)
      (Decode.optional SeedPricePriceField Decode.uint32)
      (Decode.field SeedPriceTypeField Decode.string)
    |> Decode.andThen (fun (vendor, price, kind) -> fun path value ->
      match kind, price with
      | FixedSeedPrice, Some price -> Ok (FixedPrice (vendor, price))
      | FixedSeedPrice, None -> Error (path, BadField ($"an object with a field named '{SeedPricePriceField}'", value))
      | ScalingSeedPrice, Some price -> Ok (ScalingPrice (vendor, Some price))
      | ScalingSeedPrice, None -> Ok (ScalingPrice (vendor, None))
      | _ -> Error (path, BadPrimitive ($"a valid price type ('{FixedSeedPrice}' or '{ScalingSeedPrice}')", value)))

  let private growthDataFields =
    let u = Unchecked.defaultof<GrowthData>
    fun (get: Decode.IGetters) ->
      let stages = get.Required.Field (nameof u.Stages) (Decode.array nonZeroNat |> validate "growth stage with at least one stage" (Array.isEmpty >> not))
      let total =
        match Some stages with
        | None -> 0u
        | Some ind -> Array.natSum ind

      {
        Stages = stages
        TotalTime = total
        RegrowTime = get.Optional.Field (nameof u.RegrowTime) Decode.uint32
        Seasons = get.Required.Field (nameof u.Seasons) seasons
        Paddy = get.Optional.Field (nameof u.Paddy) Decode.bool |> Option.defaultValue false
        Seed = get.Required.Field (nameof u.Seed) seedId
      }

  let growthData: GrowthData Decoder = Decode.object growthDataFields

  let cropAmount =
    let u = Unchecked.defaultof<CropAmount>
    Decode.object (fun get ->
      let field name decoder defVal = get.Optional.Field name decoder |> Option.defaultValue defVal
      let single = CropAmount.singleAmount
      let minYield = field (nameof u.MinCropYield) Decode.uint32 CropAmount.minYield
      let maxYield = field (nameof u.MaxCropYield) (Decode.uint32 |> validate "a max crop yield greater than the min crop yield" (fun x -> x >= minYield)) single.MaxCropYield
      {
        MinCropYield = minYield
        MaxCropYield = maxYield
        FarmLevelsPerYieldIncrease = field (nameof u.FarmLevelsPerYieldIncrease) Decode.uint32 single.FarmLevelsPerYieldIncrease
        ExtraCropChance = field (nameof u.ExtraCropChance) (floatInRange CropAmount.minExtraCropChance CropAmount.maxExtraCropChance) single.ExtraCropChance
        CanDouble = field (nameof u.CanDouble) Decode.bool single.CanDouble
        Giant = field (nameof u.Giant) Decode.bool single.Giant
        FarmingQualities = field (nameof u.FarmingQualities) Decode.bool single.FarmingQualities
      }
    )

  let farmCrop: FarmCrop Decoder =
    let u = Unchecked.defaultof<FarmCrop>
    Decode.object (fun get -> {
      Growth = growthDataFields get
      Item = get.Required.Field (nameof u.Item) itemId
      Amount = get.Optional.Field (nameof u.Amount) cropAmount |> Option.defaultValue CropAmount.singleAmount
      ExtraItem = get.Optional.Field (nameof u.ExtraItem) (Decode.tuple2 itemId nonZeroFloat)
    } )

  let forageCrop: ForageCrop Decoder =
    let u = Unchecked.defaultof<ForageCrop>
    Decode.object (fun get -> {
      Growth = growthDataFields get
      Items = get.Required.Field (nameof u.Items) (Decode.array itemId |> validate "at least one item" (Array.isEmpty >> not))
      SeedRecipeUnlockLevel = get.Required.Field (nameof u.SeedRecipeUnlockLevel) Decode.uint32
    } )
    |> validate "a forage crop that grows in one season" (ForageCrop.growth >> Growth.seasons >> Seasons.tryExactlyOne >> Option.isSome)
    |> validate "a forage crop with no regrow time" (ForageCrop.growth >> Growth.regrowTime >> Option.isNone)

  let cropAmountSettings: CropAmountSettings Decoder =
    let u = Unchecked.defaultof<CropAmountSettings>
    Decode.object (fun get ->
      let field name decode = get.Required.Field name decode
      {
        SpecialCharm = field (nameof u.SpecialCharm) Decode.bool
        GiantChecksPerTile = field (nameof u.GiantChecksPerTile) (floatInRange CropAmount.minGiantCropChecks CropAmount.maxGiantCropChecks)
        ShavingToolLevel = get.Optional.Field (nameof u.ShavingToolLevel) (natLessOrEqual CropAmount.maxShavingToolLevel)
        LuckBuff = field (nameof u.LuckBuff) Decode.uint32
      }
    )

  let multipliers: Multipliers Decoder =
    let u = Unchecked.defaultof<Multipliers>
    Decode.object (fun get ->
      let field name decode = get.Required.Field name decode
      {
        ProfitMargin = field (nameof u.ProfitMargin) (floatInRange 0.25 1.0 |> validate "a multiple of 0.25" (fun x -> x % 0.25 = 0.0))
        BearsKnowledge = field (nameof u.BearsKnowledge) Decode.bool
        ForagedFruitTillerOverrides = field (nameof u.ForagedFruitTillerOverrides) (set itemId)
      }
    )

  let modData: ModData Decoder =
    let u = Unchecked.defaultof<ModData>
    Decode.object (fun get -> {
      QualityProducts = get.Required.Field (nameof u.QualityProducts) Decode.bool
      QualityProcessors = get.Required.Field (nameof u.QualityProcessors) (set processor)
    } )
