module StardewValleyStonks.Json

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

let private encodeProcessor (ProcessorName processor) = Encode.string processor
let private decodeProcessor = Decode.string |> Decode.map ProcessorName
let private processorCoders = Extra.empty |> Extra.withCustom encodeProcessor decodeProcessor

[<RequireQualifiedAccess>]
module Encode =
  let mapSeq encoder (seq: _ seq) = seq |> Seq.map encoder |> Encode.seq

  let keyValues keyString encodeValue seq =
    seq
    |> Seq.map (fun (k, v) -> keyString k, encodeValue v)
    #if !FABLE_COMPILER
    |> List.ofSeq // for some reason the API is different between Thoth.Json and Thoth.Json.Net
    #endif
    |> Encode.object

  let table keyString encodeValue table = table |> Table.toSeq |> keyValues keyString encodeValue

  let vendor (VendorName vendor) = Encode.string vendor

  let processor = encodeProcessor

  let seedId (seed: SeedId) = Encode.uint32 (nat seed)
  let itemId (item: ItemId) = Encode.uint32 (nat item)

  let processedItem = Encode.Auto.generateEncoder<ProcessedItem> (extra = processorCoders)

  let season (season: Season) = Season.name season |> Encode.string

  let seasons seasons =
    Enum.values
    |> Array.filter (fun season -> seasons |> Seasons.contains season)
    |> mapSeq season

  let date (date: Date) = Encode.object [
    nameof date.Season, season date.Season
    nameof date.Day, Encode.uint32 date.Day
  ]

  let fertilizer (fertilizer: Fertilizer) = Encode.object [
    nameof fertilizer.Name, Encode.string fertilizer.Name
    if fertilizer.Quality <> 0u then
      nameof fertilizer.Quality, Encode.uint32 fertilizer.Quality
    if fertilizer.Speed <> 0.0 then
      nameof fertilizer.Speed, Encode.float fertilizer.Speed
  ]

  let seedPrice = function
    | FixedPrice (v, p) ->
      Encode.object [
        SeedPriceTypeField, Encode.string FixedSeedPrice
        SeedPriceVendorField, vendor v
        SeedPricePriceField, Encode.uint32 p
      ]
    | ScalingPrice (v, None) ->
      Encode.object [
        SeedPriceTypeField, Encode.string ScalingSeedPrice
        SeedPriceVendorField, vendor v
      ]
    | ScalingPrice (v, Some p) ->
      Encode.object [
        SeedPriceTypeField, Encode.string ScalingSeedPrice
        SeedPriceVendorField, vendor v
        SeedPricePriceField, Encode.uint32 p
      ]

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
      if amount.FarmLevelsPerYieldIncrease <> single.FarmLevelsPerYieldIncrease then
        nameof amount.FarmLevelsPerYieldIncrease, Encode.uint32 amount.FarmLevelsPerYieldIncrease
      if amount.FarmingQualities <> single.FarmingQualities then
        nameof amount.FarmingQualities, Encode.bool amount.FarmingQualities
    ]

  let farmCrop (crop: FarmCrop) = Encode.object [
    nameof crop.Seed, seedId crop.Seed
    nameof crop.Item, itemId crop.Item
    if crop.Amount <> CropAmount.singleAmount then
      nameof crop.Amount, cropAmount crop.Amount
    if crop.ExtraItem.IsSome then
      nameof crop.ExtraItem, Encode.tuple2 itemId Encode.float crop.ExtraItem.Value
    if crop.Giant then
      nameof crop.Giant, Encode.bool crop.Giant

    nameof crop.Seasons, seasons crop.Seasons
    nameof crop.Stages, crop.Stages |> mapSeq Encode.uint32
    if crop.RegrowTime.IsSome then
      nameof crop.RegrowTime, Encode.uint32 crop.RegrowTime.Value
    if crop.Paddy then
      nameof crop.Paddy, Encode.bool crop.Paddy
  ]

  let forageCrop (crop: ForageCrop) = Encode.object [
    nameof crop.Seed, seedId crop.Seed
    nameof crop.Items, crop.Items |> mapSeq itemId
    nameof crop.SeedRecipeUnlockLevel, Encode.uint32 crop.SeedRecipeUnlockLevel
    nameof crop.Season, season crop.Season
    nameof crop.Stages, crop.Stages |> mapSeq Encode.uint32
  ]

  let extractedData (data: ExtractedData) = Encode.object [
    nameof data.Items, data.Items |> mapSeq (Encode.Auto.generateEncoder ())
    nameof data.Products, data.Products |> table string (mapSeq processedItem)
    nameof data.FarmCrops, data.FarmCrops |> mapSeq farmCrop
    nameof data.ForageCrops, data.ForageCrops |> mapSeq forageCrop
  ]


[<RequireQualifiedAccess>]
module Decode =
  let inline private tryParseNat<[<Measure>] ^u> (str: string): uint<'u> option =
    match System.UInt32.TryParse str with
    | true, value -> Some (value * 1u<_>)
    | _ -> None

  let parseItemId = tryParseNat<ItemNum>

  let parseSeedId = tryParseNat<SeedNum>

  let private natMeasure<[<Measure>] 'u> =
    #if FABLE_COMPILER
    unbox<uint<'u> Decoder> Decode.uint32
    #else
    Decode.uint32 |> Decode.map LanguagePrimitives.UInt32WithMeasure<'u>
    #endif

  let set decoder = Decode.array decoder |> Decode.map Set.ofArray

  let private wrapKeys mapping (keyWrap: string -> 'k) (decodeValue: 'v Decoder) =
    #if FABLE_COMPILER
    Decode.keyValuePairs decodeValue |> unbox<('k * 'v) list Decoder> |> Decode.map mapping
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
      then acc |> mapping |> Decode.succeed
      else fun path value -> Error (path, BadPrimitive ("a valid key", value)))

  let table keyWrap decodeValue = wrapKeys Table.ofSeq keyWrap decodeValue
  let tableParse parseKey decodeValue = parseKeysWith Table.ofSeq parseKey decodeValue

  let fertilizer =
    let u = Unchecked.defaultof<Fertilizer>
    Decode.object (fun get -> {
      Name = get.Required.Field (nameof u.Name) Decode.string
      Quality = get.Optional.Field (nameof u.Quality) Decode.uint32 |> Option.defaultValue 0u
      Speed = get.Optional.Field (nameof u.Speed) Decode.float |> Option.defaultValue 0.0
    })

  let vendor = Decode.string |> Decode.map VendorName

  let processor = decodeProcessor

  let itemId = natMeasure<ItemNum>
  let seedId = natMeasure<SeedNum>

  let processedItem = Decode.Auto.generateDecoder<ProcessedItem> (extra = processorCoders)

  let season =
    Decode.string |> Decode.andThen (fun str ->
      match Season.TryParse str with
      | true, season -> Decode.succeed season
      | _ -> Decode.fail $"Failed to parse season: {str}")

  let seasons = Decode.array season |> Decode.map Seasons.ofSeq

  let date: Date Decoder =
    let u = Unchecked.defaultof<Date>
    Decode.object (fun get -> {
      Season = get.Required.Field (nameof u.Season) season
      Day = get.Required.Field (nameof u.Day) Decode.uint32
    })

  let seedPrice: SeedPrice Decoder =
    Decode.map3 (fun a b c -> a, b, c)
      (Decode.field SeedPriceTypeField Decode.string)
      (Decode.field SeedPriceVendorField vendor)
      (Decode.optional SeedPricePriceField Decode.uint32)
    |> Decode.andThen (fun (kind, vendor, price) -> fun path value ->
      match kind, price with
      | FixedSeedPrice, Some price -> Ok (FixedPrice (vendor, price))
      | FixedSeedPrice, None -> Error (path, BadField ($"an object with a field named '{SeedPricePriceField}'", value))
      | ScalingSeedPrice, Some price -> Ok (ScalingPrice (vendor, Some price))
      | ScalingSeedPrice, None -> Ok (ScalingPrice (vendor, None))
      | _ -> Error (path, BadPrimitive ($"a valid price type ('{FixedSeedPrice}' or '{ScalingSeedPrice}')", value)))

  let cropAmount =
    let u = Unchecked.defaultof<CropAmount>
    Decode.object (fun get ->
      let field name decoder defaultValue = get.Optional.Field name decoder |> Option.defaultValue defaultValue
      {
        MinCropYield = field (nameof u.MinCropYield) Decode.uint32 CropAmount.singleAmount.MinCropYield
        MaxCropYield = field (nameof u.MaxCropYield) Decode.uint32 CropAmount.singleAmount.MaxCropYield
        FarmLevelsPerYieldIncrease =
          field
            (nameof u.FarmLevelsPerYieldIncrease)
            Decode.uint32
            CropAmount.singleAmount.FarmLevelsPerYieldIncrease
        ExtraCropChance = field (nameof u.ExtraCropChance) Decode.float CropAmount.singleAmount.ExtraCropChance
        CanDouble = field (nameof u.CanDouble) Decode.bool CropAmount.singleAmount.CanDouble
        FarmingQualities = field (nameof u.FarmingQualities) Decode.bool CropAmount.singleAmount.FarmingQualities
      })

  let farmCrop =
    let u = Unchecked.defaultof<FarmCrop>
    Decode.object (fun get -> {
      Seed = get.Required.Field (nameof u.Seed) seedId
      Item = get.Required.Field (nameof u.Item) itemId
      Amount = get.Optional.Field (nameof u.Amount) cropAmount |> Option.defaultValue CropAmount.singleAmount
      ExtraItem = get.Optional.Field (nameof u.ExtraItem) (Decode.tuple2 itemId Decode.float)
      Giant = get.Optional.Field (nameof u.Giant) Decode.bool |> Option.defaultValue false

      Seasons = get.Required.Field (nameof u.Seasons) seasons
      Stages = get.Required.Field (nameof u.Stages) (Decode.array Decode.uint32)
      RegrowTime = get.Optional.Field (nameof u.RegrowTime) Decode.uint32
      Paddy = get.Optional.Field (nameof u.Paddy) Decode.bool |> Option.defaultValue false
    })

  let forageCrop =
    let u = Unchecked.defaultof<ForageCrop>
    Decode.object (fun get ->
      let field name decoder = get.Required.Field name decoder
      {
        Seed = field (nameof u.Seed) seedId
        Items = field (nameof u.Items) (Decode.array itemId)
        SeedRecipeUnlockLevel = field (nameof u.SeedRecipeUnlockLevel) Decode.uint32
        Season = field (nameof u.Season) season
        Stages = field (nameof u.Stages) (Decode.array Decode.uint32)
      })

  let extractedData: ExtractedData Decoder =
    let u = Unchecked.defaultof<ExtractedData>
    Decode.object (fun get ->
      let field name decode = get.Required.Field name decode
      {
        Items = field (nameof u.Items) (Decode.Auto.generateDecoder ())
        Products = field (nameof u.Products) (tableParse parseItemId (Decode.array processedItem))
        FarmCrops = field (nameof u.FarmCrops) (Decode.array farmCrop)
        ForageCrops = field (nameof u.ForageCrops) (Decode.array forageCrop)
      })

  let supplementalData: SupplementalData Decoder =
    let u = Unchecked.defaultof<SupplementalData>
    Decode.object (fun get ->
      let field name decode = get.Required.Field name decode
      {
        Fertilizers = field (nameof u.Fertilizers) (Decode.array fertilizer)
        FertilizerPrices = field (nameof u.FertilizerPrices) (table id (table VendorName Decode.uint32))
        GenerateSeedPrices = field (nameof u.GenerateSeedPrices) (table VendorName (Decode.array seedId))
        SeedPrices = field (nameof u.SeedPrices) (tableParse parseSeedId (Decode.array seedPrice))
        ProcessorUnlockLevel = field (nameof u.ProcessorUnlockLevel) (table ProcessorName Decode.uint32)
      })
