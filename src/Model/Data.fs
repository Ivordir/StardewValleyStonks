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

module [<RequireQualifiedAccess>] Encode =
  let mapSeq encoder (seq: _ seq) = seq |> Seq.map encoder |> Encode.seq

  let keyValues keyString encodeValue seq =
    seq
    |> Seq.map (fun (k, v) -> keyString k, encodeValue v)
    #if !FABLE_COMPILER
    |> List.ofSeq
    #endif
    |> Encode.object

  let mapObj keyString encodeValue map =
    map |> Map.toSeq |> keyValues keyString encodeValue

  let table keyString encodeValue table =
    table |> Table.toSeq |> keyValues keyString encodeValue

  let vendor (VendorName vendor) = Encode.string vendor

  let processor (ProcessorName processor) = Encode.string processor

  let seedId (seed: SeedId) = Encode.uint32 (nat seed)
  let itemId (item: ItemId) = Encode.uint32 (nat item)

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

  let season (season: Season) = Season.name season |> Encode.string

  let seasons seasons =
    Season.all
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

  let farmCrop crop = Encode.object [
    nameof crop.Seasons, seasons crop.Seasons
    nameof crop.Seed, seedId crop.Seed
    nameof crop.Stages, crop.Stages |> mapSeq Encode.uint32
    if crop.Paddy then
      nameof crop.Paddy, Encode.bool crop.Paddy
    if crop.Giant then
      nameof crop.Giant, Encode.bool crop.Giant
    if crop.Amount <> CropAmount.singleAmount then
      nameof crop.Amount, cropAmount crop.Amount
    if crop.ExtraItem.IsSome then
      nameof crop.ExtraItem, Encode.tuple2 itemId Encode.float (Option.get crop.ExtraItem)
    if crop.RegrowTime.IsSome then
      nameof crop.RegrowTime, Encode.uint32 (Option.get crop.RegrowTime)
    nameof crop.Item, itemId crop.Item
  ]

  let forageCrop crop = Encode.object [
    nameof crop.Season, season crop.Season
    nameof crop.Seed, seedId crop.Seed
    nameof crop.Stages, crop.Stages |> mapSeq Encode.uint32
    nameof crop.Items, crop.Items |> mapSeq itemId
    nameof crop.SeedRecipeUnlockLevel, Encode.uint32 crop.SeedRecipeUnlockLevel
  ]

  let extractedData (data: ExtractedData) = Encode.object [
    nameof data.Items, data.Items |> mapSeq (Encode.Auto.generateEncoder ())
    nameof data.FarmCrops, data.FarmCrops |> mapSeq farmCrop
    nameof data.ForageCrops, data.ForageCrops |> mapSeq forageCrop
  ]


module [<RequireQualifiedAccess>] Decode =
  let inline private tryParseInt<[<Measure>] ^u> (str: string): uint<'u> option =
    match System.UInt32.TryParse str with
    | true, value -> Some (value * 1u<_>)
    | _ -> None

  let parseItemId = tryParseInt<ItemNum>

  let parseSeedId = tryParseInt<SeedNum>

  let private wrap (wrapper: 'a -> 'b) (decoder: 'a Decoder) =
    #if FABLE_COMPILER
    Fable.Core.JsInterop.(!!)decoder : 'b Decoder
    #else
    decoder |> Decode.map wrapper
    #endif

  let private natMeasure<[<Measure>] 'u> =
    #if FABLE_COMPILER
    Fable.Core.JsInterop.(!!)Decode.uint32 : uint<'u> Decoder
    #else
    Decode.uint32 |> Decode.map LanguagePrimitives.UInt32WithMeasure<'u>
    #endif

  let set decoder = Decode.array decoder |> Decode.map Set.ofArray

  let private wrapKeys mapping (keyWrap: string -> 'k) (decodeValue: 'v Decoder) =
    #if FABLE_COMPILER
    (Fable.Core.JsInterop.(!!)Decode.keyValuePairs decodeValue: ('k * 'v) list Decoder) |> Decode.map mapping
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

  let mapObj keyWrap decodeValue = wrapKeys Map.ofSeq keyWrap decodeValue
  let mapObjParse parseKey decodeValue = parseKeysWith Map.ofSeq parseKey decodeValue

  let fertilizer: Fertilizer Decoder =
    let u = Unchecked.defaultof<Fertilizer>
    Decode.object (fun get -> {
      Name = get.Required.Field (nameof u.Name) Decode.string
      Quality = get.Optional.Field (nameof u.Quality) Decode.uint32 |> Option.defaultValue 0u
      Speed = get.Optional.Field (nameof u.Speed) Decode.float |> Option.defaultValue 0.0
    } )

  let vendor: Vendor Decoder = Decode.string |> wrap VendorName

  let processor: Processor Decoder = Decode.string |> wrap ProcessorName

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
        Ratio = get.Optional.Field (nameof processed.Ratio) (Decode.tuple2 Decode.uint32 Decode.uint32)
      |} )
    ]

  let season: Season Decoder =
    Decode.string |> Decode.andThen (fun str ->
      match Season.TryParse str with
      | true, season -> Decode.succeed season
      | _ -> Decode.fail $"Failed to parse season: {str}")

  let seasons: Seasons Decoder = Decode.array season |> Decode.map Seasons.ofSeq

  let date: Date Decoder =
    let u = Unchecked.defaultof<Date>
    Decode.object (fun get -> {
      Season = get.Required.Field (nameof u.Season) season
      Day = get.Required.Field (nameof u.Day) Decode.uint32
    } )

  let seedPrice: SeedPrice Decoder =
    Decode.map3 (fun a b c -> a, b, c)
      (Decode.field SeedPriceVendorField (Decode.Auto.generateDecoder ()))
      (Decode.optional SeedPricePriceField Decode.uint32)
      (Decode.field SeedPriceTypeField Decode.string)
    |> Decode.andThen (fun (vendor, price, kind) -> fun path value ->
      match kind, price with
      | FixedSeedPrice, Some price -> Ok (FixedPrice (vendor, price))
      | FixedSeedPrice, None -> Error (path, BadField ($"an object with a field named '{SeedPricePriceField}'", value))
      | ScalingSeedPrice, Some price -> Ok (ScalingPrice (vendor, Some price))
      | ScalingSeedPrice, None -> Ok (ScalingPrice (vendor, None))
      | _ -> Error (path, BadPrimitive ($"a valid price type ('{FixedSeedPrice}' or '{ScalingSeedPrice}')", value)))

  let cropAmount: CropAmount Decoder =
    let u = Unchecked.defaultof<CropAmount>
    Decode.object (fun get ->
      let field name decoder defVal = get.Optional.Field name decoder |> Option.defaultValue defVal
      let single = CropAmount.singleAmount
      {
        MinCropYield = field (nameof u.MinCropYield) Decode.uint32 single.MinCropYield
        MaxCropYield = field (nameof u.MaxCropYield) Decode.uint32 single.MaxCropYield
        FarmLevelsPerYieldIncrease = field (nameof u.FarmLevelsPerYieldIncrease) Decode.uint32 single.FarmLevelsPerYieldIncrease
        ExtraCropChance = field (nameof u.ExtraCropChance) Decode.float single.ExtraCropChance
        CanDouble = field (nameof u.CanDouble) Decode.bool single.CanDouble
        FarmingQualities = field (nameof u.FarmingQualities) Decode.bool single.FarmingQualities
      }
    )

  let farmCrop: FarmCrop Decoder =
    let u = Unchecked.defaultof<FarmCrop>
    Decode.object (fun get -> {
      Seasons = get.Required.Field (nameof u.Seasons) seasons
      Seed = get.Required.Field (nameof u.Seed) seedId
      Stages = get.Required.Field (nameof u.Stages) (Decode.array Decode.uint32)
      RegrowTime = get.Optional.Field (nameof u.RegrowTime) Decode.uint32
      Paddy = get.Optional.Field (nameof u.Paddy) Decode.bool |> Option.defaultValue false
      Giant = get.Optional.Field (nameof u.Giant) Decode.bool |> Option.defaultValue false
      Item = get.Required.Field (nameof u.Item) itemId
      Amount = get.Optional.Field (nameof u.Amount) cropAmount |> Option.defaultValue CropAmount.singleAmount
      ExtraItem = get.Optional.Field (nameof u.ExtraItem) (Decode.tuple2 itemId Decode.float)
    } )

  let forageCrop: ForageCrop Decoder =
    let u = Unchecked.defaultof<ForageCrop>
    Decode.object (fun get ->
      let field name decoder = get.Required.Field name decoder
      {
        Season = field (nameof u.Season) season
        Stages = field (nameof u.Stages) (Decode.array Decode.uint32)
        Seed = field (nameof u.Seed) seedId
        Items = field (nameof u.Items) (Decode.array itemId)
        SeedRecipeUnlockLevel = field (nameof u.SeedRecipeUnlockLevel) Decode.uint32
      }
    )

  let extractedData: ExtractedData Decoder =
    let u = Unchecked.defaultof<ExtractedData>
    Decode.object (fun get ->
      let field name decode = get.Required.Field name decode
      {
        Items = field (nameof u.Items) (Decode.Auto.generateDecoder ())
        FarmCrops = field (nameof u.FarmCrops) (Decode.array farmCrop)
        ForageCrops = field (nameof u.ForageCrops) (Decode.array forageCrop)
      }
    )

  let supplementalData: SupplementalData Decoder =
    let u = Unchecked.defaultof<SupplementalData>
    Decode.object (fun get ->
      let field name decode = get.Required.Field name decode
      {
        Fertilizers = field (nameof u.Fertilizers) (Decode.array fertilizer)
        Products = field (nameof u.Products) (tableParse parseItemId (Decode.array product))
        ProcessorUnlockLevel = field (nameof u.ProcessorUnlockLevel) (table ProcessorName Decode.uint32)
        FertilizerPrices = field (nameof u.FertilizerPrices) (table id (table VendorName Decode.uint32))
        SeedPrices = field (nameof u.SeedPrices) (tableParse parseSeedId (Decode.array seedPrice))
        GenerateSeedPrices = field (nameof u.GenerateSeedPrices) (wrapKeys id VendorName (Decode.array seedId))
      }
    )
