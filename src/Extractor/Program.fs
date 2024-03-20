module StardewValleyStonks.Extractor

open System.IO
open Thoth.Json.Net
open Microsoft.Xna.Framework
open Microsoft.Xna.Framework.Graphics
open StardewValleyStonks
open StardewValleyStonks.Json

type ItemOverride = {
  SellPrice: nat option
  Category: Category option
}

[<RequireQualifiedAccess>]
module ItemOverride =
  let none = {
    SellPrice = None
    Category = None
  }

type CropAmountOverride = {
  CanDouble: bool option
  FarmingDistribution: bool option
}

[<RequireQualifiedAccess>]
module CropAmountOverride =
  let none = {
    CanDouble = None
    FarmingDistribution = None
  }

type FarmCropOverride = {
  Seasons: Seasons option
  Amount: CropAmountOverride option
  ExtraItem: (ItemId * float) option
}

[<RequireQualifiedAccess>]
module FarmCropOverride =
  let none = {
    Seasons = None
    Amount = None
    ExtraItem = None
  }

type ForageCropData = {
  SeedRecipeUnlockLevel: nat
  Foragables: ItemId array
}

type Config = {
  ItemOverrides: Table<ItemId, ItemOverride>
  Products: Table<ItemId, ProcessedItem array>
  SkipCrops: SeedId Set
  FarmCropOverrides: Table<SeedId, FarmCropOverride>
  ForageCropData: Table<Season, ForageCropData>
  DataOutputPath: string
  CropImageOutputPath: string
  ItemImageOutputPath: string
}

[<RequireQualifiedAccess>]
module Decode =
  let farmCropOverride =
    let u = Unchecked.defaultof<FarmCropOverride>
    Decode.object (fun get ->
      let inline field name decoder = get.Optional.Field name decoder
      {
        Seasons = field (nameof u.Seasons) Decode.seasons
        Amount = field (nameof u.Amount) (Decode.Auto.generateDecoder ())
        ExtraItem = field (nameof u.ExtraItem) (Decode.tuple2 Decode.itemId Decode.float)
      })

  let config =
    let u = Unchecked.defaultof<Config>
    Decode.object (fun get ->
      let inline field name decode = get.Required.Field name decode
      {
        ItemOverrides =
          field
            (nameof u.ItemOverrides)
            (Decode.tableParse Decode.parseItemId (Decode.Auto.generateDecoder ()))
        Products =
          field
            (nameof u.Products)
            (Decode.tableParse Decode.parseItemId (Decode.Auto.generateDecoder ()))
        SkipCrops = field (nameof u.SkipCrops) (Decode.set Decode.seedId)
        FarmCropOverrides =
          field
            (nameof u.FarmCropOverrides)
            (Decode.tableParse Decode.parseSeedId farmCropOverride)
        ForageCropData =
          field
            (nameof u.ForageCropData)
            (Decode.tableParse
              (Season.TryParse >> function true, s -> Some s | _ -> None)
              (Decode.Auto.generateDecoder ()))
        DataOutputPath = field (nameof u.DataOutputPath) Decode.string
        CropImageOutputPath = field (nameof u.CropImageOutputPath) Decode.string
        ItemImageOutputPath = field (nameof u.ItemImageOutputPath) Decode.string
      })

[<AutoOpen>]
module Constants =
  let [<Literal>] unknownIdStart = 4000u
  let [<Literal>] forageSpriteIndex = 23u
  let [<Literal>] cropImageWidth = 16
  let [<Literal>] cropImageHeight = 32
  let [<Literal>] itemImageWidth = 16
  let [<Literal>] itemImageHeight = 16

  let [<Literal>] configPath = "config.json"

  let [<Literal>] cropDataPath = "Data\\Crops"
  let [<Literal>] itemDataPath = "Data\\Objects"
  let [<Literal>] giantCropDataPath = "Data\\GiantCrops"
  let [<Literal>] cropSpriteSheetPath = "TileSheets\\crops"
  let [<Literal>] itemSpriteSheetPath = "Maps\\springobjects"


let tryParseNat (str: string) =
  match System.UInt32.TryParse str with
  | true, n -> Some n
  | false, _ -> None

let processCategory (itemId: string) = function
  | -74 -> Seeds
  | -75 -> Vegetable
  | -79 -> Fruit
  | -80 -> Flower
  | -81 -> Forage
  | -26 -> ArtisanGood
  | -17 (* sweet gem berry *) | 0 (* coffee, sugar, flour, etc *) | -16 (* fiber *) -> Other
  | str -> failwith $"Item {itemId} has an unsupported category: {str}"

let processItem overrides itemData strId itemId =
  match itemData |> Table.tryFind strId with
  | None -> failwith $"Could not find data for item with id: {strId}"
  | Some (item: StardewValley.GameData.Objects.ObjectData) ->
    let overrides = overrides |> Table.tryFind itemId |> Option.defaultValue ItemOverride.none
    let category = overrides.Category |> Option.defaultWith (fun () -> processCategory strId item.Category)
    let sellPrice = overrides.SellPrice |> Option.defaultWith (fun () ->
      if item.Price < 0
      then failwith $"Item {strId} has a negative price: {item.Price}"
      else nat item.Price)

    let spriteIndex =
      if item.SpriteIndex < 0
      then failwith $"Item {strId} has a negative sprite index: {item.SpriteIndex}"
      else nat item.SpriteIndex

    (spriteIndex, item.Texture),
    {
      Id = itemId
      Name = item.Name
      SellPrice = sellPrice
      Category = category
    }

let processCrop farmCropOverrides forageCropData giantCrops assignId strId seed (crop: StardewValley.GameData.Crops.CropData) =
  // Some currently ignored fields that might be useful in the future:
  // crop.IsRaised
  // crop.NeedsWatering
  // crop.PlantableLocationRules

  let seasons =
    crop.Seasons
    |> Seq.map (function
      | StardewValley.Season.Spring -> Season.Spring
      | StardewValley.Season.Summer -> Season.Summer
      | StardewValley.Season.Fall -> Season.Fall
      | StardewValley.Season.Winter -> Season.Winter
      | season -> failwith $"Crop {strId} has an unknown season: {season}")
    |> Seasons.ofSeq

  let growthStages =
    crop.DaysInPhase
    |> Seq.map (fun stage ->
      if stage <= 0
      then failwith $"Crop {strId} has a negative or zero growth stage: {stage}"
      else nat stage)
    |> Array.ofSeq

  if growthStages.Length = 0 then
    failwith $"Crop {strId} has no growth stages"

  let regrowTime =
    match crop.RegrowDays with
    | -1 -> None
    | time when time <= 0 -> failwith $"Crop {strId} had a negative or zero regrow time: {time}"
    | time -> Some (nat time)

  let minHarvest =
    if crop.HarvestMinStack < int CropAmount.minYield
    then failwith $"Crop {strId} has a minimum harvest below {CropAmount.minYield}: {crop.HarvestMinStack}"
    else nat crop.HarvestMinStack

  let maxHarvest =
    if crop.HarvestMaxStack < int CropAmount.minYield
    then failwith $"Crop {strId} has a maximum harvest below {CropAmount.minYield}: {crop.HarvestMaxStack}"
    else nat crop.HarvestMaxStack |> max minHarvest

  let increasePerLevel =
    if crop.HarvestMaxIncreasePerFarmingLevel < 0.0f
    then failwith $"Crop {strId} has a negative maximum harvest increase per level: {crop.HarvestMaxIncreasePerFarmingLevel}"
    else nat (1.0f / crop.HarvestMaxIncreasePerFarmingLevel)

  let extraChance =
    if crop.ExtraHarvestChance < CropAmount.minExtraCropChance || CropAmount.maxExtraCropChance < crop.ExtraHarvestChance
    then failwith $"Crop {strId} has an extra crop chance not in the range [{CropAmount.minExtraCropChance}, {CropAmount.maxExtraCropChance}]: {crop.ExtraHarvestChance}"
    else crop.ExtraHarvestChance

  let scythe =
    match crop.HarvestMethod with
    | StardewValley.GameData.Crops.HarvestMethod.Grab -> false
    | StardewValley.GameData.Crops.HarvestMethod.Scythe -> true
    | method -> failwith $"Crop {strId} has an unknown harvest method: {method}"

  let paddy = crop.IsPaddyCrop
  let giant = giantCrops |> Set.contains crop.HarvestItemId

  let spriteIndex =
    if crop.SpriteIndex < 0
    then failwith $"Crop {strId} has a negative sprite index: {crop.SpriteIndex}"
    else nat crop.SpriteIndex

  spriteIndex,
  if spriteIndex = forageSpriteIndex then
    let season =
      match seasons with
      | Seasons.Spring -> Season.Spring
      | Seasons.Summer -> Season.Summer
      | Seasons.Fall -> Season.Fall
      | Seasons.Winter -> Season.Winter
      | seasons -> failwith $"Forage crop {strId} does not grow in a single season: {seasons}"

    if regrowTime.IsSome then failwith $"Forage crop {strId} regrows"
    if scythe then failwith $"Forage crop {strId} has the scythe harvest method"
    if paddy then failwith $"Forage crop {strId} is a paddy crop"
    if giant then failwith $"Forage crop {strId} is a giant crop"

    let data =
      match forageCropData |> Table.tryFind season with
      | Some data -> data
      | None -> failwith $"No data was provided for {Season.name season} Forage in the config"

    if string data.Foragables[0] <> crop.HarvestItemId then
      failwith $"The main item provided for {Season.name season} Forage in the config does not match {crop.HarvestItemId}"

    ForageCrop {
      Season = season
      Stages = growthStages
      Seed = seed
      Foragables = data.Foragables |> Array.map (string >> assignId)
      SeedRecipeUnlockLevel = data.SeedRecipeUnlockLevel
    }

  else
    let overrides = farmCropOverrides |> Table.tryFind seed |> Option.defaultValue FarmCropOverride.none
    let amountOverride = overrides.Amount |> Option.defaultValue CropAmountOverride.none
    let cropAmount = {
      MinCropYield = minHarvest
      MaxCropYield = maxHarvest
      FarmLevelsPerYieldIncrease = nat increasePerLevel
      ExtraCropChance = extraChance
      CanDouble = amountOverride.CanDouble |> Option.defaultValue (not scythe)
      FarmingQualities = amountOverride.FarmingDistribution |> Option.defaultValue true
    }

    let item = assignId crop.HarvestItemId
    let extraItem = overrides.ExtraItem |> Option.map (fun (itemId, amount) ->
      itemId |> string |> assignId, amount)

    FarmCrop {
      Seasons = overrides.Seasons |> Option.defaultValue seasons
      Stages = growthStages
      RegrowTime = regrowTime
      Paddy = paddy
      Giant = giant
      Seed = seed
      Amount = cropAmount
      Item = item
      ExtraItem = extraItem
    }

let saveSubTexture name graphics x y width height (texture: Texture2D) =
  let sub = Array.zeroCreate (width * height)
  texture.GetData (0, Rectangle (x, y, width, height), sub, 0, sub.Length)
  let subTexture = new Texture2D (graphics, width, height)
  subTexture.SetData sub
  use file = File.OpenWrite (name + ".png")
  subTexture.SaveAsPng (file, width, height)

let saveStageImages graphics outputPath cropSpriteSheet spriteIndex crop =
  let path = Path.Combine (outputPath, string (Crop.seed crop))
  Directory.CreateDirectory path |> ignore

  let x = if spriteIndex % 2u = 0u then 0 else 128
  let y = (int spriteIndex / 2) * cropImageHeight

  let save name i =
    saveSubTexture
      (Path.Combine (path, name))
      graphics
      (x + i * cropImageWidth)
      y
      cropImageWidth
      cropImageHeight
      cropSpriteSheet

  let stages = Crop.stages crop

  save (string 0) 0
  for i = 1 to stages.Length do
    save (string i) (i + 1)

  if Crop.regrows crop then save "Regrow" (stages.Length + 2)

let saveItemImage graphics outputPath spriteSheets (spriteIndex, spriteSheet) item =
  let spritesheet = if spriteSheet = null then itemSpriteSheetPath else spriteSheet
  let spriteSheet: Texture2D = spriteSheets |> Table.find spritesheet
  saveSubTexture
    (Path.Combine (outputPath, string item.Id))
    graphics
    (int spriteIndex * itemImageWidth % spriteSheet.Width)
    (int spriteIndex * itemImageWidth / spriteSheet.Width * itemImageHeight)
    itemImageWidth
    itemImageHeight
    spriteSheet


[<EntryPoint>]
let main args =
  let stardewValleyRoot =
    match Array.tryExactlyOne args with
    | Some root -> root
    | None -> failwith "Unexpected number of command line arguments. Pass the root directory of the Stardew Valley exe as the first and only command line argument."

  let config =
    let json =
      try File.ReadAllText configPath
      with _ -> eprintfn "Error reading config file"; reraise ()

    match Decode.fromString Decode.config json with
    | Ok config -> config
    | Error e -> failwith $"Error parsing config file: {e}"

  printfn "Press enter to run the extractor..."
  |> System.Console.ReadLine
  |> ignore

  let game = new Game ()
  let graphicsManager = new GraphicsDeviceManager (game)
  (graphicsManager :> IGraphicsDeviceManager).CreateDevice ()
  let graphics = graphicsManager.GraphicsDevice
  game.RunOneFrame ()

  let content =
    let dummyServiceProvider = {
      new System.IServiceProvider with
        member _.GetService t =
          if t = typeof<IGraphicsDeviceService>
          then graphicsManager :> obj
          else null
    }
    new Content.ContentManager (dummyServiceProvider, Path.Combine (stardewValleyRoot, "Content"))

  let mutable lastId = unknownIdStart
  let itemIds = Dictionary ()

  let assignId (strId: string) =
    match itemIds.TryGetValue strId with
    | true, itemId -> itemId
    | false, _ ->
      let itemId = tryParseNat strId |> Option.defaultWith (fun () ->
        let itemId = lastId
        lastId <- lastId + 1u
        itemId)
      let itemId = itemId * 1u<_>
      itemIds.Add (strId, itemId)
      itemId

  let tryLoad name (path: string) =
    try content.Load (path.Replace ('\\', Path.DirectorySeparatorChar))
    with _ -> eprintfn $"Error loading {name}"; reraise ()

  let cropData: Dictionary<string, StardewValley.GameData.Crops.CropData> = tryLoad "crop data" cropDataPath
  let itemData: Dictionary<string, StardewValley.GameData.Objects.ObjectData> = tryLoad "item data" itemDataPath
  let giantCrops: Dictionary<string, StardewValley.GameData.GiantCrops.GiantCropData> = tryLoad "giant crop data" giantCropDataPath

  let giantCrops =
    giantCrops.Values
    |> Seq.map (fun data ->
      let itemId = data.FromItemId
      if itemId.StartsWith "(O)"
      then itemId.Substring 3
      else failwith $"Unsupported giant crop item: {itemId}")
    |> Set.ofSeq

  let crops =
    cropData
    |> Seq.choose (fun (KeyValue(seed, crop)) ->
      if tryParseNat seed |> Option.exists (fun seed -> config.SkipCrops.Contains (seed * 1u<_>))
      then None
      else Some (assignId seed * 1u<_>, (seed, crop)))
    |> Seq.map (fun (seed, (strId, data)) ->
      processCrop config.FarmCropOverrides config.ForageCropData giantCrops assignId strId seed data)
    |> Array.ofSeq
  crops |> Array.sortInPlaceBy (snd >> Crop.seed)

  config.Products.Values |> Seq.collect (Seq.map _.Item) |> Seq.iter (string >> assignId >> ignore)

  let itemData =
    itemData
    |> Seq.map (fun (KeyValue(strId, item)) -> strId, item)
    |> Table.ofSeq

  let items =
    itemIds
    |> Seq.map (fun (KeyValue(strId, itemId)) -> processItem config.ItemOverrides itemData strId itemId)
    |> Array.ofSeq
  items |> Array.sortInPlaceBy (snd >> _.Id)

  printfn "Successfully parsed all crops and items. Proced to output the data file and images? [y/N] "

  let writeImages =
    match System.Console.ReadLine().ToLower() with
    | "y" | "yes" -> true
    | _ -> false

  if writeImages then
    let itemSpriteSheets =
      items
      |> Seq.map (fst >> snd)
      |> Seq.distinct
      |> Seq.map (fun texture ->
        if texture = null
        then itemSpriteSheetPath, tryLoad $"item spritesheet" itemSpriteSheetPath
        else texture, tryLoad $"item spritesheet: {texture}" texture)
      |> Table.ofSeq

    try
      Directory.CreateDirectory config.ItemImageOutputPath |> ignore
      items |> Array.iter (fun (spriteData, item) ->
        saveItemImage graphics config.ItemImageOutputPath itemSpriteSheets spriteData item)
    with _ -> eprintfn "Error writing the item images"; reraise ()

    let cropSpriteSheet = tryLoad "crop spritesheet" cropSpriteSheetPath

    try
      Directory.CreateDirectory config.CropImageOutputPath |> ignore
      crops |> Array.iter (fun (spriteSheetRow, crop) ->
        saveStageImages graphics config.CropImageOutputPath cropSpriteSheet spriteSheetRow crop)
    with _ -> eprintfn "Error writing the crop images"; reraise ()

    let data = {
      Items = items |> Array.map snd
      Products = config.Products
      FarmCrops = crops |> Array.choose (function | (_, FarmCrop crop) -> Some crop | _ -> None)
      ForageCrops = crops |> Array.choose (function | (_, ForageCrop crop) -> Some crop | _ -> None)
    }

    let dataStr = data |> Encode.extractedData |> Encode.toString 2

    try File.WriteAllText (config.DataOutputPath, dataStr)
    with _ -> eprintfn "Error writing the data file"; reraise ()

  0
