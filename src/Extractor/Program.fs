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
  Paddy: bool option
  Giant: bool option
  Amount: CropAmountOverride option
  ExtraItem: (ItemId * float) option
}

[<RequireQualifiedAccess>]
module FarmCropOverride =
  let none = {
    Seasons = None
    Paddy = None
    Giant = None
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
        Paddy = field (nameof u.Paddy) Decode.bool
        Giant = field (nameof u.Giant) Decode.bool
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
  let [<Literal>] forageSpriteSheetRow = 23u
  let [<Literal>] cropImageWidth = 16
  let [<Literal>] cropImageHeight = 32
  let [<Literal>] itemImageWidth = 16
  let [<Literal>] itemImageHeight = 16
  let [<Literal>] imageScale = 4

  let [<Literal>] configPath = "config.json"

  let [<Literal>] cropDataPath = "Content/Data/Crops"
  let [<Literal>] itemDataPath = "Content/Data/ObjectInformation"
  let [<Literal>] cropSpriteSheetPath = "Content/TileSheets/crops"
  let [<Literal>] itemSpriteSheetPath = "Content/Maps/springobjects"


let parseCategory (itemId: ItemId) = function
  | "Seeds -74" -> Seeds
  | "Basic -75" -> Vegetable
  | "Basic -79" -> Fruit
  | "Basic -80" -> Flower
  | "Basic -81" -> Forage
  | "Basic -26" -> ArtisanGood
  | "Basic -17" // sweet gem berry
  | "Crafting" // (coffee)
  | "Basic" // sugar, flour, etc
  | "Basic -16" // building resources (fiber)
    -> Other
  | str -> failwith $"Unexpected item category for item {itemId}: {str}"

let parseItem overrides itemData itemId =
  match itemData |> Table.tryFind itemId with
  | None -> failwith $"Could not find data for item with id: {itemId}"
  | Some (str: string) ->
    let split = str.Split '/'
    if split.Length < 5 then failwith $"Unexpected item data format for item {itemId}: {str}"
    let overrides = overrides |> Table.tryFind itemId |> Option.defaultValue ItemOverride.none
    {
      Id = itemId
      Name = split[4]
      SellPrice = overrides.SellPrice |> Option.defaultWith (fun () -> nat split[1])
      Category = overrides.Category |> Option.defaultWith (fun () -> parseCategory itemId split[3])
    }

let parseCropAmount seed overrides scythe (cropAmount: string) =
  match cropAmount.Split ' ' with
  | [| "false" |] ->
    (CropAmount.singleAmount, overrides) ||> Option.fold (fun amount overrides ->
      { amount with
          CanDouble = overrides.CanDouble |> Option.defaultValue amount.CanDouble
          FarmingQualities = overrides.FarmingDistribution |> Option.defaultValue amount.FarmingQualities
      })

  | [| "true"; minHarvest; maxHarvest; increasePerLevel; extraChance |] ->
    let overrides = overrides |> Option.defaultValue CropAmountOverride.none

    let minHarvest = uint minHarvest
    if minHarvest < CropAmount.minYield then
      failwith $"The minimum harvest for crop {seed} was below {CropAmount.minYield}."

    let maxHarvest = uint maxHarvest
    if minHarvest > maxHarvest then
      failwith $"The min harvest ({minHarvest}) was greater than the max harvest ({maxHarvest}) for crop {seed}."

    let extraChance = float extraChance
    if extraChance < CropAmount.minExtraCropChance || CropAmount.maxExtraCropChance < extraChance then
      failwith $"The extra crop chance for crop {seed} was not in the range [{CropAmount.minExtraCropChance}, {CropAmount.maxExtraCropChance}]."

    {
      MinCropYield = minHarvest
      MaxCropYield = maxHarvest
      FarmLevelsPerYieldIncrease = uint increasePerLevel
      ExtraCropChance = extraChance
      CanDouble = overrides.CanDouble |> Option.defaultValue (not scythe)
      FarmingQualities = overrides.FarmingDistribution |> Option.defaultValue true
    }

  | _ -> failwith $"Unexpected crop amount format for crop {seed}: {cropAmount}"


let parseCrop farmCropOverrides forageCropData seed (data: string) =
  match data.Split '/' with
  | [| growthStages; seasons; spriteSheetRow; itemId; regrowTime; scythe; cropAmount; _; _ |] ->
    let spriteSheetRow = uint spriteSheetRow
    let itemId = nat itemId * 1u<_>

    let growthStages =
      let split = growthStages.Split ' '
      if Array.isEmpty split then failwith $"The crop {seed} had no growth stages." else
      let stages = split |> Array.map uint
      if stages |> Array.contains 0u then failwith $"The crop {seed} had a growth stage of 0."
      stages

    let seasons = (Seasons.None, seasons.Split ' ') ||> Array.fold (fun seasons season ->
      Seasons.Parse (season, true) ||| seasons)

    let regrowTime =
      match regrowTime with
      | "0" -> failwith $"The crop {seed} had a regrow time of 0."
      | "-1" -> None
      | time -> Some (uint time)

    spriteSheetRow,
    if spriteSheetRow = forageSpriteSheetRow then
      let season =
        match seasons with
        | Seasons.Spring -> Season.Spring
        | Seasons.Summer -> Season.Summer
        | Seasons.Fall -> Season.Fall
        | Seasons.Winter -> Season.Winter
        | _ -> failwith $"Forage crop {seed} does not grow in a single season."

      if regrowTime.IsSome then failwith $"Forage crop {seed} regrows."

      let data =
        match forageCropData |> Table.tryFind season with
        | Some data -> data
        | None -> failwith $"No data was provided for {Season.name season} Forage in the config."

      if data.Foragables[0] <> itemId then
        failwith "The main item provided for {Season.name season} Forage does not match {itemId}."

      ForageCrop {
        Season = season
        Stages = growthStages
        Seed = seed
        Foragables = data.Foragables
        SeedRecipeUnlockLevel = data.SeedRecipeUnlockLevel
      }

    else
      let overrides = farmCropOverrides |> Table.tryFind seed |> Option.defaultValue FarmCropOverride.none

      let scythe =
        match scythe with
        | "0" -> false
        | "1" -> true
        | str -> failwith $"Unexpected scythe value for crop {seed}: {str}"

      let cropAmount = parseCropAmount seed overrides.Amount scythe cropAmount

      FarmCrop {
        Seasons = overrides.Seasons |> Option.defaultValue seasons
        Stages = growthStages
        RegrowTime = regrowTime
        Paddy = overrides.Paddy |> Option.defaultValue false
        Giant = overrides.Giant |> Option.defaultValue false
        Seed = seed
        Amount = cropAmount
        Item = itemId
        ExtraItem = overrides.ExtraItem
      }

  | _ -> failwith $"Unexpected crop data format for crop {seed}: {data}"


// monogame has no scale method?
// and I'd rather not create an imagemagick script
let scale scale graphics width height (texture: Color array) =
  let scaledWidth = scale * width
  let scaledHeight = scale * height
  let data = Array.zeroCreate (scaledWidth * scaledHeight)

  for y = 0 to height - 1 do
    let row = y * width
    let scaledRow = y * scale * scaledWidth

    // scale the row
    for x = 0 to width - 1 do
      texture[row + x] |> Array.fill data (scaledRow + x * scale) scale

    // copy the scaled row
    for z = 1 to scale - 1 do
      Array.blit data scaledRow data (scaledRow + z * scaledWidth) scaledWidth

  let subtexture = new Texture2D (graphics, scaledWidth, scaledHeight)
  subtexture.SetData data
  subtexture

let saveSubTexture name graphics x y width height (texture: Texture2D) =
  let sub = Array.zeroCreate (width * height)
  texture.GetData (0, Rectangle (x, y, width, height), sub, 0, sub.Length)
  let subTexture = sub |> scale imageScale graphics width height
  use file = File.OpenWrite (name + ".png")
  subTexture.SaveAsPng (file, imageScale * width, imageScale * height)

let saveStageImages graphics outputPath cropSpriteSheet spriteSheetRow crop =
  let path = Path.Combine (outputPath, string (Crop.seed crop))
  Directory.CreateDirectory path |> ignore

  let x = if spriteSheetRow % 2u = 0u then 0 else 128
  let y = (int spriteSheetRow / 2) * cropImageHeight

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

let saveItemImage graphics outputPath (itemSpriteSheet: Texture2D) item =
  let id = int item.Id
  saveSubTexture
    (Path.Combine (outputPath, string id))
    graphics
    (id * itemImageWidth % itemSpriteSheet.Width)
    (id * itemImageWidth / itemSpriteSheet.Width * itemImageHeight)
    itemImageWidth
    itemImageHeight
    itemSpriteSheet


[<EntryPoint>]
let main args =
  let stardewValleyRoot =
    match Array.tryExactlyOne args with
    | Some root -> root
    | None -> failwith "Unexpected number of command line arguments. Pass the root directory of the Stardew Valley exe as the first and only command line argument."

  let config =
    let json =
      try File.ReadAllText configPath
      with _ -> eprintfn "Error reading config file."; reraise ()

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
    new Content.ContentManager (dummyServiceProvider, stardewValleyRoot)

  let tryLoad name path =
    try content.Load path
    with _ -> eprintfn $"Error loading {name}."; reraise ()

  let inline tryLoadData name path =
    (tryLoad $"{name} data" path: Dictionary<int,_>)
    |> Seq.map (fun (KeyValue (k, v)) -> nat k * 1u<_>, v)

  let cropData =
    tryLoadData "crop" cropDataPath
    |> Seq.filter (fst >> config.SkipCrops.Contains >> not)
    |> Array.ofSeq

  let itemData = tryLoadData "item" itemDataPath |> Table.ofSeq

  let inline tryLoadSpriteSheet name path: Texture2D = tryLoad $"{name} spritesheet" path

  let cropSpriteSheet = tryLoadSpriteSheet "crop" cropSpriteSheetPath
  let itemSpriteSheet = tryLoadSpriteSheet "item" itemSpriteSheetPath

  let crops = cropData |> Array.map (fun (seed, data) ->
    parseCrop config.FarmCropOverrides config.ForageCropData seed data)
  crops |> Array.sortInPlaceBy (snd >> Crop.seed)

  let items =
    [|
      config.Products.Values |> Seq.collect (Seq.map _.Item)
      crops |> Seq.collect (snd >> Crop.items)
      crops |> Seq.map (snd >> Crop.seedItem)
    |]
    |> Seq.concat
    |> Seq.distinct
    |> Seq.map (parseItem config.ItemOverrides itemData)
    |> Array.ofSeq
  items |> Array.sortInPlaceBy _.Id

  printfn "Successfully parsed all crops and items. Press enter to output the data file and images."
  |> System.Console.ReadLine
  |> ignore

  try
    Directory.CreateDirectory config.ItemImageOutputPath |> ignore
    items |> Array.iter (saveItemImage graphics config.ItemImageOutputPath itemSpriteSheet)
  with _ -> eprintfn "Error writing the item images."; reraise ()

  try
    Directory.CreateDirectory config.CropImageOutputPath |> ignore
    crops |> Array.iter (fun (spriteSheetRow, crop) ->
      saveStageImages graphics config.CropImageOutputPath cropSpriteSheet spriteSheetRow crop)
  with _ -> eprintfn "Error writing the crop images."; reraise ()

  let data = {
    Items = items
    Products = config.Products
    FarmCrops = crops |> Array.choose (function | (_, FarmCrop crop) -> Some crop | _ -> None)
    ForageCrops = crops |> Array.choose (function | (_, ForageCrop crop) -> Some crop | _ -> None)
  }

  let dataStr = data |> Encode.extractedData |> Encode.toString 2

  try File.WriteAllText (config.DataOutputPath, dataStr)
  with _ -> eprintfn "Error writing the data file."; reraise ()

  0
