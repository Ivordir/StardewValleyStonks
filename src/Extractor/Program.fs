﻿module StardewValleyStonks.Extractor

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

module [<RequireQualifiedAccess>] ItemOverride =
  let none = {
    SellPrice = None
    Category = None
  }

type CropAmountOverride = {
  CanDouble: bool option
  FarmingDistribution: bool option
}

module [<RequireQualifiedAccess>] CropAmountOverride =
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

module [<RequireQualifiedAccess>] FarmCropOverride =
  let none = {
    Seasons = None
    Paddy = None
    Giant = None
    Amount = None
    ExtraItem = None
  }

type ForageCropData = {
  SeedRecipeUnlockLevel: nat
  Items: ItemId array
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

module [<RequireQualifiedAccess>] Decode =
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
      }
    )

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
            (Decode.tableParse Decode.parseItemId (Decode.array Decode.processedItem))
        SkipCrops = field (nameof u.SkipCrops) (Decode.set Decode.seedId)
        FarmCropOverrides =
          field
            (nameof u.FarmCropOverrides)
            (Decode.tableParse Decode.parseSeedId farmCropOverride)
        ForageCropData =
          field
            (nameof u.ForageCropData)
            (Decode.tableParse (Season.TryParse >> function true, s -> Some s | _ -> None) (Decode.Auto.generateDecoder ()))
        DataOutputPath = field (nameof u.DataOutputPath) Decode.string
        CropImageOutputPath = field (nameof u.CropImageOutputPath) Decode.string
        ItemImageOutputPath = field (nameof u.ItemImageOutputPath) Decode.string
      }
    )


module [<AutoOpen>] Constants =
  let [<Literal>] forageSpriteSheetRow = 23u
  let [<Literal>] cropImageWidth = 16
  let [<Literal>] cropImageHeight = 32
  let [<Literal>] itemImageWidth = 16
  let [<Literal>] itemImageHeight = 16

  let [<Literal>] configPath = "config.json"

  let [<Literal>] cropDataPath = "Content/Data/Crops"
  let [<Literal>] itemDataPath = "Content/Data/ObjectInformation"
  let [<Literal>] cropSpriteSheetPath = "Content/TileSheets/crops"
  let [<Literal>] itemSpriteSheetPath = "Content/Maps/springobjects"


let parseItem overrrides itemData itemId =
  match itemData |> Table.tryFind itemId with
  | None -> failwith $"Could not find data for item with id: {itemId}"
  | Some (str: string) ->
    let splitted = str.Split '/'
    if splitted.Length < 5 then failwith $"Unexpected item data format for item {itemId}: {str}"

    let o = overrrides |> Table.tryFind itemId |> Option.defaultValue ItemOverride.none

    let sellPrice =
      match o.SellPrice with
      | Some price -> price
      | None -> nat splitted[1]

    let category =
      match o.Category with
      | Some category -> category
      | None ->
        match splitted[3] with
        | "Seeds -74" -> Seeds
        | "Basic -75" -> Vegetable
        | "Basic -79" -> Fruit
        | "Basic -80" -> Flower
        | "Basic -81" -> Forage
        | "Basic -26" -> ArtisanGood
        | "Basic" // sugar, flour, etc
        | "Basic -16" // building resources (fiber)
        | "Basic -17" // sweet gem berry
        | "Crafting" // (coffee)
          -> Other
        | str -> failwith $"Unexpected item category for item {itemId}: {str}"

    {
      Id = itemId
      Name = splitted[4]
      SellPrice = sellPrice
      Category = category
    }

let parseCrop farmCropOverrides forageCropData seedId (data: string) =
  match data.Split '/' with
  | [| growthStages; seasons; spriteSheetRow; itemId; regrowTime; scythe; cropAmount; _; _ |] ->
    let spriteSheetRow = uint spriteSheetRow
    let itemId = nat itemId * 1u<_>

    let growthStages =
      let splitted = growthStages.Split ' '
      if splitted.Length = 0 then failwith $"The crop {seedId} had no growth stages." else
      let stages = splitted |> Array.map uint
      if stages |> Array.contains 0u then failwith $"The crop {seedId} had a growth stage of 0."
      stages

    let seasons =
      seasons.Split ' ' |> Array.fold (fun seasons season ->
        Seasons.Parse (season, true) ||| seasons)
        Seasons.None

    let regrowTime =
      match regrowTime with
      | "0" -> failwith $"The crop {seedId} had a regrow time of 0."
      | "-1" -> None
      | time -> Some (uint time)

    spriteSheetRow,
    if spriteSheetRow = forageSpriteSheetRow then
      let season =
        match Seasons.tryExactlyOne seasons with
        | Some season -> season
        | None -> failwith $"Forage crop {seedId} does not grow in a single season."

      if regrowTime.IsSome then failwith $"Forage crop {seedId} regrows."

      let data =
        match forageCropData |> Table.tryFind season with
        | Some data -> data
        | None -> failwith $"No data was provided for {Season.name season} Forage in the config."

      if data.Items[0] <> itemId then failwith "The main item provided for {Season.name season} Forage does not match {itemId}."

      ForageCrop {
        Season = season
        Stages = growthStages
        Seed = seedId
        Items = data.Items
        SeedRecipeUnlockLevel = data.SeedRecipeUnlockLevel
      }

    else
      let overrides = farmCropOverrides |> Table.tryFind seedId |> Option.defaultValue FarmCropOverride.none

      let scythe =
        match scythe with
        | "0" -> false
        | "1" -> true
        | str -> failwith $"Unexpected scythe value for crop {seedId}: {str}"

      let cropAmount =
        match cropAmount.Split ' ' with
        | [| "false" |] ->
          match overrides.Amount with
          | None -> CropAmount.singleAmount
          | Some o ->
            let amount = CropAmount.singleAmount
            { amount with
                CanDouble = o.CanDouble |> Option.defaultValue amount.CanDouble
                FarmingQualities = o.FarmingDistribution |> Option.defaultValue amount.FarmingQualities
            }

        | [| "true"; minHarvest; maxHarvest; increasePerLevel; extraChance |] ->
          let o = overrides.Amount |> Option.defaultValue CropAmountOverride.none
          let minHarvest = uint minHarvest
          if minHarvest < CropAmount.minYield then failwith $"The minimum harvest for crop {seedId} was below {CropAmount.minYield}."
          let maxHarvest = uint maxHarvest
          if minHarvest > maxHarvest then failwith $"The min harvest ({minHarvest}) was greater than the max harvest ({maxHarvest}) for crop {seedId}."
          let extraChance = float extraChance
          if extraChance < CropAmount.minExtraCropChance || CropAmount.maxExtraCropChance < extraChance then
            failwith $"The extra crop chance for crop {seedId} was not in the range [{CropAmount.minExtraCropChance}, {CropAmount.maxExtraCropChance}]."
          {
            MinCropYield = minHarvest
            MaxCropYield = maxHarvest
            FarmLevelsPerYieldIncrease = uint increasePerLevel
            ExtraCropChance = extraChance
            CanDouble = o.CanDouble |> Option.defaultValue scythe
            FarmingQualities = o.FarmingDistribution |> Option.defaultValue true
          }

        | _ -> failwith $"Unexpected crop amount format for crop {seedId}: {cropAmount}"

      FarmCrop {
        Seasons = overrides.Seasons |> Option.defaultValue seasons
        Stages = growthStages
        RegrowTime = regrowTime
        Paddy = overrides.Paddy |> Option.defaultValue false
        Giant = overrides.Giant |> Option.defaultValue false
        Seed = seedId
        Amount = cropAmount
        Item = itemId
        ExtraItem = overrides.ExtraItem
      }

  | _ -> failwith $"Unexpected crop data format for crop {seedId}: {data}"


let getSubTexture graphics x y width height (texture: Texture2D) =
  let data: Color array = Array.zeroCreate (texture.Width * texture.Height)
  texture.GetData data
  let sub = Array.zeroCreate (width * height)
  for y0 = 0 to height - 1 do
    let y1 = y0 * width
    let y2 = (y0 + y) * texture.Width
    for x0 = 0 to width - 1 do
      sub.[x0 + y1] <- data.[x0 + x + y2]
  let subtexture = new Texture2D (graphics, width, height)
  subtexture.SetData sub
  subtexture

let saveStageImages graphics outputPath cropSpriteSheet crop (spriteSheetRow: uint) =
  let path = Path.Combine (outputPath, string (Crop.seed crop))
  Directory.CreateDirectory path |> ignore

  let x = if spriteSheetRow % 2u = 0u then 0 else 128
  let y = (int spriteSheetRow / 2) * cropImageHeight

  let save name i =
    let subTexture = cropSpriteSheet |> getSubTexture graphics (x + i * cropImageWidth) y cropImageWidth cropImageHeight
    use file = Path.Combine (path, name + ".png") |> File.OpenWrite
    subTexture.SaveAsPng (file, cropImageWidth, cropImageHeight)

  let stages = Crop.stages crop

  save (string 0) 0
  for i = 1 to stages.Length do
    save (string i) (i + 1)

  if Crop.regrows crop then save "Regrow" (stages.Length + 2)

let saveItemImage graphics outputPath (itemSpriteSheet: Texture2D) item =
  let id = int item.Id
  let subTexture =
    getSubTexture
      graphics
      (id * itemImageWidth % itemSpriteSheet.Width)
      (id * itemImageWidth / itemSpriteSheet.Width * itemImageHeight)
      itemImageWidth
      itemImageHeight
      itemSpriteSheet
  use file = Path.Combine (outputPath, string id + ".png") |> File.OpenWrite
  subTexture.SaveAsPng (file, itemImageWidth, itemImageHeight)


let [<EntryPoint>] main args =
  let stardewValleyRoot =
    match Array.tryExactlyOne args with
    | Some root -> root
    | None -> failwith "Unexpected number of command line arguments. Pass the root directory of the Stardew Valley exe as the first and only command line argument."

  let config =
    let json =
      try File.ReadAllText configPath
      with _ -> printfn "Error reading config file."; reraise ()

    match Decode.fromString Decode.config json with
    | Ok config -> config
    | Error e -> failwithf "Error parsing config file: %s" e

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
    with _ -> printfn $"Error loading {name}."; reraise ()

  let inline tryLoadData name path =
    (tryLoad (name + " data") path: Dictionary<int,_>)
    |> Seq.map (fun (KeyValue (k, v)) -> nat k * 1u<_>, v)

  let cropData = tryLoadData "crop" cropDataPath |> Seq.filter (fst >> config.SkipCrops.Contains >> not) |> Array.ofSeq
  let itemData = tryLoadData "item" itemDataPath |> Table.ofSeq

  let inline tryLoadSpriteSheet name path: Texture2D = tryLoad (name + " spritesheet") path

  let cropSpriteSheet = tryLoadSpriteSheet "crop" cropSpriteSheetPath
  let itemSpriteSheet = tryLoadSpriteSheet "item" itemSpriteSheetPath

  let crops = cropData |> Array.map (fun (seedId, data) ->
    parseCrop config.FarmCropOverrides config.ForageCropData seedId data)
  crops |> Array.sortInPlaceBy (snd >> Crop.seed)

  let items =
    [|
      config.Products.Values |> Seq.collect (Seq.map (fun p -> p.Item))
      crops |> Seq.collect (snd >> Crop.items)
      crops |> Seq.map (snd >> Crop.seedItem)
    |]
    |> Seq.concat
    |> Seq.distinct
    |> Seq.map (parseItem config.ItemOverrides itemData)
    |> Array.ofSeq
  items |> Array.sortInPlaceBy Item.id

  printfn "Successfully parsed all crops and items. Press enter to output the data file and images."
  |> System.Console.ReadLine
  |> ignore

  try
    Directory.CreateDirectory config.ItemImageOutputPath |> ignore
    items |> Array.iter (saveItemImage graphics config.ItemImageOutputPath itemSpriteSheet)
  with _ -> printfn "Error writing the item images."; reraise ()

  try
    Directory.CreateDirectory config.CropImageOutputPath |> ignore
    crops |> Array.iter (fun (spriteSheetRow, crop) -> saveStageImages graphics config.CropImageOutputPath cropSpriteSheet crop spriteSheetRow)
  with _ -> printfn "Error writing the crop images."; reraise ()

  let dataStr =
    Encode.extractedData {
      Items = items
      Products = config.Products
      FarmCrops = crops |> Array.choose (function | (_, FarmCrop crop) -> Some crop | _ -> None)
      ForageCrops = crops |> Array.choose (function | (_, ForageCrop crop) -> Some crop | _ -> None)
    }
    |> Encode.toString 2

  try File.WriteAllText (config.DataOutputPath, dataStr)
  with _ -> printfn "Error writing the data file."; reraise ()

  0
