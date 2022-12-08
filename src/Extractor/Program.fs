module StardewValleyStonks.Extractor

open System.IO
open Thoth.Json.Net
open Microsoft.Xna.Framework
open Microsoft.Xna.Framework.Content
open Microsoft.Xna.Framework.Graphics
open StardewValleyStonks
open StardewValleyStonks.Json

type ItemOverride = {
  SellPrice: nat option
  Category: Category option
}

module ItemOverride =
  let none = {
    SellPrice = None
    Category = None
  }

type CropAmountOverride = {
  CanDouble: bool option
  FarmingDistribution: bool option
}

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
  Items: ItemId array
}

type Config = {
  SkipCrops: SeedId Set
  IncludeItems: ItemId array
  ItemOverrides: Table<ItemId, ItemOverride>
  FarmCropOverrides: Table<SeedId, FarmCropOverride>
  ForageCropData: Table<Season, ForageCropData>

  DataOutputPath: string
  CropImageOutputPath: string
  ItemImageOutputPath: string
}

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
      }
    )

  let config =
    let u = Unchecked.defaultof<Config>
    Decode.object (fun get ->
      let inline field name decode = get.Required.Field name decode
      {
        SkipCrops = field (nameof u.SkipCrops) (Decode.set Decode.seedId)
        IncludeItems = field (nameof u.IncludeItems) (Decode.array Decode.itemId)
        ItemOverrides =
          field
            (nameof u.ItemOverrides)
            (Decode.tableParse Decode.parseItemId (Decode.Auto.generateDecoder ()))
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


[<AutoOpen>]
module Constants =
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


let {
  SkipCrops = skipCrops
  IncludeItems = includeItems
  ItemOverrides = itemOverrides
  FarmCropOverrides = farmCropOverrides
  ForageCropData = forageCropData
  DataOutputPath = dataOutputPath
  CropImageOutputPath = cropImageOutputPath
  ItemImageOutputPath = itemImageOutputPath
} =
  let json =
    try File.ReadAllText configPath
    with _ -> printfn "Error reading config file."; reraise ()

  match Decode.fromString Decode.config json with
  | Ok config -> config
  | Error e -> failwithf "Error parsing config file: %A" e


let game = new Game ()
let graphicsManager = new GraphicsDeviceManager (game)
(graphicsManager :> IGraphicsDeviceManager).CreateDevice ()
let graphics = graphicsManager.GraphicsDevice

game.RunOneFrame ()


let parseItem (items: Dictionary<_,_>) (itemData: Table<_,_>) itemId =
  if items.ContainsKey itemId then () else
  match itemData.TryFind itemId with
  | None -> failwith $"Could not find data for item with id: {itemId}"
  | Some (str: string) ->
    let splitted = str.Split '/'
    if splitted.Length < 5 then failwith $"Unexpected item data format for item {itemId}: '{str}'"
    let o = itemOverrides.TryFind itemId |> Option.defaultValue ItemOverride.none

    let sellPrice = o.SellPrice |> Option.defaultValue (uint splitted.[1])

    let category =
      match splitted[3] with
      | "Basic" // sugar, flour, etc
      | "Basic -16" // buildingResources (fiber)
      | "Basic -17" // sweet gem berry
      | "Basic -81" // forage
      | "Crafting" // (coffee bean)
        -> Other
      | "Seeds -74" -> Seeds
      | "Basic -75" -> Vegetable
      | "Basic -79" -> Fruit
      | "Basic -80" -> Flower
      | "Basic -26" -> ArtisanGood
      | str -> failwith $"Unexpected item category for item {itemId}: {str}"

    let category = o.Category |> Option.defaultValue category

    items.Add (itemId, {
      Id = itemId
      Name = splitted[4]
      SellPrice = sellPrice
      Category = category
    } )

let parseCrop parseItem seedId (data: string) =
  match data.Split '/' with
  | [| growthStages; seasons; spriteSheetRow; itemId; regrowTime; scythe; cropAmount; _; _ |] ->
    let spriteSheetRow = uint spriteSheetRow
    let itemId = nat itemId * 1u<_>
    parseItem (seedId * 1u<_>)

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
        match forageCropData.TryFind season with
        | Some data -> data
        | None -> failwith $"No data was provided for {Season.name season} Forage in the config."

      if data.Items[0] <> itemId then failwith "The main item provided for {Season.name season} Forage does not match {itemId}."

      data.Items |> Array.iter parseItem

      ForageCrop {
        Season = season
        Stages = growthStages
        Seed = seedId
        Items = data.Items
        SeedRecipeUnlockLevel = data.SeedRecipeUnlockLevel
      }

    else
      parseItem itemId

      let overrides = farmCropOverrides.TryFind seedId |> Option.defaultValue FarmCropOverride.none

      overrides.ExtraItem |> Option.iter (fst >> parseItem)

      let scythe =
        match scythe with
        | "0" -> false
        | "1" -> true
        | _ -> failwith $"Unexpected scythe value for crop {seedId}."

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

        | _ -> failwith $"Unexpected crop amount format for crop {seedId}: '{cropAmount}'"

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

  | _ -> failwith $"Unexpected crop data format for crop {seedId}: '{data}'"




let getSubTexture x y width height (texture: Texture2D) =
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

let saveStageImages cropSpriteSheet crop (spriteSheetRow: uint) =
  let path = Path.Combine (cropImageOutputPath, string (Crop.seed crop))
  if not <| Directory.Exists path then Directory.CreateDirectory path |> ignore

  let x = if spriteSheetRow % 2u = 0u then 0 else 128
  let y = (int spriteSheetRow / 2) * cropImageHeight

  let save name i =
    let subTexture = cropSpriteSheet |> getSubTexture (x + i * cropImageWidth) y cropImageWidth cropImageHeight
    use file = Path.Combine (path, name + ".png") |> File.OpenWrite
    subTexture.SaveAsPng (file, cropImageWidth, cropImageHeight)

  save (string 0) 0
  let stages = Crop.stages crop
  for i = 1 to stages.Length - (if Crop.isForage crop then 1 else 0) do
    save (string i) (i + 1)

  if Crop.regrows crop then save "Regrow" (stages.Length + 2)

let saveItemImage (itemSpriteSheet: Texture2D) item =
  let id = int item.Id
  let subTexture =
    getSubTexture
      (id * itemImageWidth % itemSpriteSheet.Width)
      (id * itemImageWidth / itemSpriteSheet.Width * itemImageHeight)
      itemImageWidth
      itemImageHeight
      itemSpriteSheet
  use file = Path.Combine (itemImageOutputPath, string id + ".png") |> File.OpenWrite
  subTexture.SaveAsPng (file, itemImageWidth, itemImageHeight)



[<EntryPoint>]
let main args =
  let stardewValleyRoot =
    match args with
    | [| root |] -> root
    | [| |] -> failwith "Pass the root directory of the Stardew Valley exe as the first command line argument."
    | _ -> failwith "Unexpected number of command line arguments."

  printfn "Press enter to run the extractor..."
  |> System.Console.ReadLine
  |> ignore

  let content =
    let dummyServiceProvider = {
      new System.IServiceProvider with
        member _.GetService t =
          if t = typeof<IGraphicsDeviceService>
          then graphicsManager :> obj
          else null
    }
    new ContentManager (dummyServiceProvider, stardewValleyRoot)

  let tryLoad name path =
    try content.Load path
    with _ -> printfn $"Error loading {name}."; reraise ()

  let inline tryLoadData name path =
    (tryLoad (name + " data") path: Dictionary<int,_>)
    |> Seq.map (fun (KeyValue (k, v)) -> nat k * 1u<_>, v)

  let cropData = tryLoadData "crop" cropDataPath |> Seq.filter (fst >> skipCrops.Contains >> not) |> Array.ofSeq
  let itemData = tryLoadData "item" itemDataPath |> Table.ofSeq

  let tryLoadSpriteSheet name path: Texture2D = tryLoad (name + " spritesheet") path

  let cropSpriteSheet = tryLoadSpriteSheet "crop" cropSpriteSheetPath
  let itemSpriteSheet = tryLoadSpriteSheet "item" itemSpriteSheetPath

  let items = Dictionary ()
  let parseItem = parseItem items itemData
  includeItems |> Array.iter parseItem

  let farmCrops = ResizeArray (cropData.Length - 4)
  let forageCrops = ResizeArray 4
  for seedId, data in cropData do
    match parseCrop parseItem seedId data with
    | spriteSheetRow, FarmCrop crop -> farmCrops.Add (crop, spriteSheetRow)
    | _, ForageCrop crop -> forageCrops.Add crop

  printfn "Successfully parsed all items and crops. Press enter to output the data file and images."
  |> System.Console.ReadLine
  |> ignore

  items.Values |> Seq.iter (saveItemImage itemSpriteSheet)
  farmCrops |> Seq.iter (fun (crop, spriteSheetRow) -> saveStageImages cropSpriteSheet (FarmCrop crop) spriteSheetRow)
  forageCrops |> Seq.iter (fun crop -> saveStageImages cropSpriteSheet (ForageCrop crop) forageSpriteSheetRow)

  let dataStr =
    Encode.extractedData {
      Items = Array.ofSeq items.Values
      FarmCrops = farmCrops |> Seq.map fst |> Array.ofSeq
      ForageCrops = forageCrops.ToArray ()
    }
    |> Encode.toString 2

  try File.WriteAllText (dataOutputPath, dataStr)
  with _ -> printfn "Error writing the data file."; reraise ()

  0
