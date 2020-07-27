namespace StardewValleyStonks

open Types

type Fertilizer =
  { Name: string
    Selected: bool
    Quality: int
    Speed: float
    PriceFrom: Map<Name<Source>, Price> }
  member this.Toggle = { this with Selected = not this.Selected }

type FertilizerSort =
  | ByName
  | Selected
  | Quality
  | Speed
  | Price

//[<RequireQualifiedAccess>]
module Fertilizer =
  let name fertilizer = fertilizer.Name
  let selected fertilizer = fertilizer.Selected
  let quality fertilizer = fertilizer.Quality
  let speed fertilizer = fertilizer.Speed

  let create
    name
    quality
    speed
    prices =
    { Name = name
      Selected = true
      Quality = quality
      Speed = speed
      PriceFrom = priceListToMap prices }