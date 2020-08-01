namespace StardewValleyStonks

open Types

type Fertilizer =
  { Name: string
    Selected: bool
    Quality: int
    Speed: float
    PriceFrom: Map<NameOf<Source>, Price> }
  member this.Toggle = { this with Selected = not this.Selected }

type FertilizerSort =
  | ByName
  | Selected
  | Quality
  | Speed
  | Price

type CacheFertilizer =
  { Fertilizer: Fertilizer
    Price: int
    Sources: NameOf<Source> list }

module Fertilizer =
  let name fertilizer = fertilizer.Name
  let selected fertilizer = fertilizer.Selected
  let quality fertilizer = fertilizer.Quality
  let speed fertilizer = fertilizer.Speed
  let priceFrom fertilizer = fertilizer.PriceFrom

  let nameOf = toNameOf name

  let create
    name
    quality
    speed
    prices =
    { Name = name
      Selected = true
      Quality = quality
      Speed = speed
      PriceFrom = prices |> listToMap Price.nameOf }

  let all =
    [ create
          "Basic Fertilizer"
          1
          0.0
          [ Price.create 100 "Pierre" ]

      create
        "Quality Fertilizer"
        2
        0.0
        [ BuyPrice
            {| Value = 150
               Source = Name "Pierre"
               Requirements = [ Year 2 ]
               SourceOverride = None |} ]

      create
        "Speed-Gro"
        0
        0.1
        [ Price.create 100 "Pierre" ]

      create
        "Deluxe Speed-Gro"
        0
        0.25
        [ BuyPrice
            {| Value = 150
               Source = Name "Pierre"
               Requirements = [ Year 2 ]
               SourceOverride = None |}
          Price.create 80 "Oasis" ] ]