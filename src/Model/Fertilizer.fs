namespace StardewValleyStonks

open Types

type Fertilizer =
  { Name: string
    Selected: bool
    Quality: int
    Speed: float
    PriceFrom: Map<NameOf<Source>, Buy> }
  member this.Toggle = { this with Selected = not this.Selected }

type FertilizerSort =
  | ByName
  | Selected
  | Quality
  | Speed
  | Price

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
      PriceFrom = prices |> listToMap Buy.source }

  let all =
    [ create
        "Basic Fertilizer"
        1
        0.0
        [ Buy.create 100 "Pierre" ]

      create
        "Quality Fertilizer"
        2
        0.0
        [ Buy.createYear2 150 "Pierre" ]

      create
        "Speed-Gro"
        0
        0.1
        [ Buy.create 100 "Pierre" ]

      create
        "Deluxe Speed-Gro"
        0
        0.25
        [ Buy.createYear2 150 "Pierre"
          Buy.create 80 "Oasis" ] ]

type CacheFertilizer =
  { Fertilizer: Fertilizer
    Price: int
    Sources: Set<NameOf<Source>> }

module CacheFertilizer =
  let compare comparePrice a b =
    [ if comparePrice then compare b.Price a.Price
      compare a.Fertilizer.Quality b.Fertilizer.Quality
      compare a.Fertilizer.Speed b.Fertilizer.Speed ]
    |> List.map Comparison.ofInt
    |> Comparison.overall