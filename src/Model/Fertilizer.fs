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
  | Quality
  | Speed
  | Price

module Fertilizer =
  let name fertilizer = fertilizer.Name
  
  let none = "No Fertilizer"

  let nameOfOption = ofNameOptionWith<Fertilizer> none
  let selected fertilizer = fertilizer.Selected
  let quality fertilizer = fertilizer.Quality
  let qualityOfOption = defaultProjection 0 quality
  let speed fertilizer = fertilizer.Speed
  let speedOfOption = defaultProjection 0.0 speed
  let priceFrom fertilizer = fertilizer.PriceFrom

  let nameOf = toNameOf name
  let private img name = "img/Fertilizers/" + name + ".png"
  let image = name >> img
  let imageOfName (name: NameOf<Fertilizer>) = img <| ofName name

  let create
    name
    quality
    speed
    prices =
    { Name = name
      Selected = true
      Quality = quality
      Speed = speed
      PriceFrom = prices |> toMapByKey Buy.source }

type Comparison =
  | Better
  | Equal
  | Worse

module Comparison =
  let ofInt = function
    | 0 -> Equal
    | x when x > 0 -> Better
    | _ -> Worse

  let overall = function
    | [] -> None
    | first::rest ->
        let rec helper comparison list =
          match list with
          | [] -> Some comparison
          | head::tail ->
              match ofInt head with
              | Equal -> helper comparison tail
              | x -> if x = comparison then helper comparison tail else None
        helper (ofInt first) rest

type CacheFertilizer =
  { Fertilizer: Fertilizer
    Price: int
    Sources: Set<NameOf<Source>> }

module CacheFertilizer =
  let compare comparePrice a b =
    [ if comparePrice then compare b.Price a.Price
      compare a.Fertilizer.Quality b.Fertilizer.Quality
      compare a.Fertilizer.Speed b.Fertilizer.Speed ]
    |> Comparison.overall