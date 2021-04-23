namespace StardewValleyStonks

[<Fable.Core.Erase>]
type Source = SourceName of string

[<Fable.Core.Erase>]
type PriceMultiplier = PriceMultiplier of name: string * value: float * applyOnSelected: bool

module PriceMultiplier =
  let inline name (PriceMultiplier (name, _,_)) = name
  let inline value (PriceMultiplier (_, value ,_)) = value
  let inline applyOnSelected (PriceMultiplier (_,_, a)) = a


type Price =
  { Value: int
    Source: Source
    Multiplier: PriceMultiplier option }

module Price =
  let rawValue price = price.Value
  let source price = price.Source
  let multiplier price = price.Multiplier

  let value multiplierSelected price =
    match price.Multiplier with
    | Some (PriceMultiplier(_, v, a) as pm) when a = multiplierSelected pm -> intMulti v price.Value
    | _ -> price.Value

  let valueWithAlternate multiplierSelected price =
    match price.Multiplier with
    | Some (PriceMultiplier(_, v, a) as pm) when multiplierSelected pm ->
        if a
        then intMulti v price.Value, Some price.Value
        else price.Value, Some <| intMulti v price.Value
    | _ -> price.Value, None

  let validPrice = positive

  let createWith multiplier source price =
    { Value = validPrice price
      Source = source
      Multiplier = multiplier }

  let create = createWith None
  let createMultiplier = Some >> createWith



type Prices =
  { Id: int
    From: Map<Source, Price>
    Selected: Price Set }

module Prices =
  let id prices = prices.Id
  let from prices = prices.From
  let selected prices = prices.Selected
  let sourceSelected source prices = prices.From.TryFind source |> Option.forall prices.Selected.Contains

  let lowest priceValue prices = prices.Selected |> Seq.map priceValue |> Seq.tryMin
  let hasOne (lowest: Prices -> _) = lowest >> Option.isSome

  let setSelected add source prices =
    match prices.From.TryFind source with
    | Some price ->
        { prices with Selected = prices.Selected |> Set.tryAddOrRemove add price }
    | None -> prices

  let toggle price prices =
    { prices with Selected = prices.Selected |> Set.addOrRemove price }

  let mutable private nextId = -1

  let create prices =
    nextId <- nextId + 1
    { Id = nextId
      From = prices |> Map.ofValues Price.source
      Selected = set prices }



type Fertilizer =
  { Name: string
    Quality: int
    Speed: float
    Prices: Prices }

[<Fable.Core.StringEnum>]
type FertilizerSort =
  | [<CompiledName("Fertilizer Name")>] FertilizerName
  | [<CompiledName("Quality")>] Quality
  | [<CompiledName("Speed")>] Speed
  | [<CompiledName("Cost")>] Cost


module Fertilizer =
  let name fertilizer = fertilizer.Name
  let nameOption = defaultMap "No Fertilizer" name
  let quality fertilizer = fertilizer.Quality
  let qualityOption = defaultMap 0 quality
  let speed fertilizer = fertilizer.Speed
  let speedOption = defaultMap 0.0 speed

  let nameKey: _ -> Fertilizer name = name >> Name

  let prices fertilizer = fertilizer.Prices
  let lowestPrice (lowest: Prices -> int option) = prices >> lowest
  let lowestPriceOption (lowest: Prices -> _) = defaultMap (Some 0) (prices >> lowest)
  let hasAPrice (hasOne: Prices -> bool) = prices >> hasOne
  let hasAPriceOption = hasAPrice >> Option.forall

  let validQuality = positive
  let validSpeed = positive

  let private mapPrices mapping fertilizer =
    { fertilizer with Prices = mapping fertilizer.Prices }

  let togglePrice = Prices.toggle >> mapPrices
  let setPriceSelected = Prices.setSelected >>| mapPrices

  let create name quality speed prices =
    { Name = name
      Quality = validQuality quality
      Speed = validSpeed speed
      Prices = Prices.create prices }
