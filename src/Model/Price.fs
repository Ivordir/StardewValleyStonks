namespace StardewValleyStonks

open Types

type Source =
  { Name: string
    Selected: bool }
  member this.Toggle = { this with Selected = not this.Selected }

module Source =
  let name source = source.Name

  let nameOf = toNameOf name

  let create name =
    { Name = name
      Selected = true }

type Price =
  { Value: int
    Source: NameOf<Source>
    SourceOverride: Override }

module Price =
  let create price source =
    { Value = price
      Source = Name source
      SourceOverride = None }

type PriceMultiplier =
  { Name: string
    Value: float
    MultiplyWhenSelected: bool
    Selected: bool }
  member this.Toggle = { this with Selected = not this.Selected }

module PriceMultiplier =
  let name price = price.Name

  let nameOf = toNameOf name

  let create name multiplier multiplyWhenSelected =
    { Name = name
      Value = multiplier
      MultiplyWhenSelected = multiplyWhenSelected
      Selected = false }

type Buy =
  | BuyPrice of Price
  // | MultiplierPrice of NameOf<PriceMultiplier> * Price
  | RelativePrice of
      {| Multiplier: NameOf<PriceMultiplier>
         RelativeTo: NameOf<Source>
         Source: NameOf<Source>
         SourceOverride: Override |}

type CreatePrices =
  | Pierre
  | JojaWith of PierrePrice: Price
  | PierreAndJoja
  | Oasis
  | PriceList of Buy list
  | NoPrice

module Buy =
  let source = function
    | BuyPrice p -> p.Source
    | RelativePrice r -> r.Source
  
  let sourceName = source >> ofName

  let sourceOverride = function
    | BuyPrice p -> p.SourceOverride
    | RelativePrice r -> r.SourceOverride

  let create = Price.create >>| BuyPrice

  let createSeedPrice = (*) 2 >> Price.create
  let createBuySeed = createSeedPrice >>| BuyPrice

  let createRelative source multiplier relative =
    RelativePrice
      {| Source = Name source
         Multiplier = Name multiplier
         RelativeTo = Name relative
         SourceOverride = None |}

  let joja = createRelative "Joja" "Joja Membership" "Pierre"

  let createAll seedSellPrice = function
    | Pierre -> [ createBuySeed seedSellPrice "Pierre" ]
    | JojaWith pierrePrice ->
        [ BuyPrice pierrePrice
          joja ]
    | PierreAndJoja ->
        [ createBuySeed seedSellPrice "Pierre"
          joja ]
    | Oasis -> [ createBuySeed seedSellPrice "Oasis" ]
    | PriceList prices -> prices
    | NoPrice -> []

type Quality =
  | Normal
  | Silver
  | Gold
  | Iridium

module Quality =
  let multiplier = function
    | Normal -> 1.0
    | Silver -> 1.25
    | Gold -> 1.5
    | Iridium -> 2.0

  let color = function
    | Normal -> "white"
    | Iridium -> "purple"
    | x -> string x |> lower

  let multiply = multiplier >> apply

  let singleWith amount quality : Map<Quality, float> = Map.ofList [ quality, amount ]
  let single = singleWith 1.0

  let common =
    [ Gold
      Silver
      Normal ]

  let all = Iridium::common