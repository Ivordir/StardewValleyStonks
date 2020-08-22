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

  let all =
    [ "Pierre"
      "Joja"
      "Oasis"
      "Traveling Merchant"
      "Crafting" ]
    |> List.map create

type MatchCondition =
  { Name: string
    Selected: bool }
  member this.Toggle = { this with Selected = not this.Selected }

module MatchCondition =
  let name matchCondition = matchCondition.Name

  let nameOf = toNameOf name

  let create name =
    { Name = name
      Selected = false }

  let all =
    [ "Joja Membership" ]
    |> List.map create

[<Fable.Core.StringEnum>]
type RequirementsShould =
  | [<CompiledName("Require")>] Require
  | [<CompiledName("Warn")>] Warn
  | [<CompiledName("Ignore")>] Ignore

module RequirementsShould =
  let all =
    [ Require
      Warn
      Ignore ]

type Requirement =
  | SkillLevel of Skill: NameOf<Skill> * Level: int
  | Year of int

module Requirement =
  let status isMet = function
    | Ignore -> Valid
    | Warn | Require when isMet -> Valid
    | Warn -> Warning
    | Require -> Invalid

  let seedMaker = [ SkillLevel (Name "Farming", 9) ]
  let year2 = [ Year 2 ]

type Alert =
  | UnmetRequirement of Requirement
  | Message of string
  | AlertList of Message: string * SubAlerts: Alert list

module Alert =
  let notSelected = Message "Is not selected."
  let overridden = Message "Is manually overridden to 'Never'."

type Price =
  { Value: int
    Source: NameOf<Source>
    Requirements: Requirement list
    SourceOverride: Override }

module Price =
  let createWith requirement price source =
    { Value = price
      Source = Name source
      Requirements = requirement
      SourceOverride = None }

  let create = createWith List.empty

type Buy =
  | BuyPrice of Price
  | MatchPrice of
      {| OwnPrice: Price
         MatchPriceFrom: NameOf<Source>
         MatchCondition: NameOf<MatchCondition> |}

type CreatePrices =
  | Pierre
  | JojaWith of PierrePrice: Price
  | Oasis
  | PierreAndJoja
  | PriceList of Buy list
  | NoPrice

module Buy =
  let value = function
    | BuyPrice p -> p.Value
    | MatchPrice m -> m.OwnPrice.Value

  let source = function
    | BuyPrice p -> p.Source
    | MatchPrice m -> m.OwnPrice.Source
  
  let overrideSource = function
    | BuyPrice p -> p.SourceOverride
    | MatchPrice m -> m.OwnPrice.SourceOverride

  let requirements = function
    | BuyPrice p -> p.Requirements
    | MatchPrice m -> m.OwnPrice.Requirements

  let create price source = BuyPrice <| Price.create price source

  let createYear2 price source = BuyPrice <| Price.createWith Requirement.year2 price source

  let createSeedPrice seedSellPrice = Price.create (seedSellPrice * 2)
  let createBuySeed seedSellPrice source = BuyPrice <| createSeedPrice seedSellPrice source

  let createMatch price source matchSource matchCondition =
    MatchPrice
      {| OwnPrice = Price.create price source
         MatchPriceFrom = Name matchSource
         MatchCondition = Name matchCondition |}

  let createJojaPrice pierrePrice = createMatch (float pierrePrice.Value * 1.25 |> int) "Joja" "Pierre" "Joja Membership"

  let createAll seedSellPrice = function
    | Pierre -> [ createBuySeed seedSellPrice "Pierre" ]
    | JojaWith pierrePrice ->
        [ BuyPrice pierrePrice
          createJojaPrice pierrePrice ]
    | Oasis -> [ createBuySeed seedSellPrice "Oasis" ]
    | PierreAndJoja ->
        let pierrePrice = createSeedPrice seedSellPrice "Pierre"
        [ BuyPrice pierrePrice
          createJojaPrice pierrePrice ]
    | PriceList list -> list
    | NoPrice -> List.empty