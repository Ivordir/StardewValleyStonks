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

type Requirement =
  | SkillLevel of Skill: NameOf<Skill> * Level: int
  | Year of int

module Requirement =
  let status isMet = function
    | Ignore -> Valid
    | Warn | Invalidate when isMet -> Valid
    | Warn -> Warning
    | Invalidate -> Invalid

  let seedMaker = [ SkillLevel (Name "Farming", 9) ]
  let year2 = [ Year 2 ]

type Alert =
  | UnmetRequirement of Requirement
  | Alert of string
  | AlertList of Alert: string * SubAlerts: Alert list

module Alert =
  let notSelected = Alert "Is not selected."
  let overridden = Alert "Is manually overridden to false."

type StatusData =
  | ValidD
  | WarningD of Alert
  | InvalidD of Alert

module StatusData =
  let validPrecedence = function
    | ValidD -> 2
    | WarningD _ -> 1
    | InvalidD _ -> 0
  
  let invalidPrecedence = function
    | ValidD -> 0
    | WarningD _ -> 1
    | InvalidD _ -> 2

  let WVIPrecedence = function
    | WarningD _ -> 2
    | ValidD -> 1
    | InvalidD _ -> 0

  let compare (precedence: StatusData -> int) a b = if precedence a > precedence b then a else b
  
  let compareValid = compare validPrecedence
  let compareInvalid = compare invalidPrecedence
  let compareWVI = compare WVIPrecedence

type Price =
  | BuyPrice of
      {| Value: int
         Source: NameOf<Source>
         Requirements: Requirement list
         SourceOverride: bool option |}
  | MatchPrice of
      {| Value: int
         Source: NameOf<Source>
         Requirements: Requirement list
         SourceOverride: bool option
         MatchSource: NameOf<Source>
         MatchCondition: NameOf<MatchCondition> |}

type CreatePrices =
  | Pierre
  | Joja of PierrePrice: Price
  | Oasis
  | PierreAndJoja
  | PriceList of Price list
  | NoPrice

module Price =
  let value = function
    | BuyPrice p -> p.Value
    | MatchPrice m -> m.Value

  let source = function
    | BuyPrice p -> p.Source
    | MatchPrice m -> m.Source
  
  let overrideSource = function
    | BuyPrice p -> p.SourceOverride
    | MatchPrice m -> m.SourceOverride

  let requirements = function
    | BuyPrice p -> p.Requirements
    | MatchPrice m -> m.Requirements

  let nameOf = source

  let (.*) price multiplier = value price |> float |> (*) multiplier |> int

  let create value source =
    BuyPrice
      {| Value = value
         Source = Name source
         Requirements = List.empty
         SourceOverride = None |}

  let createYear2 value source =
    BuyPrice
      {| Value = value
         Source = Name source
         Requirements = Requirement.year2
         SourceOverride = None |}

  let createSeed seedSellPrice = create (seedSellPrice * 2)

  let createMatch value source matchSource matchCondition =
    MatchPrice
      {| Value = value
         Source = Name source
         Requirements = List.empty
         SourceOverride = None
         MatchSource = Name matchSource
         MatchCondition = Name matchCondition |}

  let createJojaPrice pierrePrice = createMatch (pierrePrice .* 1.25) "Joja" "Pierre" "Joja Membership"

  let createAll seedSellPrice = function
    | Pierre -> [ createSeed seedSellPrice "Pierre" ]
    | Joja pierrePrice -> [ pierrePrice; createJojaPrice pierrePrice ]
    | Oasis -> [ createSeed seedSellPrice "Oasis" ]
    | PierreAndJoja ->
      let pierrePrice = createSeed seedSellPrice "Pierre"
      [ pierrePrice
        createJojaPrice pierrePrice ]
    | PriceList list -> list
    | NoPrice -> List.empty