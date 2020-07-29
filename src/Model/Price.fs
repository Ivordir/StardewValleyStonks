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

type Requirement =
  | SkillLevel of Skill: NameOf<Skill> * Level: int
  | Year of int

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

  let create name value =
    BuyPrice
      {| Value = value
         Source = Name name
         Requirements = List.empty
         SourceOverride = None |}