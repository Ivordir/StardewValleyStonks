namespace StardewValleyStonks

open Types

type Source =
  { Name: string
    Selected: bool }
  member this.Toggle = { this with Selected = not this.Selected }

module Source =
  let name source = source.Name

  let nameOf = toNameOf name

  let create name : Source =
    { Name = name
      Selected = true }

type MatchCondition =
  { Name: string
    Selected: bool }
  member this.Toggle = { this with Selected = not this.Selected }

module MatchCondition =
  let name matchCondition = matchCondition.Name

  let nameOf = toNameOf name

type Requirement =
  | SkillLevel of Skill: NameOf<Skill> * Level: int
  | Year of int

type InvalidReason =
  | UnmetRequirement of Requirement
  | Reason of string
  | SubReason of Reason: string * SubReasons: InvalidReason list

type Price =
  | Price of
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
    | Price p -> p.Value
    | MatchPrice m -> m.Value

  let source = function
    | Price p -> p.Source
    | MatchPrice m -> m.Source
  
  let overrideSource = function
    | Price p -> p.SourceOverride
    | MatchPrice m -> m.SourceOverride

  let requirements = function
    | Price p -> p.Requirements
    | MatchPrice m -> m.Requirements

  let nameOf = source

  let create name value =
    Price
      {| Value = value
         Source = Name name
         Requirements = List.empty
         SourceOverride = None |}