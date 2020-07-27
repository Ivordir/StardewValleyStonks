namespace StardewValleyStonks

open Types

type Source =
  { Name: string
    Selected: bool }
  member this.Toggle = { this with Selected = not this.Selected }

module Source =
  let create name : Source =
    { Name = name
      Selected = true }

type MatchCondition =
  { Name: string
    Selected: bool }
  member this.Toggle = { this with Selected = not this.Selected }

type Requirement =
  | SkillLevel of Skill: Name<Skill> * Level: int
  | Year of int

type InvalidReason =
  | UnmetRequirement of Requirement
  | Reason of string
  | SubReason of Reason: string * SubReasons: InvalidReason list

type Price =
  | Price of
      {| Value: int
         Source: Name<Source>
         Requirements: Requirement list
         OverrideSource: bool option |}
  | MatchPrice of
      {| Value: int
         Source: Name<Source>
         Requirements: Requirement list
         OverrideSource: bool option
         MatchSource: Name<Source>
         MatchCondition: Name<MatchCondition> |}

module Price =
  let value = function
    | Price p -> p.Value
    | MatchPrice m -> m.Value

  let source = function
    | Price p -> p.Source
    | MatchPrice m -> m.Source
  
  let overrideSource = function
    | Price p -> p.OverrideSource
    | MatchPrice m -> m.OverrideSource

  let requirements = function
    | Price p -> p.Requirements
    | MatchPrice m -> m.Requirements

  let key price = source price

  let create name value =
    Price
      {| Value = value
         Source = Name name
         Requirements = List.empty
         OverrideSource = None |}