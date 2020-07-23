module Types

type Name<'t> =
  | Name of string
  member this.Value =
    match this with Name name -> name

let valueOf name =
  match name with Name str -> str

type Season =
  | Spring
  | Summer
  | Fall
  | Winter
  member this.ToInt =
    match this with
    | Spring -> 1
    | Summer -> 2
    | Fall -> 3
    | Winter -> 4
  static member List =
    [ Spring
      Summer
      Fall
      Winter ]

let season int =
  match int with
  | 1 -> Spring
  | 2 -> Summer
  | 3 -> Fall
  | _ -> Winter

type Date =
  { Season: Season
    Day: int }
  member this.IsBeforeOrOn date =
    this.Season < date.Season
    || this.Season = date.Season && this.Day < date.Day

type Profession =
  { Name: string
    Selected: bool
    UnlockLevel: int
    Requires: Set<Name<Profession>>
    ExclusiveWith: Set<Name<Profession>>
    Dependants: Set<Name<Profession>> }
  member this.Toggle = { this with Selected = not this.Selected }
  static member Initial =
    { Name = "Initial"
      Selected = false
      UnlockLevel = 10
      Requires = Set.empty
      ExclusiveWith = Set.empty
      Dependants = Set.empty }

type Skill =
  { Name: string
    Level: int
    Buff: int
    Professions: Map<Name<Profession>, Profession>
    ProfessionLayout: Name<Profession> list list }
  member this.BuffedLevel = this.Level + this.Buff
  member this.ProfessionIsUnlocked profession =
    this.Professions.[profession].UnlockLevel <= this.Level
  member this.ToggleProfession name ignoreConflicts =
    let professions = this.Professions.Add(name, this.Professions.[name].Toggle)
    if ignoreConflicts then
      { this with Professions = professions }
    else
      let profession = professions.[name]
      { this with
          Professions =
            match profession.Selected with
            | true ->
              professions
              |> Skill.SetSelected true profession.Requires
              |> Skill.SetSelected false profession.ExclusiveWith
            | false -> professions |> Skill.SetSelected false profession.Dependants }
  static member private SetSelected value set professions =
    professions |> Map.map
      (fun name profession ->
        if set.Contains name then
          { profession with Selected = value }
        else
          profession) //better way to do this?
  static member Initial =
    { Name = "Initial"
      Level = 0
      Buff = 0
      Professions = Map.empty
      ProfessionLayout = List.empty }

type ConditionsDo =
  | Warn
  | Invalidate
  | Ignore
  static member List =
    [ Warn
      Invalidate
      Ignore ]

type Condition =
  | SkillLevel of Skill: Name<Skill> * Level: int
  | Year of int

type InvalidReason =
  | InvalidCondition of Condition
  | Reason of string
  | SubReason of Reason: string * SubReason: InvalidReason list

type Status =
  | Valid
  | Warning
  | Invalid

type Comparison =
  | Better
  | Worse
  | Same
  | Incomparable

type Source =
  { Name: string
    Selected: bool }
  member this.Toggle = { this with Selected = not this.Selected }

let source name =
  { Name = name
    Selected = true }

type MatchCondition =
  { Name: string
    Selected: bool }
  member this.Toggle = { this with Selected = not this.Selected }

type Price =
  | Price of
      {| Value: int
         Source: Name<Source>
         Conditions: Condition list
         Override: bool option |}
  | MatchPrice of
      {| Value: int
         Source: Name<Source>
         Conditions: Condition list
         Override: bool option
         MatchSource: Name<Source>
         MatchCondition: Name<MatchCondition> |}
  member this.Value =
    match this with
    | Price p -> p.Value
    | MatchPrice m -> m.Value
  member this.Source =
    match this with
    | Price p -> p.Source
    | MatchPrice m -> m.Source
  member this.Override =
    match this with
    | Price p -> p.Override
    | MatchPrice m -> m.Override
  member this.Conditions =
    match this with
    | Price p -> p.Conditions
    | MatchPrice m -> m.Conditions

let priceListToMap prices =
  prices
  |> List.map (fun (price: Price) -> price.Source, price)
  |> Map.ofList

type Fertilizer =
  { Name: string
    Selected: bool
    Quality: int
    Speed: float
    PriceFrom: Map<Name<Source>, Price> }
  member this.Toggle = { this with Selected = not this.Selected }

let genFertilizer
  name
  quality
  speed
  prices =
  { Name = name
    Selected = true
    Quality = quality
    Speed = speed
    PriceFrom = priceListToMap prices }