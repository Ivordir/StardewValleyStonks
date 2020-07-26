module StardewValleyStonks.Types

type Name<'t> =
  | Name of string
  member this.Value =
    match this with Name name -> name

type Season =
  | Spring
  | Summer
  | Fall
  | Winter
  static member List =
    [ Spring
      Summer
      Fall
      Winter ]

let seasonToInt = function
  | Spring -> 1
  | Summer -> 2
  | Fall -> 3
  | Winter -> 4

let season int =
  match int with
  | 1 -> Spring
  | 2 -> Summer
  | 3 -> Fall
  | 4 -> Winter
  | _ -> invalidArg "int" (sprintf "There are only four seasons dummy. '%i' does not correspond to a season." int)

type Date =
  { Season: Season
    Day: int }
  member this.IsBefore date =
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

let professionListToMap professions =
  professions
  |> List.map (fun p -> Name p.Name, p)
  |> Map.ofList

let private setSelected value (set: Set<Name<Profession>>) professions =
  professions
  |> Map.map
    (fun name profession ->
      if set.Contains name then
        { profession with Selected = value }
      else
        profession)

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
            if profession.Selected then
              professions
              |> setSelected true profession.Requires
              |> setSelected false profession.ExclusiveWith
            else
              professions |> setSelected false profession.Dependants }
  static member Initial =
    { Name = "Initial"
      Level = 0
      Buff = 0
      Professions = Map.empty
      ProfessionLayout = List.empty }

type Requirement =
  | SkillLevel of Skill: Name<Skill> * Level: int
  | Year of int

let seedMakerRequirement = [ SkillLevel (Name "Farming", 9) ]

type RequirementsShould =
  | Warn
  | Invalidate
  | Ignore
  static member List =
    [ Warn
      Invalidate
      Ignore ]

type InvalidReason =
  | UnmetRequirement of Requirement
  | Reason of string
  | SubReason of Reason: string * SubReasons: InvalidReason list

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
         Requirements: Requirement list
         Override: bool option |}
  | MatchPrice of
      {| Value: int
         Source: Name<Source>
         Requirements: Requirement list
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
  member this.Requirements =
    match this with
    | Price p -> p.Requirements
    | MatchPrice m -> m.Requirements

let priceListToMap prices =
  prices
  |> List.map (fun (price: Price) -> price.Source, price)
  |> Map.ofList

type Processor =
  { Name: string
    Selected: bool
    Requirements: Requirement list
    PreservesQuality: bool }
  member this.Toggle = { this with Selected = not this.Selected }
  member this.TogglePreservesQuality = { this with PreservesQuality = not this.PreservesQuality }
  static member Initial =
    { Name = "Initial"
      Selected = true
      Requirements = List.empty
      PreservesQuality = false }

type Quality =
  | Normal
  | Silver
  | Gold
  | Iridium
  member this.Multiplier =
    match this with
    | Normal -> 1.0
    | Silver -> 1.25
    | Gold -> 1.5
    | Iridium -> 2.0
  static member List =
    [ Normal
      Silver
      Gold
      Iridium ]

type Multiplier =
  | Multiplier of
      {| Name: string
         Value: float
         Selected: bool |}
  | Profession of
      {| Skill: Name<Skill>
         Profession: Name<Profession>
         Value: float |}
  member this.Name =
    match this with
    | Multiplier m -> m.Name
    | Profession p -> p.Profession.Value

let agri: Name<Multiplier> list = [ Name "Agriculturist" ]

type Item =
  { Name: string
    BasePrice: int
    Multiplier: Name<Multiplier> option }

type ProductSource =
  | RawCrop
  | Processor of Name<Processor>
  | SeedMaker
  member this.Name =
    match this with
    | RawCrop -> "Raw Crop"
    | Processor p -> p.Value
    | SeedMaker -> "Seed Maker"

type Product =
  | RawItem of Override: bool option
  | Process of
      {| Processor: Name<Processor>
         Output: Item
         Override: bool option |}
  | RatioProcess of
      {| InputAmount: int
         Processor: Name<Processor>
         Output: Item
         OutputAmount: float
         Override: bool option |}
  | SeedsFromSeedMaker of Override: bool option
  member this.Source =
    match this with
    | RawItem -> RawCrop
    | Process p -> Processor p.Processor
    | RatioProcess r -> Processor r.Processor
    | SeedsFromSeedMaker -> SeedMaker
  member this.InputAmount =
    match this with
    | RatioProcess r -> r.InputAmount
    | _ -> 1
  member this.OutputAmount =
    match this with
    | RatioProcess r -> r.OutputAmount
    | _ -> 1.0