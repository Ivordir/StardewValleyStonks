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
  static member List =
    [ Spring
      Summer
      Fall
      Winter ]

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
  | SubReason of (string * InvalidReason list)

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
  static member Initial =
    { Name = "Initial"
      Selected = true }

type Processor =
  { Name: string
    Selected: bool
    Conditions: Condition list
    PreservesQuality: bool }
  member this.Toggle = { this with Selected = not this.Selected }
  static member Initial =
    { Name = "Initial"
      Selected = true
      Conditions = List.empty
      PreservesQuality = false }

type ReplantMethod =
  { Name: string
    Selected: bool
    Conditions: Condition list }
  member this.Toggle = { this with Selected = not this.Selected }
  static member Initial =
    { Name = "Initial"
      Selected = true
      Conditions = List.empty }

type MatchCondition =
  { Name: string
    Selected: bool }

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
  static member InitialPrice =
    {| Value = -1
       Source = Name "Initial"
       Conditions = List.empty<Condition>
       Override = Option<bool>.None |}
  static member IntitialMatchPrice =
    {| Value = -1
       Source = Name "Initial"
       Conditions = List.empty<Condition>
       Override = Option<bool>.None
       MatchSource = Name "Initial"
       MatchCondition = Name "Initial" |}

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

type Multiplier =
  | Multiplier of Name: string * Value: float * Selected: bool
  | Profession of Name: Name<Profession> * Value: float

let applyMultiplier value multiplier =
  int (float value * multiplier)

type Item =
  { Name: string
    BasePrice: int
    Multiplier: Name<Multiplier> option }
  //member this.Price =
  //    match this.Multiplier with
  //    | Some multi ->
  //        match multi with
  //        | Multiplier m -> applyMultiplier this.BasePrice m.Value //when m.Selectable.Active
  //        | Profession p -> applyMultiplier this.BasePrice p.Value //when p.Profession.Selected 
  //        //| _ -> this.BasePrice
  //    | None -> this.BasePrice
  static member Initial =
    { Name = "Initial"
      BasePrice = -1
      Multiplier = None }

type Process =
  | UseItem of
      {| Processor: Name<Processor>
         Override: bool option |}
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
  | SeedMaker of Override: bool option
  member this.InputAmount =
    match this with
    | UseItem _ | Process _ | SeedMaker _ -> 1
    | RatioProcess r -> r.InputAmount
   member this.Processor =
    match this with
    | UseItem i -> i.Processor
    | Process p -> p.Processor
    | RatioProcess r -> r.Processor
    | SeedMaker _ -> Name "Seed Maker"
  // member this.Output =
  //   match this with
  //   | UseItem _ -> Item.Initial
  //   | Process p -> p.Output
  //   | RatioProcess r -> r.Output
  // member this.OutputAmount =
  //   match this with
  //   | UseItem _ | Process _ -> 1.0
  //   | RatioProcess r -> r.OutputAmount

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
  static member Initial =
    { Name = "Initial"
      Selected = true
      Quality = 0
      Speed = 0.0
      PriceFrom = Map.empty }
  static member WithPrices prices fertilizer =
    { fertilizer with PriceFrom = priceListToMap prices }