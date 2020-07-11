module Types

type ID<'t> = ID of int
type Name<'t> =
  | Name of string
  member this.Value =
    match this with Name name -> name
  static member ValueOf name =
    match name with Name str -> str

type Season =
  | Spring
  | Summer
  | Fall
  | Winter

type Date = 
  { Year: int
    StartSeason: Season
    StartDay: int 
    EndSeason: Season
    EndDay: int }
  member this.Valid =
    this.StartSeason < this.EndSeason || 
    this.StartSeason = this.EndSeason && this.StartDay < this.EndDay
  static member initial =
    { Year = 1
      StartSeason = Spring
      StartDay = 1
      EndSeason = Fall
      EndDay = 28 }

type Profession =
  { Name: string
    Selected: bool
    UnlockLevel: int
    Requires: Set<Name<Profession>>
    ExclusiveWith: Set<Name<Profession>>
    Dependants: Set<Name<Profession>> }
  member this.Toggle = { this with Selected = not this.Selected}
  static member initial =
    { Name = "initial"
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
  member this.ProfessionUnlocked profession =
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
    if (set.IsEmpty) then
      professions
    else
      professions |> Map.map (fun name profession ->
        if set.Contains name then
          { profession with Selected = value }
        else
          profession) //better way to do this?
  static member initial =
    { Name = "initial"
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
  | SkillLevel of {| Skill: Name<Skill>; Level: int |}
  | Year of int

type InvalidReason =
  | InvalidCondition of Condition
  | Reason of string
  | SubReason of (string * InvalidReason list)

type StatusData =
  | Valid
  | Warning of InvalidReason
  | Invalid of InvalidReason

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
  static member initial =
    { Name = "initial"
      Selected = true }

type Processor = 
  { Name: string
    Selected: bool
    Conditions: Condition list
    PreservesQuality: bool }
  member this.Toggle = { this with Selected = not this.Selected }
  static member initial =
    { Name = "initial"
      Selected = true
      Conditions = List.empty
      PreservesQuality = false }

type Price =
  { Value: int
    Source: Name<Source>
    Override: bool option
    Conditions: Condition list }
  static member initial =
    { Value = -1
      Source = Name "initial"
      Override = None
      Conditions = List.empty }

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
  | Multiplier of {| Name: string; Value: float; Selected: bool |}
  | Profession of {| Profession: Name<Profession>; Value: float |}

let applyMultiplier value multiplier =
  int (float value * multiplier)

type Item = 
  { Name: string
    BasePrice: int
    Multiplier: Multiplier option }
  member this.Price = 
      match this.Multiplier with
      | Some multi ->
          match multi with
          | Multiplier m -> applyMultiplier this.BasePrice m.Value //when m.Selectable.Active
          | Profession p -> applyMultiplier this.BasePrice p.Value //when p.Profession.Selected 
          //| _ -> this.BasePrice
      | None -> this.BasePrice

type Output =
  | Output of Item
  | ProcessFun of (Item -> Item)

type Process =
  | UseItem of {| Item: Item; Processor: Processor |}
  | Process of {| Input: Item; Processor: Processor; Output: Item |}
  | RatioProcess of {| Input: Item; InputAmount: int; Processor: Processor; Output: Item; OutputAmount: float |}
  | QualityProcess of {| Input: Item; InputAmount: int; Processor: Processor; Output: Item; OutputAmount: float[] |}
  member this.Input =
    match this with
    | UseItem i -> i.Item
    | Process p -> p.Input
    | RatioProcess r -> r.Input
    | QualityProcess q -> q.Input
  member this.InputAmount =
    match this with
    | UseItem _ | Process _ -> 1
    | RatioProcess r -> r.InputAmount
    | QualityProcess q -> q.InputAmount
  member this.Processor =
    match this with
    | UseItem i -> i.Processor
    | Process p -> p.Processor
    | RatioProcess r -> r.Processor
    | QualityProcess q -> q.Processor
  member this.Output =
    match this with
    | UseItem i -> i.Item
    | Process p -> p.Output
    | RatioProcess r -> r.Output
    | QualityProcess q -> q.Output
  member this.OutputAmount quality =
    match this with
    | UseItem _ | Process _ -> 1.0
    | RatioProcess r -> r.OutputAmount
    | QualityProcess q -> q.OutputAmount.[quality]

type Fertilizer =
  { Name: string
    Selected: bool
    Quality: int
    Speed: float
    PriceFrom: Map<Name<Source>, Price>
    Sources: Name<Source> list }
  member this.Toggle = { this with Selected = not this.Selected }
  static member initial =
    { Name = "initial"
      Selected = true
      Quality = 0
      Speed = 0.0
      PriceFrom = Map.empty
      Sources = List.empty }
  static member WithPrices prices =
    { Fertilizer.initial with 
        Sources = List.map (fun price -> price.Source) prices
        PriceFrom = 
          prices
          |> List.map (fun price -> price.Source, price)
          |> Map.ofList }
  //member this.ActivePrices = List.filter (fun p -> p.Selectable.Active) this.Prices
  //member this.Price = (List.minBy (fun p -> p.Value ) this.ActivePrices).Value
  //member this.BestPrices = List.filter (fun p -> p.Value = this.Price) this.ActivePrices

//type Fertilizer =
//  { Name: string
//    Quality: int
//    Speed: float
//    Price: int
//    Sources: Source list }

//type Replant =
//  | Process of Process
//  | BuySeeds of {| Value: int; Sources: Source list |}

 type Crop = 
  { Name: string
    Seasons: Set<Season>
    GrowthStages: int array
    RegrowTime: int option
    GrowthSpeed: Multiplier list 
    CropItem: Item
    PriceFrom: Map<Source, Price>
    HarvestedItems: Item list
    HarvestedAmounts: Map<Item, float>
    Processes: Map<Item, Process list>
    Replants: Map<Item, Process list>
    }