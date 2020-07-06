module Types

type ID<'t> = ID of int
type Name<'t> =
    | Name of string
    member this.Value =
        match this with Name name -> name
    static member ValueOf name =
        match name with Name str -> str
    override this.ToString() = this.Value

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

type Status =
    | Valid
    | Warning
    | Invalid

type ConditionsDo =
    | Warn
    | Override
    | Nothing

type Condition =
    | SkillLevel of {| Skill: Name<Skill>; Level: int |}
    | Year of int
    //static member isMet condition =
    //    match condition with
    //    | SkillLevel s -> s.Skill.Level >= s.Level
    //    | Year y -> y.Date.Year >= y.Year

and Selectable = 
    { Selected: bool 
      Conditions: Condition list }
    member this.Toggle = { this with Selected = not this.Selected }
    static member initial =
        { Selected = false
          Conditions = [] }
    //member this.Active = this.Selected && List.forall Condition.isMet this.Conditions

and Profession =
    { Name: string
      Selectable: Selectable
      Requires: Set<Name<Profession>>
      ExclusiveWith: Set<Name<Profession>>
      Dependants: Set<Name<Profession>> }
    member this.Toggle =
        { this with 
            Selectable = 
                { this.Selectable with
                    Selected = not this.Selectable.Selected } }
    static member initial =
        { Name = "initial"
          Selectable = Selectable.initial
          Requires = Set.empty
          ExclusiveWith = Set.empty
          Dependants = Set.empty }

and Skill =
    { Name: string 
      Level: int
      Buff: int
      Professions: Map<Name<Profession>, Profession>
      ProfessionLayout: Name<Profession> list list }
    member this.BuffedLevel = this.Level + this.Buff
    member this.ToggleProfession name ignoreConflicts =
        let professions = this.Professions.Add(name, this.Professions.[name].Toggle)
        if ignoreConflicts then
            { this with Professions = professions }
        else
            let profession = professions.[name]
            { this with
                Professions = 
                    match profession.Selectable.Selected with
                    | true -> 
                        professions
                        |> Skill.SetSelected true profession.Requires 
                        |> Skill.SetSelected false profession.ExclusiveWith
                    | false -> professions |> Skill.SetSelected false profession.Dependants }
    static member private SetSelected value set professions =
        if (set.IsEmpty) then
            professions
        else
            professions |> Map.map (fun k v ->
                if set.Contains k then
                    { v with Selectable = { v.Selectable with Selected = value } }
                else v) //better way to do this?
    static member initial =
        { Name = "initial"
          Level = 0
          Buff = 0
          Professions = Map.empty
          ProfessionLayout = List.empty }

type Comparison =
    | Better
    | Worse
    | Same
    | Incomparable

type Source =
    { Name: string
      Selectable: Selectable }
    member this.Toggle = { this with Selectable = this.Selectable.Toggle }
    static member Selected source = source.Selectable.Selected

type Processor = 
    { Name: string
      PreservesQuality: bool
      Selectable: Selectable }
    member this.Toggle = { this with Selectable = this.Selectable.Toggle }
    static member Selected processor = processor.Selectable.Selected

type Price =
    { Value: int
      Source: Source
      Override: bool option
      Conditions: Condition list }

type Multiplier =
    | Multiplier of {| Name: string; Value: float; Selectable: Selectable |}
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

type FertilizerDIO =
    { Name: string
      Selected: bool
      Quality: int
      Speed: float
      Prices: Price list
      PriceFrom: Map<Source, Price> }
    //member this.ActivePrices = List.filter (fun p -> p.Selectable.Active) this.Prices
    //member this.Price = (List.minBy (fun p -> p.Value ) this.ActivePrices).Value
    //member this.BestPrices = List.filter (fun p -> p.Value = this.Price) this.ActivePrices

type Fertilizer =
    { Name: string
      Quality: int
      Speed: float
      Price: int
      Sources: Source list }

//type Replant =
//    | Process of Process
//    | BuySeeds of {| Value: int; Sources: Source list |}

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