module App

open Fable.Core
open Fable.Core.JsInterop
open Elmish

importAll "../sass/main.sass"

//--Model--
open Types
open Crop

type Page =
  | Home
  | Help

type SidebarTab =
  | Skills
  | Crops
  | Fertilizers
  | Buy
  | Sell
  | Replant
  | Date
  | Settings
  static member List =
    [ Skills
      Crops
      Fertilizers
      Buy
      Sell
      Replant
      Date
      Settings ]

type Integer = int //for accessing int.Min, int.Max

type Model =
  { Page: Page
    StartDate: Date
    EndDate: Date
    Year: int
    SidebarTab: SidebarTab
    SidebarOpen: bool
    Skills: Map<Name<Skill>, Skill>
    SkillList: Name<Skill> list
    Multipliers: Map<Name<Multiplier>, Multiplier>
    IgnoreProfessions: bool
    BuySources: Map<Name<Source>, Source>
    BuySourceList: Name<Source> list //lists are necessary to enforce/preserve order, haven't thought of another way
    MatchConditions: Map<Name<MatchCondition>, MatchCondition>
    MatchConditionList: Name<MatchCondition> list
    SellSources: Map<Name<Processor>, Processor>
    SellSourceList: Name<Processor> list
    Replants: Map<Name<ReplantMethod>, ReplantMethod>
    ReplantList: Name<ReplantMethod> list
    BuySeeds: bool
    Crops: Map<Name<Crop>, Crop>
    CropList: Name<Crop> list
    Fertilizers: Map<Name<Fertilizer>, Fertilizer>
    FertilizerList: Name<Fertilizer> list
    YearConditionsDo: ConditionsDo
    SkillLevelConditionsDo: ConditionsDo
    SpecialCharm: bool
    LuckBuff: int
    BaseGiantCropChance: float
    GiantCropChecksPerTile: float
    GreenhouseMode: bool
    StartingFertilizer: Name<Fertilizer> option }
  member this.ConditionMet = function
    | SkillLevel (skill, level) -> this.SkillLevelConditionsDo = Ignore || this.Skills.[skill].Level >= level
    | Year y -> this.YearConditionsDo = Ignore || this.Year >= y
  member private this.StatusHelper condition conditionsDo =
    match conditionsDo with
    | Ignore -> Valid
    | Warn | Invalidate when (this.ConditionMet condition) -> Valid
    | Warn -> Warning
    | Invalidate -> Invalid
  member this.ConditionStatus = function
    | SkillLevel (skill, level) -> this.StatusHelper (SkillLevel (skill, level)) this.SkillLevelConditionsDo
    | Year y -> this.StatusHelper (Year y) this.YearConditionsDo
  member this.ConditionStatuses = List.map (fun c -> (c, this.ConditionStatus c))
  member this.ConditionStatusData conditions overallStatus =
    conditions
    |> List.filter (fun c -> this.ConditionStatus c = overallStatus)
    |> List.map InvalidCondition
  member this.OverallStatus conditionStatuses =
    if (List.exists (fun cs -> snd cs = Invalid) conditionStatuses) then
      Invalid
    elif (List.exists (fun cs -> snd cs = Warning) conditionStatuses) then
      Warning
    else Valid
  member this.SourceStatus source =
    if this.BuySources.[source].Selected then Valid else Invalid
  member this.SourceStatusData source =
    [ if not this.BuySources.[source].Selected then Reason "Is not selected." ]
  member this.ProcessorStatus source =
    if this.SellSources.[source].Selected then
      this.ConditionStatuses this.SellSources.[source].Conditions
      |> this.OverallStatus
    else Invalid
  member this.ProcessorStatusData source overallStatus =
    [ if not this.SellSources.[source].Selected then Reason "Is not selected." ]
    @ this.ConditionStatusData this.SellSources.[source].Conditions overallStatus
  member this.ReplantStatus replant =
    if this.Replants.[replant].Selected then
      this.ConditionStatuses this.Replants.[replant].Conditions
      |> this.OverallStatus
    else Invalid
  member this.ReplantStatusData replant overallStatus =
    [ if not this.Replants.[replant].Selected then Reason "Is not selected." ]
    @ this.ConditionStatusData this.Replants.[replant].Conditions overallStatus
  member this.PriceStatus (price: Price) =
    match price.Override with
    | Some true -> Valid //true //ignore source conditions but consider local conditions
    | Some false -> Invalid //false //false, return display: manually overriden to false
    | None ->
        if (this.SourceStatus price.Source = Valid) then
          this.ConditionStatuses price.Conditions
          |> this.OverallStatus
        else Invalid
   member this.PriceStatusData (priceStatus: Price * Status) =
    ( match (fst priceStatus).Override with
      | Some true -> []
      | Some false -> []
      | None -> this.SourceStatusData (fst priceStatus).Source)
    @ this.ConditionStatusData (fst priceStatus).Conditions (snd priceStatus)
  member this.PriceOf (priceFrom: Map<Name<Source>, Price>) price =
    match price with
    | Price p -> p.Value
    | MatchPrice m -> if this.MatchConditions.[m.MatchCondition].Selected then this.PriceOf priceFrom priceFrom.[m.MatchSource] else m.Value
  member this.BestPrices (priceFrom: Map<Name<Source>, Price>) =
    let priceStatuses = Map.map (fun source _ -> this.PriceStatus priceFrom.[source] ) priceFrom
    let validPrices = Map.filter (fun _ status -> status <> Invalid) priceStatuses
    ( if (validPrices.IsEmpty) then
        priceFrom
      else
        let bestPrice = Map.fold (fun currentMin source _ -> min (this.PriceOf priceFrom priceFrom.[source] ) currentMin) Integer.MaxValue validPrices
        Map.filter (fun _ price -> this.PriceOf priceFrom price = bestPrice) priceFrom)
    |> Map.fold (fun prices source price -> (price, priceStatuses.[source])::prices) []
  member this.DoubleCropProb = 0.0001 + float this.LuckBuff / 1500.0 + (if this.SpecialCharm then 0.025 else 0.0)
  member this.NoGiantCropProb = (1.0 - this.BaseGiantCropChance) ** this.GiantCropChecksPerTile
  member this.CropAmounts crop =
    let noGiantCropProb = if crop.IsGiantCrop then this.NoGiantCropProb else 1.0
    (noGiantCropProb,
     noGiantCropProb * (crop.ExtraCrops + (if crop.HasDoubleCropChance then crop.ExtraCrops * this.DoubleCropProb + this.DoubleCropProb else 0.0))
     + (1.0 - noGiantCropProb) * 2.0)

let init () =
  let skills =
    [ { Skill.Initial with
          Name = "Farming"
          Professions =
            [ { Profession.Initial with
                  Name = "Tiller"
                  UnlockLevel = 5
                  Dependants = set [ Name "Artisan"; Name "Agriculturist" ] }
              { Profession.Initial with
                  Name = "Artisan"
                  Requires = set [ Name "Tiller" ]
                  ExclusiveWith = set [ Name "Agriculturist" ] }
              { Profession.Initial with
                  Name = "Agriculturist"
                  Requires = set [ Name "Tiller" ]
                  ExclusiveWith = set [ Name "Artisan" ] } ]
            |> List.map (fun p -> (Name p.Name, p))
            |> Map.ofList
          ProfessionLayout =
            [ [ Name "Tiller" ]
              [ Name "Artisan"; Name "Agriculturist" ] ] }
      { Skill.Initial with
          Name = "Foraging"
          Professions =
            [ { Profession.Initial with
                  Name = "Gatherer"
                  UnlockLevel = 5
                  Dependants = set [ Name "Botanist" ] }
              { Profession.Initial with
                  Name = "Botanist"
                  Requires = set [ Name "Gatherer" ] } ]
            |> List.map (fun p -> (Name p.Name, p))
            |> Map.ofList
          ProfessionLayout = [ [ Name "Gatherer" ]; [ Name "Botanist" ] ] } ]

  let multipliers =
    [ Profession (Name "Tiller", 1.1)
      Profession (Name "Artisan", 1.4)
      Profession (Name "Agriculturist", 0.1)
      Profession (Name "Gatherer", 1.2)
      Multiplier ("Irrigated", 1.1, false) ]

  let buySources =
    [ { Source.Initial with Name = "Pierre" }
      { Source.Initial with Name = "Joja" }
      { Source.Initial with Name = "Oasis" }
      { Source.Initial with Name = "Traveling Merchant" }
      { Source.Initial with Name = "Crafting" } ]

  let matchConditions =
    [ { Name = "Joja Membership"; Selected = false } ]

  let sellSources =
    [ { Processor.Initial with
          Name = "Raw Crop"
          PreservesQuality = true }
      { Processor.Initial with
          Name = "Preserves Jar"
          Conditions = [ SkillLevel (Name "Farming", 4) ] }
      { Processor.Initial with
          Name = "Keg"
          Conditions = [ SkillLevel (Name "Farming", 8) ] }
      { Processor.Initial with
          Name = "Oil Maker"
          Conditions = [ SkillLevel (Name "Farming", 8) ] }
      { Processor.Initial with Name = "Mill" }
      { Processor.Initial with
          Name = "Seed Maker"
          Conditions = [ SkillLevel (Name "Farming", 9) ] } ]

  let replantList =
    [ { ReplantMethod.Initial with
          Name = "Seed Maker"
          Conditions = [ SkillLevel (Name "Farming", 9) ] }
      { ReplantMethod.Initial with Name = "Harvested Crop or Seed" } ]

  let oil =
    Process
      {| Processor = Name "Oil Maker"
         Output =
           { Name = "Oil"
             BasePrice = 100
             Multiplier = None }
         Override = None |}

  let item name price =
    { Name = name
      BasePrice = price
      Multiplier = None }

  let cropItem name price =
    { Name = name
      BasePrice = price
      Multiplier = Some (Name "Tiller") }

  let kegProduct name price =
    Process
      {| Processor = Name "Keg"
         Output =
           { Name = name
             BasePrice = price
             Multiplier = Some (Name "Artisan") }
         Override = None |}

  let price name value =
    Price
      {| Value = value
         Source = Name name
         Conditions = List.empty<Condition>
         Override = Option<bool>.None |}

  let crops =
    [ genCrop
        "Blue Jazz"
        [ Spring ]
        [ 1; 2; 2; 2 ]
        (SellPrice 50)
        (Seed (item "Jazz Seeds" 15))
        NoProduct
        PierreAndJoja
        true

      { genCrop
          "Cauliflower"
          [ Spring ]
          [ 1; 2; 4; 4; 1 ]
          (SellPrice 175)
          (SeedSell 40)
          Vegetable
          PierreAndJoja
          true
        with
          IsGiantCrop = true }

      { genCrop
          "Coffee"
          [ Spring; Summer ]
          [ 1; 2; 2; 3; 2 ]
          (Item (item "Coffee Bean" 15))
          CropItem
          (ProductList
            [ RatioProcess
                {| InputAmount = 5
                   Processor = Name "Keg"
                   Output = item "Coffee" 150
                   OutputAmount = 1.0
                   Override = None |} ] )
          (PriceList [ price "Traveling Merchant" 2500 ] )
          false
        with
          RegrowTime = Some 2
          ExtraCrops = extraCrops 4 0.02 }

      genCrop
        "Garlic"
        [ Spring ]
        [ 1; 1; 1; 1 ]
        (SellPrice 60)
        (SeedSell 20)
        Vegetable
        (PriceList
          [ Price
              {| Value = 40
                 Source = Name "Pierre"
                 Conditions = [ Year 2 ]
                 Override = None |} ] )
        true

      { genCrop
          "Green Bean"
          [ Spring ]
          [ 1; 1; 1; 3; 4 ]
          (SellPrice 40)
          (Seed (item "Bean Starter" 30))
          Vegetable
          PierreAndJoja
          true
        with
          RegrowTime = Some 3 }

      { genCrop
          "Kale"
          [ Spring ]
          [ 1; 2; 2; 1 ]
          (SellPrice 110)
          (SeedSell 35)
          Vegetable
          PierreAndJoja
          true
        with
          HasDoubleCropChance = false }

      genCrop
        "Parsnip"
        [ Spring ]
        [ 1; 1; 1; 1 ]
        (SellPrice 35)
        (SeedSell 10)
        Vegetable
        PierreAndJoja
        true

      { genCrop
          "Potato"
          [ Spring ]
          [ 1; 1; 1; 2; 1 ]
          (SellPrice 80)
          (SeedSell 25)
          Vegetable
          PierreAndJoja
          true
        with
          ExtraCrops = extraCrops 1 0.2 }

      genCrop
        "Rhubarb"
        [ Spring ]
        [ 2; 2; 2; 3; 4]
        (SellPrice 220)
        (SeedSell 50)
        Fruit
        Oasis
        true

      { genCrop
          "Strawberry"
          [ Spring ]
          [ 1; 1; 2; 2; 2 ]
          (SellPrice 120)
          (SeedSell 0)
          Fruit
          (PriceList [ price "Pierre" 100 ])
          true
        with
          RegrowTime = Some 4
          ExtraCrops = extraCrops 1 0.02 }

      genCrop
        "Tulip"
        [ Spring ]
        [ 1; 1; 2; 2 ]
        (Item (item "Tulip Bulb" 10))
        (SeedSell 30)
        NoProduct
        PierreAndJoja
        true

      { genCrop
          "Rice"
          [ Spring ]
          [ 1; 2; 2; 3 ]
          (Item (cropItem "Unmilled Rice" 30))
          (Seed (item "Rice Shoot" 20))
          (GenerateAndList
            ( Vegetable,
              [ Process
                  {| Processor = Name "Mill"
                     Output = item "Rice" 100
                     Override = None |} ] ))
          Pierre
          true
        with
          GrowthMultipliers = Name "Irrigated" ::agri
          HasDoubleCropChance = false
          ExtraCrops = extraCrops 1 0.1 }

      { genCrop
          "Blueberry"
          [ Summer ]
          [ 1; 3; 3; 4; 2 ]
          (SellPrice 50)
          (SeedSell 40)
          Fruit
          Pierre
          true 
        with
          RegrowTime = Some 4
          ExtraCrops = extraCrops 3 0.02 }

      { genCrop
          "Corn"
          [ Summer; Fall ]
          [ 2; 3; 3; 3; 3 ]
          (SellPrice 50)
          (SeedSell 75)
          (GenerateAndList (Vegetable, [ oil ] ))
          PierreAndJoja
          true
        with
          RegrowTime = Some 4 }

      { genCrop
          "Hops"
          [ Summer ]
          [ 1; 1; 2; 3; 4 ]
          (SellPrice 25)
          (Seed (item "Hops Starter" 30))
          (GenerateAndList (Jar Pickle, [ kegProduct "Pale Ale" 300 ] ))
          PierreAndJoja
          true
        with
          RegrowTime = Some 1 }

      { genCrop
          "Hot Pepper"
          [ Summer ]
          [ 1; 1; 1; 1; 1 ]
          (SellPrice 40)
          (Seed (item "Pepper Seeds" 20))
          Fruit
          PierreAndJoja
          true
        with
          RegrowTime = Some 3
          ExtraCrops = extraCrops 1 0.03 }

      { genCrop
          "Melon"
          [ Summer ]
          [ 1; 2; 3; 3; 3 ]
          (SellPrice 250)
          (SeedSell 40)
          Fruit
          PierreAndJoja
          true
        with
          IsGiantCrop = true }

      genCrop
        "Poppy"
        [ Summer ]
        [ 1; 2; 2; 2 ]
        (SellPrice 140)
        (SeedSell 50)
        NoProduct
        PierreAndJoja
        true

      genCrop
        "Radish"
        [ Summer ]
        [ 2; 1; 2; 1 ]
        (SellPrice 90)
        (SeedSell 20)
        Vegetable
        PierreAndJoja
        true

      genCrop
        "Red Cabbage"
        [ Summer ]
        [ 2; 1; 2; 2; 2 ]
        (SellPrice 260)
        (SeedSell 50)
        Vegetable
        (PriceList
          [ Price
              {| Value = 100
                 Source = Name "Pierre"
                 Conditions = [ Year 2 ]
                 Override = None |} ] )
        true

      genCrop
        "StarFruit"
        [ Summer ]
        [ 2; 3; 2; 3; 3 ]
        (SellPrice 750)
        (SeedSell 200)
        Fruit
        Oasis
        true

      genCrop
        "Summer Spangle"
        [ Summer ]
        [ 1; 2; 3; 2 ]
        (SellPrice 90)
        (Seed (item "Spangle Seeds" 25))
        NoProduct
        PierreAndJoja
        true

      { genCrop
          "Sunflower"
          [ Summer; Fall ]
          [ 1; 2; 3; 2 ]
          (SellPrice 80)
          (SeedSell 20)
          (ProductList [ oil ] )
          (PriceList
            [ price "Pierre" 200
              price "Joja" 125 ] )
          true
        with
          HasDoubleCropChance = false }

      { genCrop
          "Tomato"
          [ Summer ]
          [ 2; 2; 2; 2; 3 ]
          (SellPrice 60)
          (SeedSell 25)
          Fruit
          PierreAndJoja
          true
        with
          RegrowTime = Some 4
          ExtraCrops = extraCrops 1 0.05 }

      { genCrop
          "Wheat"
          [ Summer; Fall ]
          [ 1; 1; 1; 1 ]
          (SellPrice 25)
          (SeedSell 5)
          (GenerateAndList
            ( Jar Pickle,
              [ kegProduct "Beer" 200
                Process
                  {| Processor = Name "Mill"
                     Output = item "Wheat Flour" 50
                     Override = None |} ] ))
          PierreAndJoja
          true
        with
          HasDoubleCropChance = false }

      { genCrop
          "Amaranth"
          [ Fall ]
          [ 1; 2; 2; 2 ]
          (SellPrice 150)
          (SeedSell 35)
          Vegetable
          PierreAndJoja
          true
        with
          HasDoubleCropChance = false }

      genCrop
        "Artichoke"
        [ Fall ]
        [ 2; 2; 1; 2; 1 ]
        (SellPrice 160)
        (SeedSell 15)
        Vegetable
        (PriceList
          [ Price
              {| Value = 30
                 Source = Name "Pierre"
                 Conditions = [ Year 2 ]
                 Override = None |} ] )
        true

      genCrop
        "Beet"
        [ Fall ]
        [ 1; 1; 2; 2 ]
        (SellPrice 100)
        (SeedSell 10)
        (GenerateAndList
          ( Vegetable,
            [ RatioProcess
                {| InputAmount = 1
                   Processor = Name "Mill"
                   Output = item "Sugar" 50
                   OutputAmount = 3.0
                   Override = None |} ] ))
         Oasis
         true

      genCrop
        "Bok Choy"
        [ Fall ]
        [ 1; 1; 1; 1 ]
        (SellPrice 80)
        (SeedSell 25)
        Vegetable
        PierreAndJoja
        true

      { genCrop
          "Cranberries"
          [ Fall ]
          [ 1; 2; 1; 1; 2 ]
          (SellPrice 75)
          (SeedSell 60)
          Fruit
          (Joja (price "Pierre" 240))
          true
        with
          RegrowTime = Some 5
          ExtraCrops = extraCrops 2 0.1 }

      { genCrop
          "Eggplant"
          [ Fall ]
          [ 1; 1; 1; 1; 1 ]
          (SellPrice 60)
          (SeedSell 10)
          Vegetable
          PierreAndJoja
          true
        with
          RegrowTime = Some 5
          ExtraCrops = extraCrops 1 0.002 }

      genCrop
        "Fairy Rose"
        [ Fall ]
        [ 1; 4; 4; 3 ]
        (SellPrice 290)
        (Seed (item "Fairy Seeds" 100))
        NoProduct
        PierreAndJoja
        true

      { genCrop
          "Grape"
          [ Fall ]
          [ 1; 1; 2; 3; 3 ]
          (SellPrice 30)
          (Seed (item "Grape Starter" 30))
          Fruit
          PierreAndJoja
          true
        with
          RegrowTime = Some 3 }

      { genCrop
          "Pumpkin"
          [ Fall ]
          [ 1; 2; 3; 4; 3 ]
          (SellPrice 320)
          (SeedSell 50)
          Vegetable
          PierreAndJoja
          true
        with
          IsGiantCrop = true }

      genCrop
        "Sweet Gem Berry"
        [ Fall ]
        [ 2; 4; 6; 6; 6 ]
        (Item (item "Sweet Gem Berry" 3000))
        (Seed (item "Rare Seed" 200))
        NoProduct
        (PriceList [ price "Traveling Merchant" 1000 ])
        true

      genCrop
        "Yam"
        [ Fall ]
        [ 1; 3; 3; 3 ]
        (SellPrice 160)
        (SeedSell 30)
        Vegetable
        PierreAndJoja
        true

      { genCrop
          "Ancient Fruit"
          [ Spring; Summer; Fall ]
          [ 2; 7; 7; 7; 5 ]
          (SellPrice 550)
          (Seed (item "Ancient Seeds" 30))
          Fruit
          NoPrices
          true
        with
          RegrowTime = Some 7 }
    ]

  let fertilizers =
    [ ( { Fertilizer.Initial with
            Name = "Basic Fertilizer"
            Quality = 1 }
        |> Fertilizer.WithPrices
          [ Price
              {| Price.InitialPrice with
                   Source = Name "Pierre"
                   Value = 100 |} ] )

      ( { Fertilizer.Initial with
            Name = "Quality Fertilizer"
            Quality = 2 }
        |> Fertilizer.WithPrices
          [ Price
              {| Price.InitialPrice with
                   Value = 150
                   Source = Name "Pierre"
                   Conditions = [ Year 2 ] |} ] )

      ( { Fertilizer.Initial with
            Name = "Speed-Gro"
            Speed = 0.1 }
        |> Fertilizer.WithPrices
          [ Price
              {| Price.InitialPrice with
                   Value = 100
                   Source = Name "Pierre" |} ] )

      ( { Fertilizer.Initial with
            Name = "Deluxe Speed-Gro"
            Speed = 0.25 }
        |> Fertilizer.WithPrices
          [ Price
              {| Price.InitialPrice with
                   Value = 150
                   Source = Name "Pierre"
                   Conditions = [ Year 2 ] |}
            Price
              {| Price.InitialPrice with
                   Value = 80
                   Source = Name "Oasis" |} ] ) ]

  { Page = Home
    StartDate = { Season = Spring; Day = 1 }
    EndDate = { Season = Fall; Day = 28 }
    Year = 1
    SidebarTab = Skills
    SidebarOpen = false
    Skills =
      skills
      |> List.map (fun s -> (Name s.Name, s))
      |> Map.ofList
    SkillList = List.map (fun (s: Skill) -> Name s.Name) skills
    Multipliers =
      multipliers
      |>List.map (fun multi ->
          match multi with
          | Multiplier (name, _, _) -> Name name
          | Profession (name, _) -> Name name.Value
          , multi)
      |> Map.ofList
    IgnoreProfessions = false
    BuySourceList = List.map (fun (s: Source)-> Name s.Name) buySources
    BuySources =
      buySources
      |> List.map (fun s -> (Name s.Name, s))
      |> Map.ofList
    MatchConditions =
      matchConditions
      |> List.map (fun m -> (Name m.Name, m))
      |> Map.ofList
    MatchConditionList = List.map (fun (m: MatchCondition) -> Name m.Name) matchConditions
    SellSourceList = List.map (fun (s: Processor) -> Name s.Name) sellSources
    SellSources =
      sellSources
      |> List.map (fun s -> (Name s.Name, s))
      |> Map.ofList
    Replants =
      replantList
      |> List.map (fun r -> (Name r.Name, r))
      |> Map.ofList
    ReplantList = List.map (fun (r: ReplantMethod) -> Name r.Name) replantList
    BuySeeds = true
    Crops = 
      crops
      |> List.map (fun c -> (Name c.Name), c)
      |> Map.ofList
    CropList = List.map (fun (c: Crop) -> Name c.Name) crops
    Fertilizers =
      fertilizers
      |> List.map (fun f -> (Name f.Name, f))
      |> Map.ofList
    FertilizerList = List.map (fun (f: Fertilizer) -> Name f.Name) fertilizers
    YearConditionsDo = Warn
    SkillLevelConditionsDo = Warn
    SpecialCharm = false
    LuckBuff = 0
    BaseGiantCropChance = 0.01
    GiantCropChecksPerTile = 8.0
    GreenhouseMode = false
    StartingFertilizer = None }

let initialModel = init ()

//--Update--
open Browser

type Message =
  | SetPage of Page
  | SetSidebarTab of SidebarTab
  | CloseSidebar
  | ToggleIgnoreProfessions
  | SetSkillLevel of Name<Skill> * int
  | SetSkillBuff of Name<Skill> * int
  | ToggleProfession of Skill * Name<Profession>
  | ToggleBuySource of Name<Source>
  | ToggleSellSource of Name<Processor>
  | ToggleReplant of Name<ReplantMethod>
  | ToggleBuySeeds
  | ToggleCropSelected of Name<Crop>
  | ToggleFertSelected of Name<Fertilizer>
  | SetStartDay of int
  | SetStartSeason of Season
  | SetEndDay of int
  | SetEndSeason of Season
  | SetYear of int
  | SetYearConditionsDo of ConditionsDo
  | SetSkillLevelConditionsDo of ConditionsDo
  | ToggleSpecialCharm
  | SetLuckBuff of int
  | SetGiantCropChecksPerTile of float
  | ToggleGreenhouseMode
  | SetStartingFertilizer of Name<Fertilizer> option

let update message model =
  match message with
  | SetPage page -> { model with Page = page }
  | SetSidebarTab tab ->
      if (tab = model.SidebarTab) then
          { model with SidebarOpen = not model.SidebarOpen }
      else
          { model with SidebarTab = tab; SidebarOpen = true }
  | CloseSidebar -> { model with SidebarOpen = false }
  | ToggleIgnoreProfessions -> { model with IgnoreProfessions = not model.IgnoreProfessions }
  | SetSkillLevel (skill, level) ->
      { model with Skills = model.Skills.Add(skill, { model.Skills.[skill] with Level = level |> max 0 |> min 10 } ) }
  | SetSkillBuff (skill, buff) ->
      { model with Skills = model.Skills.Add(skill, { model.Skills.[skill] with Buff = max buff 0 } ) }
  | ToggleProfession (skill, name) ->
      { model with Skills = model.Skills.Add(Name skill.Name, model.Skills.[Name skill.Name].ToggleProfession name model.IgnoreProfessions) }
  | ToggleBuySource source -> { model with BuySources = model.BuySources.Add(source, model.BuySources.[source].Toggle) }
  | ToggleSellSource source -> { model with SellSources = model.SellSources.Add(source, model.SellSources.[source].Toggle) }
  | ToggleReplant replant -> { model with Replants = model.Replants.Add(replant, model.Replants.[replant].Toggle) }
  | ToggleBuySeeds -> { model with BuySeeds = not model.BuySeeds }
  | ToggleCropSelected crop -> { model with Crops = model.Crops.Add(crop, model.Crops.[crop].Toggle) }
  | ToggleFertSelected fert -> { model with Fertilizers = model.Fertilizers.Add(fert, model.Fertilizers.[fert].Toggle) }
  | SetStartDay day -> { model with StartDate = { model.StartDate with Day = day } }
  | SetStartSeason season -> { model with StartDate = { model.StartDate with Season = season } }
  | SetEndDay day -> { model with EndDate = { model.EndDate with Day = day } }
  | SetEndSeason season -> { model with EndDate = { model.EndDate with Season = season } }
  | SetYear year -> { model with Year = year }
  | SetYearConditionsDo something -> { model with YearConditionsDo = something }
  | SetSkillLevelConditionsDo something -> { model with SkillLevelConditionsDo = something }
  | ToggleSpecialCharm -> { model with SpecialCharm = not model.SpecialCharm }
  | SetLuckBuff buff -> { model with LuckBuff = max buff 0 }
  | SetGiantCropChecksPerTile checks -> { model with GiantCropChecksPerTile = checks |> max 0.0 |> min 9.0 }
  | ToggleGreenhouseMode -> { model with GreenhouseMode = not model.GreenhouseMode }
  | SetStartingFertilizer fert -> { model with StartingFertilizer = fert }

//--View--
open Fable.React
open Fable.React.Props
open Elmish.React.Common
open Elmish.React.Helpers

let classModifier baseClass modifier apply =
  ClassName (if apply then baseClass + "--" + modifier else baseClass)

let checkbox message isChecked dispatch =
  label [ ClassName "checkbox-img-label" ]
    [ input
        [ Type "checkbox"
          Style [ Visibility "hidden"; Position PositionOptions.Absolute ]
          Checked isChecked
          OnChange (fun _ -> dispatch message) ]
      img
        [ ClassName "checkbox-img"
          Src (if isChecked then "img/UI/CheckboxGreen.png" else "img/UI/Checkbox.png") ] ]

let checkboxWith alsoDisplay message isChecked dispatch =
  label [ ClassName "checkbox-img-label" ]
    ( [ input
          [ Type "checkbox"
            Style [ Visibility "hidden"; Position PositionOptions.Absolute ]
            Checked isChecked
            OnChange (fun _ -> dispatch message) ]
        img
          [ ClassName "checkbox-img"
            Src (if isChecked then "img/UI/CheckboxGreen.png" else "img/UI/Checkbox.png") ] ]
      @alsoDisplay )

let checkboxText text message isChecked dispatch =
  label [ ClassName "checkbox-img-label" ]
    [ input
        [ Type "checkbox"
          Style [ Visibility "hidden"; Position PositionOptions.Absolute ]
          Checked isChecked
          OnChange (fun _ -> dispatch message) ]
      img
        [ ClassName "checkbox-img"
          Src (if isChecked then "img/UI/CheckboxGreen.png" else "img/UI/Checkbox.png") ]
      str text ]

let checkboxImg isChecked status =
  if isChecked then
    match status with
    | Valid -> "img/UI/CheckboxGreen.png"
    | Warning -> "img/UI/CheckboxYellow.png"
    | Invalid -> "img/UI/CheckboxRed.png"
  else
    "img/UI/Checkbox.png"

let statusCheckbox displayAfter message isChecked status dispatch =
  label [ ClassName "checkbox-img-label" ]
    ( [ input
          [ Type "checkbox"
            Style [ Visibility "hidden"; Position PositionOptions.Absolute ]
            Checked isChecked
            OnChange (fun _ -> dispatch message) ]
        img
          [ ClassName "checkbox-img"
            Src (checkboxImg isChecked status) ] ]
      @ displayAfter)

let viewTab css message tab active dispatch =
  li
    [ classModifier (css + "-tab") "active" active
      OnClick (fun _ -> dispatch <| message tab) ]
    [ str (string tab) ]

let viewTabs css message list activeFun dispatch =
  ul [ ClassName (css + "-tabs") ]
    [ for tab in list do
        viewTab css message tab (activeFun tab) dispatch ]

//let lazySidebarContent =

let levelInput mode name level dispatch =
  input
    [ Type mode
      Min 0
      Max 10
      valueOrDefault level
      ClassName ("skill-" + mode + "-input")
      OnChange (fun l -> dispatch <| SetSkillLevel (name, !!l.Value)) ]

let skillLevelInput name level dispatch =
  label [ ClassName "skill-input" ]
    [ str "Level: "
      levelInput "range" name level dispatch
      levelInput "number" name level dispatch ]

let skillBuffInput (name: Name<Skill>) buff dispatch =
  label [ ClassName "skill-input" ]
    [ str "Buff: "
      input
        [ Type "number"
          Min 0
          valueOrDefault buff
          ClassName "skill-number-input"
          OnChange (fun b -> dispatch <| SetSkillBuff (name, !!b.Value)) ] ]

let profession conditionsDo name skill dispatch =
  button
    [ classModifier "profession" "active" skill.Professions.[name].Selected
      OnClick (fun _ -> dispatch <| ToggleProfession (skill, name)) ]
    [ img
        [ ClassName "profession-img"
          Src ("img/Skills/" + name.Value + ".png")]
      str name.Value
      if skill.Professions.[name].Selected && not (skill.ProfessionIsUnlocked name) then
        if conditionsDo = Warn then
          span [] [ str " warn" ]
        elif conditionsDo = Invalidate then
          span [] [ str " override"] ]

let professionRow conditionsDo skill row dispatch =
  div [ ClassName "profession-row" ]
    [ for name in row do
        profession conditionsDo name skill dispatch ]

let viewProfessions conditionsDo skill dispatch =
  div [ ClassName "professions" ]
    [ for row in skill.ProfessionLayout do
        professionRow conditionsDo skill row dispatch ]

let sourceIcon (name: Name<'t>) =
  [ img
      [ ClassName "source-img" 
        Src ("img/Sources/" + name.Value + ".png") ]
    str name.Value ]

let rec invalidReason reason =
  li []
    [ match reason with
      | InvalidCondition c ->
          match c with
          | SkillLevel (skill, level) -> str (skill.Value + " level too low. Unlocks at level " + string level + ".")
          | Year y -> str ("Available only from year " + string y + " and onwards.")
      | Reason reason -> str reason
      | SubReason (text, sub) -> str text; for subReason in sub do ul [] [ invalidReason subReason ] ]

let buySource (name: Name<Source>) selected dispatch =
  li []
    [ checkboxWith (sourceIcon name) (ToggleBuySource name) selected dispatch ]

let buySources list (sources: Map<Name<Source>, Source>) dispatch =
  ul [ClassName "source-list" ]
    [ for name in list do
        buySource name sources.[name].Selected dispatch ]

let sellSource (name: Name<Processor>) selected status dispatch =
  li []
    [ statusCheckbox (sourceIcon name) (ToggleSellSource name) selected status dispatch ]

let sellSources model dispatch =
  ul [ ClassName "source-list" ]
    [ for name in model.SellSourceList do
        let status = model.ProcessorStatus name
        sellSource name model.SellSources.[name].Selected status dispatch
        if model.SellSources.[name].Selected then
          if status = Warning then
            let id = System.Guid.NewGuid().ToString()
            label
              [ ClassName "details-label"
                HtmlFor id ]
              [ str "Show Warnings"]
            input
              [ Type "checkbox"
                ClassName "details-input"
                Id id ]
            ul [ ClassName "details" ]
              [ for reason in model.ProcessorStatusData name status do
                  invalidReason reason ] 
          elif status = Invalid then
            let id = System.Guid.NewGuid().ToString()
            label
              [ ClassName "details-label"
                HtmlFor id ]
              [ str "Show Errors"]
            input
              [ Type "checkbox"
                ClassName "details-input"
                Id id ]
            ul [ ClassName "details" ]
              [ for reason in model.ProcessorStatusData name status do
                  invalidReason reason ] ]

let replant (name: Name<ReplantMethod>) selected status dispatch =
  li []
    [ statusCheckbox (sourceIcon name) (ToggleReplant name) selected status dispatch ]

let replants model dispatch =
  ul [ ClassName "source-list" ]
    [ li [] [ checkboxWith (sourceIcon (Types.Name "Buy Seeds")) ToggleBuySeeds model.BuySeeds dispatch ]
      for name in model.ReplantList do
        let status = model.ReplantStatus name
        replant name model.Replants.[name].Selected status dispatch
        if status = Warning then
          let id = System.Guid.NewGuid().ToString()
          label
            [ ClassName "details-label"
              HtmlFor id ]
            [ str "Show Warnings"]
          input
            [ Type "checkbox"
              ClassName "details-input"
              Id id ]
          ul [ ClassName "details" ]
            [ for reason in model.ReplantStatusData name status do
                invalidReason reason ] 
        elif status = Invalid then
          let id = System.Guid.NewGuid().ToString()
          label
            [ ClassName "details-label"
              HtmlFor id ]
            [ str "Show Errors"]
          input
            [ Type "checkbox"
              ClassName "details-input"
              Id id ]
          ul [ ClassName "details" ]
            [ for reason in model.ReplantStatusData name status do
                invalidReason reason ] ]

let selectOptions (list: 't seq) toText (fromText: string -> 't) text message (value: 't) dispatch=
  label []
      [ ofOption (text |> Option.bind (fun t -> str (t + ": ") |> Some))
        select
          [ valueOrDefault value
            OnChange (fun x -> fromText x.Value |> message |> dispatch) ]
          [ for something in list do
              option [ Value something ]
                [ str (toText something) ] ] ]

let parseConditionsDo str =
  match str with
  | "Warn" -> Warn
  | "Invalidate" -> Invalidate
  | _ -> Ignore

let selectConditionsDo =
  selectOptions ConditionsDo.List string parseConditionsDo

let parseSeasons str =
  match str with
  | "Spring" -> Spring
  | "Summer" -> Summer
  | "Fall" -> Fall
  | _ -> Winter




let viewPrices (model: Model) priceFrom (priceStatuses: (Price * Status) list) =
  let status =
    if (List.exists (fun ps -> snd ps = Warning) priceStatuses) then
      Warning
    elif (List.exists (fun ps -> snd ps = Valid) priceStatuses) then
      Valid
    else
      Invalid
  match status with
  | Warning ->
      let id = System.Guid.NewGuid().ToString()
      [ ofInt (model.PriceOf priceFrom (fst priceStatuses.Head))
        for price in priceStatuses do
          img
            [ classModifier "price-img" "warning" (snd price = Warning)
              Src ("img/Sources/" + (fst price).Source.Value + ".png") ]
        br []
        img
          [ ClassName "alert"
            Src ("img/UI/Warning.png") ]
        label
          [ ClassName "details-label"
            HtmlFor id ]
          [ str "Show Warnings"]
        input
          [ Type "checkbox"
            ClassName "details-input"
            Id id ]
        ul [ ClassName "details" ]
            [ for priceStatus in priceStatuses do
                li []
                  ( sourceIcon (fst priceStatus).Source @
                    [ ul []
                        [ for reason in model.PriceStatusData priceStatus do
                            invalidReason reason ] ] ) ] ]
  | Valid ->
      [ ofInt (model.PriceOf priceFrom (fst priceStatuses.Head))
        for price in priceStatuses do
          img
            [ ClassName "price-img"
              Src ("img/Sources/" + (fst price).Source.Value + ".png") ] ]
  | Invalid ->
      let id = System.Guid.NewGuid().ToString()
      [ img
          [ ClassName "alert"
            Src ("img/UI/Error.png") ]
        label
          [ ClassName "details-label"
            HtmlFor id ]
          [ str "Show Errors"]
        input
          [ Type "checkbox"
            ClassName "details-input"
            Id id ]
        ul [ ClassName "details" ]
            [ for priceStatus in priceStatuses do
                li []
                  ( sourceIcon (fst priceStatus).Source @
                    [ ul []
                        [ for reason in model.PriceStatusData priceStatus do
                            invalidReason reason ] ] ) ] ]

let selectSeasons =
  selectOptions Season.List string parseSeasons None

let dayInput message day dispatch =
  label [ ClassName "day-input" ]
    [ input
        [ Type "number"
          Min 1
          Max 28
          valueOrDefault day
          ClassName "day-number-input"
          OnChange (fun b -> dispatch <| message !!b.Value) ] ]

let date text seasonMsg dayMsg date dispatch =
  div [ ClassName "date" ]
    [ str text
      selectSeasons seasonMsg date.Season dispatch
      dayInput dayMsg date.Day dispatch ]

let sidebarContent model dispatch =
  match model.SidebarTab with
  | Skills ->
      div [ classModifier "sidebar-content" "open" model.SidebarOpen ]
        [ ul [ ClassName "skills" ]
            [ for skill in model.SkillList do
                li [ ClassName "skill" ]
                  [ span [ ClassName "skill-header" ]
                      [ img
                          [ ClassName "skill-img"
                            Src ("/img/Skills/" + skill.Value + ".png")]
                        str model.Skills.[skill].Name ]
                    skillLevelInput skill model.Skills.[skill].Level dispatch
                    skillBuffInput skill model.Skills.[skill].Buff dispatch
                    viewProfessions model.SkillLevelConditionsDo model.Skills.[skill] dispatch ] ]
          checkboxText "Ignore Profession Relationships" ToggleIgnoreProfessions model.IgnoreProfessions dispatch ]
  | Crops ->
      div [ classModifier "sidebar-table-content" "open" model.SidebarOpen ]
        [ table [ ClassName "table" ]
            [ thead [ ClassName "table-header" ]
                [ tr []
                    [ th [] []
                      th [] [ str "Crop" ]
                      th [] [ str "Growth Time" ]
                      th [] [ str "Regrow Time" ]
                      th [] [ str "Seasons" ]
                      th [] [ str "Seed Price" ]
                      th [] [ str "ExtraCrops" ]
                      th [] [ str "CropAmounts" ] ] ]
              tbody [ ClassName "table-body" ]
                [ for c in model.CropList do
                    let crop = model.Crops.[c]
                    let priceStatuses = model.BestPrices crop.PriceFrom
                    let status =
                      if (List.exists (fun ps -> snd ps = Valid) priceStatuses) then
                        Valid
                      elif (List.exists (fun ps -> snd ps = Warning) priceStatuses) then
                        Warning
                      else
                        Invalid
                    tr []
                      [ td [] [ statusCheckbox [] (ToggleCropSelected c) crop.Selected status dispatch ] 
                        td []
                          [ img
                              [ ClassName "crop-img"
                                Src ("/img/Crops/" + crop.Name + ".png") ]
                            str crop.Name ]
                        td [] [ ofInt crop.TotalGrowthTime ]
                        td [] [ ofOption (Option.bind (ofInt >> Some) crop.RegrowTime) ]
                        td [] [ for season in crop.Seasons do str (string season); br [] ]
                        td [] (viewPrices model crop.PriceFrom priceStatuses)
                        td [] [ str (sprintf "%.4f" crop.ExtraCrops) ]
                        td [] [ str (model.CropAmounts crop |> (fun (q, n) -> sprintf "%.4f" q + ", " + sprintf "%.4f" n)) ] ] ] ] ]
  | Fertilizers ->
      div [ classModifier "sidebar-table-content" "open" model.SidebarOpen ]
        [ table [ ClassName "table" ]
            [ thead [ ClassName "table-header" ]
                [ tr []
                    [ th [] []
                      th [] [ str "Fertilizer" ]
                      th [] [ str "Quality" ]
                      th [] [ str "Speed" ]
                      th [] [ str "Price" ] ] ]
              tbody [ ClassName "table-body" ]
                [ for f in model.FertilizerList do
                    let fert = model.Fertilizers.[f]
                    let priceStatuses = model.BestPrices fert.PriceFrom
                    let status =
                      if (List.exists (fun ps -> snd ps = Valid) priceStatuses) then
                        Valid
                      elif (List.exists (fun ps -> snd ps = Warning) priceStatuses) then
                        Warning
                      else
                        Invalid
                    tr []
                      [ td [] [ statusCheckbox [] (ToggleFertSelected f) fert.Selected status dispatch ] 
                        td []
                          [ img
                              [ ClassName "fertilizer-img"
                                Src ("/img/Fertilizers/" + fert.Name + ".png") ]
                            str fert.Name ]
                        td [] [ ofInt fert.Quality ]
                        td [] [ ofFloat fert.Speed ]
                        td [] (viewPrices model fert.PriceFrom priceStatuses) ] ] ] ]
  | Buy ->
      div [ classModifier "sidebar-content" "open" model.SidebarOpen ]
        [ buySources model.BuySourceList model.BuySources dispatch ]
  | Sell ->
      div [ classModifier "sidebar-content" "open" model.SidebarOpen ]
        [ sellSources model dispatch ]
  | Replant ->
      div [ classModifier "sidebar-content" "open" model.SidebarOpen ]
        [ replants model dispatch ]
  | Date ->
      div [ classModifier "sidebar-content" "open" model.SidebarOpen ]
        [ date "Start Date: " SetStartSeason SetStartDay model.StartDate dispatch
          date "End Date: " SetEndSeason SetEndDay model.EndDate dispatch
          label [ ClassName "year-input" ]
            [ str "Year: "
              input
                [ Type "number"
                  Min 1
                  valueOrDefault model.Year
                  ClassName "year-number-input"
                  OnChange (fun y -> dispatch <| SetYear !!y.Value) ] ] ]
  | Settings ->
      div [ classModifier "sidebar-content" "open" model.SidebarOpen ]
        [ selectConditionsDo (Some "Year Conditions") SetYearConditionsDo model.YearConditionsDo dispatch
          selectConditionsDo (Some "Skill Level Conditions") SetSkillLevelConditionsDo model.SkillLevelConditionsDo dispatch
          checkboxText "Special Charm" ToggleSpecialCharm model.SpecialCharm dispatch
          label [ ClassName "setting-input" ]
            [ str "Luck Buff: "
              input
                [ Type "number"
                  Min 0
                  valueOrDefault model.LuckBuff
                  ClassName "setting-number-input"
                  OnChange (fun b -> dispatch <| SetLuckBuff !!b.Value) ] ]
          label [ ClassName "setting-input" ]
            [ str "Giant Crop Checks Per Tile: "
              input
                [ Type "number"
                  Min 0
                  Max 9
                  valueOrDefault model.GiantCropChecksPerTile
                  ClassName "setting-number-input"
                  OnChange (fun c -> dispatch <| SetGiantCropChecksPerTile !!c.Value) ] ]
          checkboxText "Greenhouse Mode" ToggleGreenhouseMode model.GreenhouseMode dispatch 
          label []
            [ str "Starting Fertilizer: "
              select
                [ valueOrDefault
                    ( match model.StartingFertilizer with
                      | Some fert -> fert.Value
                      | None -> "None")
                  OnChange (fun fert ->
                    ( match fert.Value with
                      | "None" -> None
                      | f -> Some (Types.Name f))
                    |> SetStartingFertilizer |> dispatch) ]
                [ option [ Value "None" ]
                    [ str "None" ]
                  for fert in model.FertilizerList do
                    option [ Value fert.Value ]
                      [ str fert.Value ] ] ] ]
  //lazyView2With (fun oldModel newModel -> (not oldModel.SidebarOpen && not newModel.SidebarOpen) || oldModel = newModel) sidebarContent

let cover sidebarOpen dispatch =
  div
    [ classModifier "cover" "open" sidebarOpen
      OnClick (fun _ -> dispatch CloseSidebar) ]
    []

let sidebar model dispatch =
  div [ classModifier "sidebar" "open" model.SidebarOpen ]
    [ //lazySidebarContent model dispatch
      sidebarContent model dispatch
      viewTabs "sidebar" SetSidebarTab SidebarTab.List (fun t -> model.SidebarOpen && t = model.SidebarTab) dispatch ]

let view model dispatch =
  match model.Page with
  | Help ->
      span [] [ str "Help! I need somebody!" ]
  | Home ->
      div []
        [ div [ Class "calender-grid" ]
            []
          button []
            [ str "Help" ]
          cover model.SidebarOpen dispatch
          sidebar model dispatch ]

//--App--
open Elmish.React
open Elmish.Debug
open Elmish.HMR

Program.mkSimple (fun _ -> initialModel) update view
#if DEBUG
|> Program.withDebugger
#endif
|> Program.withReactBatched "elmish-app"
|> Program.run