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
  | Mod
  static member List =
    [ Skills
      Crops
      Fertilizers
      Buy
      Sell
      Replant
      Date
      Settings
      Mod ]

type Integer = int

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
    Processors: Map<Name<Processor>, Processor>
    ProcessorList: Name<Processor> list
    SellRawItem: bool
    SellSeedsFromSeedMaker: bool
    ProductSources: ProductSource list
    Replants: Map<Replant, bool>
    Crops: Map<Name<Crop>, Crop>
    CropList: Name<Crop> list
    ShowUselessCrops: bool
    Fertilizers: Map<Name<Fertilizer>, Fertilizer>
    FertilizerList: Name<Fertilizer> list
    YearRequirementsShould: RequirementsShould
    SkillLevelRequirementsShould: RequirementsShould
    SpecialCharm: bool
    LuckBuff: int
    BaseGiantCropChance: float
    GiantCropChecksPerTile: float
    SeedMakerProb: float
    AncientSeedProb: float
    GreenhouseMode: bool
    StartingFertilizer: Name<Fertilizer> option
    QualityProducts: bool
    QualitySeedMaker: bool
    QualitySeedMakerAmounts: Map<Quality, int> }
  member this.ValidDate = this.StartDate.IsBefore this.EndDate
  member this.RequirementMet = function
    | SkillLevel (skill, level) -> this.SkillLevelRequirementsShould = Ignore || this.Skills.[skill].Level >= level
    | Year y -> this.YearRequirementsShould = Ignore || this.Year >= y
  member private this.StatusHelper requirement requirementsShould =
    match requirementsShould with
    | Ignore -> Valid
    | Warn | Invalidate when (this.RequirementMet requirement) -> Valid
    | Warn -> Warning
    | Invalidate -> Invalid
  member this.RequirementStatus = function
    | SkillLevel (skill, level) -> this.StatusHelper (SkillLevel (skill, level)) this.SkillLevelRequirementsShould
    | Year y -> this.StatusHelper (Year y) this.YearRequirementsShould
  member this.RequirementStatuses = List.map (fun c -> c, this.RequirementStatus c)
  member this.RequirementStatusData requirements overallStatus =
    requirements
    |> List.filter (fun c -> this.RequirementStatus c = overallStatus)
    |> List.map UnmetRequirement
  member this.OverallStatus requirementStatuses =
    if (List.exists (fun cs -> snd cs = Invalid) requirementStatuses) then
      Invalid
    elif (List.exists (fun cs -> snd cs = Warning) requirementStatuses) then
      Warning
    else Valid
  member this.SourceStatus source =
    if this.BuySources.[source].Selected then Valid else Invalid
  member this.SourceStatusData source =
    [ if not this.BuySources.[source].Selected then Reason "Is not selected." ]
  member this.ProductSourceSelected source =
    match source with
    | RawCrop -> this.SellRawItem
    | Processor p -> this.Processors.[p].Selected
    | ProductSource.SeedMaker -> this.SellSeedsFromSeedMaker
  member this.ProductSourceStatus source=
    match source with
    | RawCrop -> if this.SellRawItem then Valid else Invalid
    | Processor p ->
        if this.Processors.[p].Selected then
          this.RequirementStatuses this.Processors.[p].Requirements
          |> this.OverallStatus
        else Invalid
    | ProductSource.SeedMaker ->
        if this.SellSeedsFromSeedMaker then
          this.RequirementStatuses seedMakerRequirement
          |> this.OverallStatus
        else
          Invalid
  member this.ProductSourceStatusData source overallStatus =
    match source with
    | RawCrop -> [ if not this.SellRawItem then Reason "Is not selected." ]
    | Processor p ->
        [ if not this.Processors.[p].Selected then Reason "Is not selected." ]
        @ this.RequirementStatusData this.Processors.[p].Requirements overallStatus
    | ProductSource.SeedMaker ->
        [ if not this.SellSeedsFromSeedMaker then Reason "Is not selected." ]
        @ this.RequirementStatusData seedMakerRequirement overallStatus
  member this.ReplantStatus replant =
    match replant with
    | SeedMaker ->
        if this.Replants.[SeedMaker] then
          this.RequirementStatuses seedMakerRequirement
          |> this.OverallStatus
        else
          Invalid
    | r -> if this.Replants.[r] then Valid else Invalid
  member this.ReplantStatusData replant overallStatus =
    match replant with
    | SeedMaker ->
        [ if not this.Replants.[SeedMaker] then Reason "Is not selected." ]
        @ this.RequirementStatusData seedMakerRequirement overallStatus
    | r -> [ if not this.Replants.[r] then Reason "Is not selected." ]
  member this.PriceStatus (price: Price) =
    match price.Override with
    | Some true -> Valid //true //ignore source conditions but consider local conditions
    | Some false -> Invalid //false //false, return display: manually overriden to false
    | None ->
        if (this.SourceStatus price.Source = Valid) then
          this.RequirementStatuses price.Requirements
          |> this.OverallStatus
        else Invalid
   member this.PriceStatusData (priceStatus: Price * Status) =
    ( match (fst priceStatus).Override with
      | Some true -> []
      | Some false -> []
      | None -> this.SourceStatusData (fst priceStatus).Source)
    @ this.RequirementStatusData (fst priceStatus).Requirements (snd priceStatus)
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
  member this.GrowthMultiplierValue name =
    match this.Multipliers.[name] with
    | Multiplier m -> if m.Selected then m.Value else 0.0
    | Profession p ->
        let skill = this.Skills.[p.Skill]
        if skill.Professions.[p.Profession].Selected && (skill.ProfessionIsUnlocked p.Profession || this.SkillLevelRequirementsShould <> Invalidate) then
          p.Value
        else
          0.0
  member this.GrowthTime fertSpeed (crop: Crop) =
    crop.GrowthTimeWith (fertSpeed + List.sumBy this.GrowthMultiplierValue crop.GrowthMultipliers)
  member this.FastestFertSpeed =
    this.FertilizerList
    |> List.filter (fun fert -> this.Fertilizers.[fert].Selected && List.exists (fun (_, s) -> s <> Invalid) (this.BestPrices this.Fertilizers.[fert].PriceFrom))
    |> (fun list -> if list.IsEmpty then 0.0 else this.Fertilizers.[ (List.maxBy (fun fert -> this.Fertilizers.[fert].Speed) list) ].Speed)
  member this.DaysInSeason (season: Season) =
    if season >= this.StartDate.Season && season <= this.EndDate.Season then
      if this.StartDate.Season = this.EndDate.Season then
        this.EndDate.Day - this.StartDate.Day + 1
      elif season = this.StartDate.Season then
        29 - this.StartDate.Day
      elif season = this.EndDate.Season then
        this.EndDate.Day
      else
        28
    else
      0
  member this.CropCanGiveOneHarvest name =
    let crop = this.Crops.[name]
    let seasons =
      [ for s = seasonToInt this.StartDate.Season to seasonToInt this.EndDate.Season do
          season s ]
    if List.exists crop.Seasons.Contains seasons then
      let lastConsecDays, maxDays =
        seasons
        |> List.fold
          (fun (consecDays, maxDays) season ->
            if crop.Seasons.Contains season then
              (consecDays + this.DaysInSeason season, maxDays)
            else
              (0, max consecDays maxDays))
          (0, 0)
      this.GrowthTime this.FastestFertSpeed this.Crops.[name] <= (max lastConsecDays maxDays) - 1
    else
      false

let init () =
  let skills =
    [ { Skill.Initial with
          Name = "Farming"
          Professions =
            professionListToMap
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
          ProfessionLayout =
            [ [ Name "Tiller" ]
              [ Name "Artisan"; Name "Agriculturist" ] ] }
      { Skill.Initial with
          Name = "Foraging"
          Professions =
            professionListToMap
              [ { Profession.Initial with
                    Name = "Gatherer"
                    UnlockLevel = 5
                    Dependants = set [ Name "Botanist" ] }
                { Profession.Initial with
                    Name = "Botanist"
                    Requires = set [ Name "Gatherer" ] } ]
          ProfessionLayout = [ [ Name "Gatherer" ]; [ Name "Botanist" ] ] } ]

  let multipliers =
    [ Profession
        {| Skill = Name "Farming"
           Profession = Name "Tiller"
           Value = 1.1 |}
      Profession
        {| Skill = Name "Farming"
           Profession = Name "Artisan"
           Value = 1.4 |}
      Profession
        {| Skill = Name "Farming"
           Profession = Name "Agriculturist"
           Value = 0.1 |}
      Profession
        {| Skill = Name "Foraging"
           Profession = Name "Gatherer"
           Value = 1.2 |}
      Multiplier
        {| Name = "Irrigated"
           Selected = false
           Value = 1.1 |} ]

  let buySources =
    [ "Pierre"
      "Joja"
      "Oasis"
      "Traveling Merchant"
      "Crafting" ]
    |> List.map source

  let matchConditions = [ { Name = "Joja Membership"; Selected = false } ]

  let processors =
    [ { Processor.Initial with
          Name = "Preserves Jar"
          Requirements = [ SkillLevel (Name "Farming", 4) ] }
      { Processor.Initial with
          Name = "Keg"
          Requirements = [ SkillLevel (Name "Farming", 8) ] }
      { Processor.Initial with
          Name = "Oil Maker"
          Requirements = [ SkillLevel (Name "Farming", 8) ] }
      { Processor.Initial with Name = "Mill" } ]

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
         Requirements = List.empty<Requirement>
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

      { genCrop
          "Cauliflower"
          [ Spring ]
          [ 1; 2; 4; 4; 1 ]
          (SellPrice 175)
          (SeedSell 40)
          Vegetable
          PierreAndJoja
        with
          IsGiantCrop = true }

      { genCropWithoutSeedMaker
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
                 Requirements = [ Year 2 ]
                 Override = None |} ] )

      { genCrop
          "Green Bean"
          [ Spring ]
          [ 1; 1; 1; 3; 4 ]
          (SellPrice 40)
          (Seed (item "Bean Starter" 30))
          Vegetable
          PierreAndJoja
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

      { genCrop
          "Potato"
          [ Spring ]
          [ 1; 1; 1; 2; 1 ]
          (SellPrice 80)
          (SeedSell 25)
          Vegetable
          PierreAndJoja
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

      { genCrop
          "Strawberry"
          [ Spring ]
          [ 1; 1; 2; 2; 2 ]
          (SellPrice 120)
          (SeedSell 0)
          Fruit
          (PriceList [ price "Pierre" 100 ])
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
        with
          GrowthMultipliers = Name "Irrigated"::agri
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

      genCrop
        "Radish"
        [ Summer ]
        [ 2; 1; 2; 1 ]
        (SellPrice 90)
        (SeedSell 20)
        Vegetable
        PierreAndJoja

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
                 Requirements = [ Year 2 ]
                 Override = None |} ] )

      genCrop
        "StarFruit"
        [ Summer ]
        [ 2; 3; 2; 3; 3 ]
        (SellPrice 750)
        (SeedSell 200)
        Fruit
        Oasis

      genCrop
        "Summer Spangle"
        [ Summer ]
        [ 1; 2; 3; 2 ]
        (SellPrice 90)
        (Seed (item "Spangle Seeds" 25))
        NoProduct
        PierreAndJoja

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
        with
          HasDoubleCropChance = false
          HarvestedItems =
            Map.ofList
              [ ( Name "Sunflower Seeds",
                  { Item = item "Sunflower Seeds" 20
                    Amount = 1.0
                    Products =
                      Map.ofList
                        [ RawCrop, RawItem None
                          Processor (Name "Mill"), oil ]
                    Replants = Map.ofList [ SeedOrCrop, None ] } ) ] }

      { genCrop
          "Tomato"
          [ Summer ]
          [ 2; 2; 2; 2; 3 ]
          (SellPrice 60)
          (SeedSell 25)
          Fruit
          PierreAndJoja
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
                 Requirements = [ Year 2 ]
                 Override = None |} ] )

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

      genCrop
        "Bok Choy"
        [ Fall ]
        [ 1; 1; 1; 1 ]
        (SellPrice 80)
        (SeedSell 25)
        Vegetable
        PierreAndJoja

      { genCrop
          "Cranberries"
          [ Fall ]
          [ 1; 2; 1; 1; 2 ]
          (SellPrice 75)
          (SeedSell 60)
          Fruit
          (Joja (price "Pierre" 240))
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

      { genCrop
          "Grape"
          [ Fall ]
          [ 1; 1; 2; 3; 3 ]
          (SellPrice 30)
          (Seed (item "Grape Starter" 30))
          Fruit
          PierreAndJoja
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

      genCrop
        "Yam"
        [ Fall ]
        [ 1; 3; 3; 3 ]
        (SellPrice 160)
        (SeedSell 30)
        Vegetable
        PierreAndJoja

      { genCrop
          "Ancient Fruit"
          [ Spring; Summer; Fall ]
          [ 2; 7; 7; 7; 5 ]
          (SellPrice 550)
          (Seed (item "Ancient Seeds" 30))
          Fruit
          NoPrices
        with
          RegrowTime = Some 7 }
    ]

  let fertilizers =
    [ genFertilizer
        "Basic Fertilizer"
        1
        0.0
        [ price "Pierre" 100 ]

      genFertilizer
        "Quality Fertilizer"
        2
        0.0
        [ Price
            {| Value = 150
               Source = Name "Pierre"
               Requirements = [ Year 2 ]
               Override = None |} ]

      genFertilizer
        "Speed-Gro"
        0
        0.1
        [ price "Pierre" 100 ]

      genFertilizer
        "Deluxe Speed-Gro"
        0
        0.25
        [ Price
            {| Value = 150
               Source = Name "Pierre"
               Requirements = [ Year 2 ]
               Override = None |}
          price "Oasis" 80 ] ]

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
      |> List.map (fun multi -> Name multi.Name, multi)
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
    ProductSources =
      [ RawCrop
        yield! List.map (fun (p: Processor) -> Processor (Name p.Name)) processors
        ProductSource.SeedMaker ]
    Processors =
      processors
      |> List.map (fun s -> (Name s.Name, s))
      |> Map.ofList
    ProcessorList = List.map (fun (p: Processor) -> Name p.Name) processors
    SellRawItem = true
    SellSeedsFromSeedMaker = true
    Replants =
      Replant.List
      |> List.map (fun r -> r, true)
      |> Map.ofList
    Crops =
      crops
      |> List.map (fun c -> (Name c.Name), c)
      |> Map.ofList
    CropList = List.map (fun (c: Crop) -> Name c.Name) crops
    ShowUselessCrops = false
    Fertilizers =
      fertilizers
      |> List.map (fun f -> (Name f.Name, f))
      |> Map.ofList
    FertilizerList = List.map (fun (f: Fertilizer) -> Name f.Name) fertilizers
    YearRequirementsShould = Warn
    SkillLevelRequirementsShould = Warn
    SpecialCharm = false
    LuckBuff = 0
    BaseGiantCropChance = 0.01
    GiantCropChecksPerTile = 8.0
    SeedMakerProb = 0.975
    AncientSeedProb = 0.005
    GreenhouseMode = false
    StartingFertilizer = None
    QualityProducts = false
    QualitySeedMaker = false
    QualitySeedMakerAmounts =
      Map.ofList
        [ Normal, 2
          Silver, 3
          Gold, 4
          Iridium, 5 ] }

let initialModel = init ()

//--Update--
open Browser

type Message =
  | SetPage of Page
  | SetSidebarTab of SidebarTab
  | CloseSidebar
  | ToggleIgnoreProfessions
  | SetSkillLevel of Name<Skill> * Level: int
  | SetSkillBuff of Name<Skill> * Level: int
  | ToggleProfession of Skill * Name<Profession>
  | ToggleBuySource of Name<Source>
  | ToggleMatchCondition of Name<MatchCondition>
  | ToggleProductSource of ProductSource
  | ToggleReplant of Replant
  | ToggleCropSelected of Name<Crop>
  | ToggleFertSelected of Name<Fertilizer>
  | ToggleShowUselessCrops
  | SetStartDay of int
  | SetStartSeason of Season
  | SetEndDay of int
  | SetEndSeason of Season
  | SetYear of int
  | SetYearRequirementsShould of RequirementsShould
  | SetSkillLevelRequirementsShould of RequirementsShould
  | ToggleSpecialCharm
  | SetLuckBuff of int
  | SetGiantCropChecksPerTile of float
  | ToggleGreenhouseMode
  | SetStartingFertilizer of Name<Fertilizer> option
  | ToggleQualityProducts
  | TogglePreservesQuality of Name<Processor>
  | ToggleQualitySeedMaker
  | SetQualitySeedMakerAmount of Quality * Amount: int

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
  | ToggleMatchCondition cond -> { model with MatchConditions = model.MatchConditions.Add(cond, model.MatchConditions.[cond].Toggle) }
  | ToggleProductSource source ->
      match source with
      | RawCrop -> { model with SellRawItem = not model.SellRawItem }
      | Processor p ->
        { model with Processors = model.Processors.Add(p, model.Processors.[p].Toggle) }
      | ProductSource.SeedMaker -> { model with SellSeedsFromSeedMaker = not model.SellSeedsFromSeedMaker }
  | ToggleReplant replant -> { model with Replants = model.Replants.Add(replant, not model.Replants.[replant]) }
  | ToggleCropSelected crop -> { model with Crops = model.Crops.Add(crop, model.Crops.[crop].Toggle) }
  | ToggleFertSelected fert -> { model with Fertilizers = model.Fertilizers.Add(fert, model.Fertilizers.[fert].Toggle) }
  | ToggleShowUselessCrops -> { model with ShowUselessCrops = not model.ShowUselessCrops }
  | SetStartDay day -> { model with StartDate = { model.StartDate with Day = day } }
  | SetStartSeason season -> { model with StartDate = { model.StartDate with Season = season } }
  | SetEndDay day -> { model with EndDate = { model.EndDate with Day = day } }
  | SetEndSeason season -> { model with EndDate = { model.EndDate with Season = season } }
  | SetYear year -> { model with Year = year }
  | SetYearRequirementsShould something -> { model with YearRequirementsShould = something }
  | SetSkillLevelRequirementsShould something -> { model with SkillLevelRequirementsShould = something }
  | ToggleSpecialCharm -> { model with SpecialCharm = not model.SpecialCharm }
  | SetLuckBuff buff -> { model with LuckBuff = max buff 0 }
  | SetGiantCropChecksPerTile checks -> { model with GiantCropChecksPerTile = checks |> max 0.0 |> min 9.0 }
  | ToggleGreenhouseMode -> { model with GreenhouseMode = not model.GreenhouseMode }
  | SetStartingFertilizer fert -> { model with StartingFertilizer = fert }
  | ToggleQualityProducts -> { model with QualityProducts = not model.QualityProducts }
  | TogglePreservesQuality processor -> { model with Processors = model.Processors.Add(processor, model.Processors.[processor].TogglePreservesQuality) }
  | ToggleQualitySeedMaker -> { model with QualitySeedMaker = not model.QualitySeedMaker }
  | SetQualitySeedMakerAmount (quality, amount) ->
      { model with QualitySeedMakerAmounts = model.QualitySeedMakerAmounts.Add(quality, amount) }

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
      @ alsoDisplay )

let checkboxWithText text message isChecked dispatch =
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
    | Warning -> "img/UI/CheckboxGreen.png"
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

let viewTabsWith activeFun css message list dispatch =
  ul [ ClassName (css + "-tabs") ]
    [ for tab in list do
        viewTab css message tab (activeFun tab) dispatch ]

let viewTabs css message list currentTab dispatch =
  viewTabsWith (fun tab -> tab = currentTab) css message list dispatch

let warningIcon =
  img
    [ ClassName "alert"
      Src ("img/UI/Warning.png") ]

let errorIcon =
  img
    [ ClassName "alert"
      Src ("img/UI/Error.png") ]

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

let profession requirementsShould name skill dispatch =
  button
    [ ClassName
        ( "profession" +
          if skill.Professions.[name].Selected then
            if skill.ProfessionIsUnlocked name || requirementsShould <> Invalidate then
              "--active"
            else
              "--error"
          else
            "")
      OnClick (fun _ -> dispatch <| ToggleProfession (skill, name)) ]
    [ if skill.Professions.[name].Selected && not (skill.ProfessionIsUnlocked name) then
        if requirementsShould = Warn then
          warningIcon
        elif requirementsShould = Invalidate then
          errorIcon
      img
        [ ClassName "profession-img"
          Src ("img/Skills/" + name.Value + ".png") ]
      str name.Value ]

let professionRow requirementsShould skill row dispatch =
  div [ ClassName "profession-row" ]
    [ for name in row do
        profession requirementsShould name skill dispatch ]

let viewProfessions requirementsShould skill dispatch =
  div [ ClassName "professions" ]
    [ for row in skill.ProfessionLayout do
        professionRow requirementsShould skill row dispatch ]

let sourceIcon name =
  [ img
      [ ClassName "source-img"
        Src ("img/Sources/" + name + ".png") ]
    str name ]

let rec invalidReason reason =
  li []
    [ match reason with
      | UnmetRequirement c ->
          match c with
          | SkillLevel (skill, level) -> str (skill.Value + " level too low. Unlocks at level " + string level + ".")
          | Year y -> str ("Available only from year " + string y + " and onwards.")
      | Reason reason -> str reason
      | SubReason (text, sub) -> str text; for subReason in sub do ul [] [ invalidReason subReason ] ]

let buySource (name: Name<Source>) selected dispatch =
  li []
    [ checkboxWith (sourceIcon name.Value) (ToggleBuySource name) selected dispatch ]

let buySources list (sources: Map<Name<Source>, Source>) dispatch =
  ul [ClassName "source-list" ]
    [ for name in list do
        buySource name sources.[name].Selected dispatch ]

let productSource (source: ProductSource) selected status dispatch =
  li []
    [ statusCheckbox (sourceIcon source.Name) (ToggleProductSource source) selected status dispatch ]

let productSources model dispatch =
  ul [ ClassName "source-list" ]
    [ for source in model.ProductSources do
        let status = model.ProductSourceStatus source
        let selected = model.ProductSourceSelected source
        productSource source selected status dispatch
        if selected then
          if status = Warning then
            label
              [ ClassName "details-label"
                HtmlFor source.Name ]
              [ str "Show Warnings"]
            input
              [ Type "checkbox"
                ClassName "details-input"
                Id source.Name ]
            ul [ ClassName "details" ]
              [ for reason in model.ProductSourceStatusData source status do
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
              [ for reason in model.ProductSourceStatusData source status do
                  invalidReason reason ] ]

let replant (replant: Replant) selected status dispatch =
  li []
    [ statusCheckbox (sourceIcon replant.Name) (ToggleReplant replant) selected status dispatch ]

let replants (model: Model) dispatch =
  ul [ ClassName "source-list" ]
    [ for r in Replant.List do
        let status = model.ReplantStatus r
        replant r model.Replants.[r] status dispatch
        if r = SeedMaker && status = Warning then
          label
            [ ClassName "details-label"
              HtmlFor r.Name ]
            [ str "Show Warnings"]
          input
            [ Type "checkbox"
              ClassName "details-input"
              Id r.Name ]
          ul [ ClassName "details" ]
            [ for reason in model.ReplantStatusData r status do
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

let parseRequirementsShould str =
  match str with
  | "Warn" -> Warn
  | "Invalidate" -> Invalidate
  | "Ignore" -> Ignore
  | _ -> invalidArg "str" ("'" + str + "' does not correspond to a RequirementsShould.")

let selectRequirementsShould =
  selectOptions RequirementsShould.List string parseRequirementsShould

let parseSeasons str =
  match str with
  | "Spring" -> Spring
  | "Summer" -> Summer
  | "Fall" -> Fall
  | "Winter" -> Winter
  | _ -> invalidArg "str" ("'" + str + "' is not the name of a Season.")

let viewPrices (model: Model) (name: Name<'t>) priceFrom (priceStatuses: (Price * Status) list) =
  let status =
    if (List.exists (fun ps -> snd ps = Warning) priceStatuses) then
      Warning
    elif (List.exists (fun ps -> snd ps = Valid) priceStatuses) then
      Valid
    else
      Invalid
  //extract warnings and errors to crop/fert level from the current price level
  match status with
  | Warning ->
      [ ofInt (model.PriceOf priceFrom (fst priceStatuses.Head))
        for price in priceStatuses do
          img
            [ classModifier "price-img" "warning" (snd price = Warning)
              Src ("img/Sources/" + (fst price).Source.Value + ".png") ]
        br []
        warningIcon
        label
          [ ClassName "details-label"
            HtmlFor name.Value ]
          [ str "Show Warnings"]
        input
          [ Type "checkbox"
            ClassName "details-input"
            Id name.Value ]
        ul [ ClassName "details" ]
            [ for priceStatus in priceStatuses do
                li []
                  ( sourceIcon (fst priceStatus).Source.Value @
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
      [ errorIcon
        label
          [ ClassName "details-label"
            HtmlFor name.Value ]
          [ str "Show Errors"]
        input
          [ Type "checkbox"
            ClassName "details-input"
            Id name.Value ]
        ul [ ClassName "details" ]
            [ for priceStatus in priceStatuses do
                li []
                  ( sourceIcon (fst priceStatus).Source.Value @
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
                    viewProfessions model.SkillLevelRequirementsShould model.Skills.[skill] dispatch ] ]
          checkboxWithText "Ignore Profession Relationships" ToggleIgnoreProfessions model.IgnoreProfessions dispatch ]
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
                      th [] [ str "Seed Price" ] ] ]
              tbody [ ClassName "table-body" ]
                [ for c in model.CropList do
                    if model.ShowUselessCrops || model.CropCanGiveOneHarvest c then
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
                          td []
                            ( if crop.Replants.ContainsKey BuySeeds then
                                viewPrices model c crop.PriceFrom priceStatuses
                              else
                                [ str "N/A" ] ) ] ] ]
          checkboxWithText "Show Crops That Cannot Give One Harvest" ToggleShowUselessCrops model.ShowUselessCrops dispatch ]
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
                        td [] (viewPrices model f fert.PriceFrom priceStatuses) ] ] ]
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
  | Buy ->
      div [ classModifier "sidebar-content" "open" model.SidebarOpen ]
        [ buySources model.BuySourceList model.BuySources dispatch
          ul [ ClassName "match-condition-list" ]
            [ for cond in model.MatchConditionList do
              checkboxWithText cond.Value (ToggleMatchCondition cond) model.MatchConditions.[cond].Selected dispatch ] ]
  | Sell ->
      div [ classModifier "sidebar-content" "open" model.SidebarOpen ]
        [ productSources model dispatch ]
  | Replant ->
      div [ classModifier "sidebar-content" "open" model.SidebarOpen ]
        [ replants model dispatch ]
  | Date ->
      div [ classModifier "sidebar-content" "open" model.SidebarOpen ]
        [ date "Start Date: " SetStartSeason SetStartDay model.StartDate dispatch
          date "End Date: " SetEndSeason SetEndDay model.EndDate dispatch
          if not model.ValidDate then
            span []
              [ errorIcon
                if model.StartDate = model.EndDate then
                  str "The end date cannot be the same as the start date."
                else
                  str "The end date cannot be before the start date." ]
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
        [ selectRequirementsShould (Some "Year Requirements") SetYearRequirementsShould model.YearRequirementsShould dispatch
          selectRequirementsShould (Some "Skill Level Requirements") SetSkillLevelRequirementsShould model.SkillLevelRequirementsShould dispatch
          checkboxWithText "Special Charm" ToggleSpecialCharm model.SpecialCharm dispatch
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
          checkboxWithText "Greenhouse Mode" ToggleGreenhouseMode model.GreenhouseMode dispatch ]
  | Mod ->
      div [ classModifier "sidebar-content" "open" model.SidebarOpen ]
        [ checkboxWithText "Quality Products" ToggleQualityProducts model.QualityProducts dispatch
          ul []
            [ for processor in model.ProcessorList do
                li []
                  [ //visual indicator for disabled
                    label
                      [ ClassName "checkbox-img-label" ]
                      ( [ input
                            [ Type "checkbox"
                              Disabled (not model.QualityProducts)
                              Style [ Visibility "hidden"; Position PositionOptions.Absolute ]
                              Checked model.Processors.[processor].PreservesQuality
                              OnChange (fun _ -> dispatch <| TogglePreservesQuality processor) ]
                          img
                            [ ClassName "checkbox-img"
                              Src (if model.Processors.[processor].PreservesQuality then "img/UI/CheckboxGreen.png" else "img/UI/Checkbox.png") ] ] 
                        @ sourceIcon processor.Value) ] ]
          checkboxWithText "Quality Seed Maker" ToggleQualitySeedMaker model.QualitySeedMaker dispatch
          str "(Average) Seed Amounts: "
          ul []
            [ for KeyValue(quality, amount) in model.QualitySeedMakerAmounts do
                li []
                  [ label []
                      [ str (string quality + ": ")
                        input
                          [ Type "number"
                            Min 0
                            Disabled (not model.QualitySeedMaker)
                            valueOrDefault amount
                            ClassName "skill-number-input"
                            OnChange (fun v -> dispatch <| SetQualitySeedMakerAmount (quality, !!v.Value)) ] ] ] ] ]
  //lazyView2With (fun oldModel newModel -> (not oldModel.SidebarOpen && not newModel.SidebarOpen) || oldModel = newModel) sidebarContent

let cover sidebarOpen dispatch =
  div
    [ classModifier "cover" "open" sidebarOpen
      OnClick (fun _ -> dispatch CloseSidebar) ]
    []

let sidebar model dispatch =
  div [ classModifier "sidebar" "open" model.SidebarOpen ]
    [ sidebarContent model dispatch
      viewTabsWith (fun t -> model.SidebarOpen && t = model.SidebarTab) "sidebar" SetSidebarTab SidebarTab.List dispatch ]

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