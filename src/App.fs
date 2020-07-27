module StardewValleyStonks.App

open Fable.Core
open Fable.Core.JsInterop
open Elmish

importAll "../sass/main.sass"

//--Model--
open Types

let init () =
  let skills =
    [ { Skill.initial with
          Name = "Farming"
          Professions =
            listToMap
              Profession.nameKey
              [ { Profession.initial with
                    Name = "Tiller"
                    UnlockLevel = 5
                    Dependants = set [ Name "Artisan"; Name "Agriculturist" ] }
                { Profession.initial with
                    Name = "Artisan"
                    Requires = set [ Name "Tiller" ]
                    ExclusiveWith = set [ Name "Agriculturist" ] }
                { Profession.initial with
                    Name = "Agriculturist"
                    Requires = set [ Name "Tiller" ]
                    ExclusiveWith = set [ Name "Artisan" ] } ]

          ProfessionLayout =
            [ [ Name "Tiller" ]
              [ Name "Artisan"; Name "Agriculturist" ] ] }
      { Skill.initial with
          Name = "Foraging"
          Professions =
            listToMap
              Profession.nameKey
              [ { Profession.initial with
                    Name = "Gatherer"
                    UnlockLevel = 5
                    Dependants = set [ Name "Botanist" ] }
                { Profession.initial with
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
    |> List.map Source.create

  let matchConditions = [ { Name = "Joja Membership"; Selected = false } ]

  let processors =
    [ { Processor.initial with
          Name = "Preserves Jar"
          Requirements = [ SkillLevel (Name "Farming", 4) ] }
      { Processor.initial with
          Name = "Keg"
          Requirements = [ SkillLevel (Name "Farming", 8) ] }
      { Processor.initial with
          Name = "Oil Maker"
          Requirements = [ SkillLevel (Name "Farming", 8) ] }
      { Processor.initial with Name = "Mill" } ]

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

  let crops =
    [ Crop.create
        "Blue Jazz"
        [ Spring ]
        [ 1; 2; 2; 2 ]
        (SellPrice 50)
        (Seed (item "Jazz Seeds" 15))
        NoProduct
        PierreAndJoja

      { Crop.create
          "Cauliflower"
          [ Spring ]
          [ 1; 2; 4; 4; 1 ]
          (SellPrice 175)
          (SeedSell 40)
          Vegetable
          PierreAndJoja
        with
          IsGiantCrop = true }

      { Crop.createWithoutSeedMaker
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
          (PriceList [ Price.create "Traveling Merchant" 2500 ] )
        with
          RegrowTime = Some 2
          ExtraCrops = Crop.extraCrops 4 0.02 }

      Crop.create
        "Garlic"
        [ Spring ]
        [ 1; 1; 1; 1 ]
        (SellPrice 60)
        (SeedSell 20)
        Vegetable
        (PriceList
          [ Price.Price
              {| Value = 40
                 Source = Name "Pierre"
                 Requirements = [ Year 2 ]
                 OverrideSource = None |} ] )

      { Crop.create
          "Green Bean"
          [ Spring ]
          [ 1; 1; 1; 3; 4 ]
          (SellPrice 40)
          (Seed (item "Bean Starter" 30))
          Vegetable
          PierreAndJoja
        with
          RegrowTime = Some 3 }

      { Crop.create
          "Kale"
          [ Spring ]
          [ 1; 2; 2; 1 ]
          (SellPrice 110)
          (SeedSell 35)
          Vegetable
          PierreAndJoja
        with
          HasDoubleCropChance = false }

      Crop.create
        "Parsnip"
        [ Spring ]
        [ 1; 1; 1; 1 ]
        (SellPrice 35)
        (SeedSell 10)
        Vegetable
        PierreAndJoja

      { Crop.create
          "Potato"
          [ Spring ]
          [ 1; 1; 1; 2; 1 ]
          (SellPrice 80)
          (SeedSell 25)
          Vegetable
          PierreAndJoja
        with
          ExtraCrops = Crop.extraCrops 1 0.2 }

      Crop.create
        "Rhubarb"
        [ Spring ]
        [ 2; 2; 2; 3; 4]
        (SellPrice 220)
        (SeedSell 50)
        Fruit
        Oasis

      { Crop.create
          "Strawberry"
          [ Spring ]
          [ 1; 1; 2; 2; 2 ]
          (SellPrice 120)
          (SeedSell 0)
          Fruit
          (PriceList [ Price.create "Pierre" 100 ])
        with
          RegrowTime = Some 4
          ExtraCrops = Crop.extraCrops 1 0.02 }

      Crop.create
        "Tulip"
        [ Spring ]
        [ 1; 1; 2; 2 ]
        (Item (item "Tulip Bulb" 10))
        (SeedSell 30)
        NoProduct
        PierreAndJoja

      { Crop.create
          "Rice"
          [ Spring ]
          [ 1; 2; 2; 3 ]
          (Item (cropItem "Unmilled Rice" 30))
          (Seed (item "Rice Shoot" 20))
          (CreateAndList
            ( Vegetable,
              [ Process
                  {| Processor = Name "Mill"
                     Output = item "Rice" 100
                     Override = None |} ] ))
          Pierre
        with
          GrowthMultipliers = Name "Irrigated"::Crop.agri
          HasDoubleCropChance = false
          ExtraCrops = Crop.extraCrops 1 0.1 }

      { Crop.create
          "Blueberry"
          [ Summer ]
          [ 1; 3; 3; 4; 2 ]
          (SellPrice 50)
          (SeedSell 40)
          Fruit
          Pierre
        with
          RegrowTime = Some 4
          ExtraCrops = Crop.extraCrops 3 0.02 }

      { Crop.create
          "Corn"
          [ Summer; Fall ]
          [ 2; 3; 3; 3; 3 ]
          (SellPrice 50)
          (SeedSell 75)
          (CreateAndList (Vegetable, [ oil ] ))
          PierreAndJoja
        with
          RegrowTime = Some 4 }

      { Crop.create
          "Hops"
          [ Summer ]
          [ 1; 1; 2; 3; 4 ]
          (SellPrice 25)
          (Seed (item "Hops Starter" 30))
          (CreateAndList (Jar Pickle, [ kegProduct "Pale Ale" 300 ] ))
          PierreAndJoja
        with
          RegrowTime = Some 1 }

      { Crop.create
          "Hot Pepper"
          [ Summer ]
          [ 1; 1; 1; 1; 1 ]
          (SellPrice 40)
          (Seed (item "Pepper Seeds" 20))
          Fruit
          PierreAndJoja
        with
          RegrowTime = Some 3
          ExtraCrops = Crop.extraCrops 1 0.03 }

      { Crop.create
          "Melon"
          [ Summer ]
          [ 1; 2; 3; 3; 3 ]
          (SellPrice 250)
          (SeedSell 40)
          Fruit
          PierreAndJoja
        with
          IsGiantCrop = true }

      Crop.create
        "Poppy"
        [ Summer ]
        [ 1; 2; 2; 2 ]
        (SellPrice 140)
        (SeedSell 50)
        NoProduct
        PierreAndJoja

      Crop.create
        "Radish"
        [ Summer ]
        [ 2; 1; 2; 1 ]
        (SellPrice 90)
        (SeedSell 20)
        Vegetable
        PierreAndJoja

      Crop.create
        "Red Cabbage"
        [ Summer ]
        [ 2; 1; 2; 2; 2 ]
        (SellPrice 260)
        (SeedSell 50)
        Vegetable
        (PriceList
          [ Price.Price
              {| Value = 100
                 Source = Name "Pierre"
                 Requirements = [ Year 2 ]
                 OverrideSource = None |} ] )

      Crop.create
        "StarFruit"
        [ Summer ]
        [ 2; 3; 2; 3; 3 ]
        (SellPrice 750)
        (SeedSell 200)
        Fruit
        Oasis

      Crop.create
        "Summer Spangle"
        [ Summer ]
        [ 1; 2; 3; 2 ]
        (SellPrice 90)
        (Seed (item "Spangle Seeds" 25))
        NoProduct
        PierreAndJoja

      { Crop.create
          "Sunflower"
          [ Summer; Fall ]
          [ 1; 2; 3; 2 ]
          (SellPrice 80)
          (SeedSell 20)
          (ProductList [ oil ] )
          (PriceList
            [ Price.create "Pierre" 200
              Price.create "Joja" 125 ] )
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
                    Replant = Some SeedOrCrop
                    ReplantOverride = None } ) ] }

      { Crop.create
          "Tomato"
          [ Summer ]
          [ 2; 2; 2; 2; 3 ]
          (SellPrice 60)
          (SeedSell 25)
          Fruit
          PierreAndJoja
        with
          RegrowTime = Some 4
          ExtraCrops = Crop.extraCrops 1 0.05 }

      { Crop.create
          "Wheat"
          [ Summer; Fall ]
          [ 1; 1; 1; 1 ]
          (SellPrice 25)
          (SeedSell 5)
          (CreateAndList
            ( Jar Pickle,
              [ kegProduct "Beer" 200
                Process
                  {| Processor = Name "Mill"
                     Output = item "Wheat Flour" 50
                     Override = None |} ] ))
          PierreAndJoja
        with
          HasDoubleCropChance = false }

      { Crop.create
          "Amaranth"
          [ Fall ]
          [ 1; 2; 2; 2 ]
          (SellPrice 150)
          (SeedSell 35)
          Vegetable
          PierreAndJoja
        with
          HasDoubleCropChance = false }

      Crop.create
        "Artichoke"
        [ Fall ]
        [ 2; 2; 1; 2; 1 ]
        (SellPrice 160)
        (SeedSell 15)
        Vegetable
        (PriceList
          [ Price.Price
              {| Value = 30
                 Source = Name "Pierre"
                 Requirements = [ Year 2 ]
                 OverrideSource = None |} ] )

      Crop.create
        "Beet"
        [ Fall ]
        [ 1; 1; 2; 2 ]
        (SellPrice 100)
        (SeedSell 10)
        (CreateAndList
          ( Vegetable,
            [ RatioProcess
                {| InputAmount = 1
                   Processor = Name "Mill"
                   Output = item "Sugar" 50
                   OutputAmount = 3.0
                   Override = None |} ] ))
         Oasis

      Crop.create
        "Bok Choy"
        [ Fall ]
        [ 1; 1; 1; 1 ]
        (SellPrice 80)
        (SeedSell 25)
        Vegetable
        PierreAndJoja

      { Crop.create
          "Cranberries"
          [ Fall ]
          [ 1; 2; 1; 1; 2 ]
          (SellPrice 75)
          (SeedSell 60)
          Fruit
          (Joja (Price.create "Pierre" 240))
        with
          RegrowTime = Some 5
          ExtraCrops = Crop.extraCrops 2 0.1 }

      { Crop.create
          "Eggplant"
          [ Fall ]
          [ 1; 1; 1; 1; 1 ]
          (SellPrice 60)
          (SeedSell 10)
          Vegetable
          PierreAndJoja
        with
          RegrowTime = Some 5
          ExtraCrops = Crop.extraCrops 1 0.002 }

      Crop.create
        "Fairy Rose"
        [ Fall ]
        [ 1; 4; 4; 3 ]
        (SellPrice 290)
        (Seed (item "Fairy Seeds" 100))
        NoProduct
        PierreAndJoja

      { Crop.create
          "Grape"
          [ Fall ]
          [ 1; 1; 2; 3; 3 ]
          (SellPrice 30)
          (Seed (item "Grape Starter" 30))
          Fruit
          PierreAndJoja
        with
          RegrowTime = Some 3 }

      { Crop.create
          "Pumpkin"
          [ Fall ]
          [ 1; 2; 3; 4; 3 ]
          (SellPrice 320)
          (SeedSell 50)
          Vegetable
          PierreAndJoja
        with
          IsGiantCrop = true }

      Crop.create
        "Sweet Gem Berry"
        [ Fall ]
        [ 2; 4; 6; 6; 6 ]
        (Item (item "Sweet Gem Berry" 3000))
        (Seed (item "Rare Seed" 200))
        NoProduct
        (PriceList [ Price.create "Traveling Merchant" 1000 ])

      Crop.create
        "Yam"
        [ Fall ]
        [ 1; 3; 3; 3 ]
        (SellPrice 160)
        (SeedSell 30)
        Vegetable
        PierreAndJoja

      { Crop.create
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
    [ Fertilizer.create
        "Basic Fertilizer"
        1
        0.0
        [ Price.create "Pierre" 100 ]

      Fertilizer.create
        "Quality Fertilizer"
        2
        0.0
        [ Price.Price
            {| Value = 150
               Source = Name "Pierre"
               Requirements = [ Year 2 ]
               OverrideSource = None |} ]

      Fertilizer.create
        "Speed-Gro"
        0
        0.1
        [ Price.create "Pierre" 100 ]

      Fertilizer.create
        "Deluxe Speed-Gro"
        0
        0.25
        [ Price.Price
            {| Value = 150
               Source = Name "Pierre"
               Requirements = [ Year 2 ]
               OverrideSource = None |}
          Price.create "Oasis" 80 ] ]

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
    BuySeeds = true
    Crops =
      crops
      |> List.map (fun c -> (Name c.Name), c)
      |> Map.ofList
    CropList = List.map (fun (c: Crop) -> Name c.Name) crops
    CropSort = Seasons
    CropSortAscending = true
    ShowUselessCrops = false
    Fertilizers =
      fertilizers
      |> List.map (fun f -> (Name f.Name, f))
      |> Map.ofList
    FertilizerList = List.map (fun (f: Fertilizer) -> Name f.Name) fertilizers
    FertilizerSort = Speed
    FertilizerSortAscending = true
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
  | SetSkillLevel of Skill: Name<Skill> * Level: int
  | SetSkillBuff of Skill: Name<Skill> * Buff: int
  | ToggleProfession of Name<Skill> * Name<Profession>
  | ToggleBuySource of Name<Source>
  | ToggleMatchCondition of Name<MatchCondition>
  | ToggleProductSource of ProductSource
  | ToggleReplant of Replant
  | ToggleBuySeeds
  | ToggleCropSelected of Name<Crop>
  | SetCropSort of CropSort
  | ToggleFertilizerSelected of Name<Fertilizer>
  | SetFertilizerSort of FertilizerSort
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
      { model with Skills = model.Skills.Add(skill, model.Skills.[skill] |> Skill.toggleProfession name model.IgnoreProfessions) }
  | ToggleBuySource source -> { model with BuySources = model.BuySources.Add(source, model.BuySources.[source].Toggle) }
  | ToggleMatchCondition cond -> { model with MatchConditions = model.MatchConditions.Add(cond, model.MatchConditions.[cond].Toggle) }
  | ToggleProductSource source ->
      match source with
      | RawCrop -> { model with SellRawItem = not model.SellRawItem }
      | Processor p ->
        { model with Processors = model.Processors.Add(p, model.Processors.[p].Toggle) }
      | ProductSource.SeedMaker -> { model with SellSeedsFromSeedMaker = not model.SellSeedsFromSeedMaker }
  | ToggleReplant replant -> { model with Replants = model.Replants.Add(replant, not model.Replants.[replant] ) }
  | ToggleBuySeeds -> { model with BuySeeds = not model.BuySeeds }
  | ToggleCropSelected crop -> { model with Crops = model.Crops.Add(crop, model.Crops.[crop].Toggle) }
  | SetCropSort sort ->
      if sort = model.CropSort then
        { model with CropSortAscending = not model.CropSortAscending }
      else
        { model with
            CropSort = sort
            CropSortAscending = true }
  | ToggleFertilizerSelected fert -> { model with Fertilizers = model.Fertilizers.Add(fert, model.Fertilizers.[fert].Toggle) }
  | SetFertilizerSort sort ->
      if sort = model.FertilizerSort then
        { model with FertilizerSortAscending = not model.FertilizerSortAscending }
      else
        { model with
            FertilizerSort = sort
            FertilizerSortAscending = true }
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

let viewTab string css message tab active dispatch =
  li
    [ classModifier (css + "-tab") "active" active
      OnClick (fun _ -> dispatch <| message tab) ]
    [ str (string tab) ]

let viewTabsWith activeFun string css message list dispatch =
  ul [ ClassName (css + "-tabs") ]
    [ for tab in list do
        viewTab string css message tab (activeFun tab) dispatch ]

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
            if Skill.professionIsUnlocked name skill || requirementsShould <> Invalidate then
              "--active"
            else
              "--error"
          else
            "")
      OnClick (fun _ -> dispatch <| ToggleProfession (Types.Name skill.Name, name)) ]
    [ if skill.Professions.[name].Selected && not (Skill.professionIsUnlocked name skill) then
        if requirementsShould = Warn then
          warningIcon
        elif requirementsShould = Invalidate then
          errorIcon
      img
        [ ClassName "profession-img"
          Src ("img/Skills/" + ofName name + ".png") ]
      str (ofName name) ]

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
          | SkillLevel (skill, level) -> str (ofName skill + " level too low. Unlocks at level " + string level + ".")
          | Year y -> str ("Available only from year " + string y + " and onwards.")
      | Reason reason -> str reason
      | SubReason (text, sub) -> str text; for subReason in sub do ul [] [ invalidReason subReason ] ]

let buySource (name: Name<Source>) selected dispatch =
  li []
    [ checkboxWith (sourceIcon (ofName name)) (ToggleBuySource name) selected dispatch ]

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
        let status = Model.productSourceStatus model source
        let selected = Model.productSourceSelected model source
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
              [ for reason in Model.productSourceStatusData status model source do
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
              [ for reason in Model.productSourceStatusData status model source do
                  invalidReason reason ] ]

let replant (replant: Replant) selected status dispatch =
  li []
    [ statusCheckbox (sourceIcon replant.Name) (ToggleReplant replant) selected status dispatch ]

let replants (model: Model) dispatch =
  ul [ ClassName "source-list" ]
    [ li []
        [ checkboxWith (sourceIcon "Buy Seeds") ToggleBuySeeds model.BuySeeds dispatch ]
      for r in Replant.List do
        let status = Model.replantStatus model r
        replant r model.Replants.[r] status dispatch
        if r = Replant.SeedMaker && status = Warning then
          label
            [ ClassName "details-label"
              HtmlFor r.Name ]
            [ str "Show Warnings"]
          input
            [ Type "checkbox"
              ClassName "details-input"
              Id r.Name ]
          ul [ ClassName "details" ]
            [ for reason in Model.replantStatusData status model r do
                invalidReason reason ] ]

let selectOptions (list: 't seq) string (ofString: string -> 't) text message (value: 't) dispatch=
  label []
      [ ofOption (text |> Option.bind (fun t -> str (t + ": ") |> Some))
        select
          [ valueOrDefault value
            OnChange (fun x -> ofString x.Value |> message |> dispatch) ]
          [ for something in list do
              option [ Value something ]
                [ str (string something) ] ] ]

let selectRequirementsShould = selectOptions RequirementsShould.List string parseRequirementsShould

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
      [ str (string (Model.priceOf priceFrom model (fst priceStatuses.Head)) + "g")
        for price in priceStatuses do
          img
            [ classModifier "price-img" "warning" (snd price = Warning)
              Src ("img/Sources/" + (fst price |> Price.source |> ofName) + ".png") ]
        br []
        warningIcon
        label
          [ ClassName "details-label"
            HtmlFor (ofName name) ]
          [ str "Show Warnings"]
        input
          [ Type "checkbox"
            ClassName "details-input"
            Id (ofName name) ]
        ul [ ClassName "details" ]
            [ for priceStatus in priceStatuses do
                li []
                  ( sourceIcon (fst priceStatus |> Price.source |> ofName) @
                    [ ul []
                        [ for reason in Model.priceStatusData priceStatus model do
                            invalidReason reason ] ] ) ] ]
  | Valid ->
      [ str (string (Model.priceOf priceFrom model (fst priceStatuses.Head)) + "g")
        for price in priceStatuses do
          img
            [ ClassName "price-img"
              Src ("img/Sources/" + (fst price |> Price.source |> ofName) + ".png") ] ]
  | Invalid ->
      [ errorIcon
        label
          [ ClassName "details-label"
            HtmlFor (ofName name) ]
          [ str "Show Errors"]
        input
          [ Type "checkbox"
            ClassName "details-input"
            Id (ofName name) ]
        ul [ ClassName "details" ]
            [ for priceStatus in priceStatuses do
                li []
                  ( sourceIcon (fst priceStatus |> Price.source |> ofName) @
                    [ ul []
                        [ for reason in Model.priceStatusData priceStatus model do
                            invalidReason reason ] ] ) ] ]

let selectSeasons = selectOptions allSeasons string parseSeason None

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
                            Src ("/img/Skills/" + ofName skill + ".png")]
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
                    [ th [ OnClick (fun _ -> dispatch <| SetCropSort CropSort.Selected) ] []
                      th [ OnClick (fun _ -> dispatch <| SetCropSort CropSort.ByName) ] [ str "Crop" ]
                      th [ OnClick (fun _ -> dispatch <| SetCropSort TotalGrowthTime) ] [ str "Growth Time" ]
                      th [ OnClick (fun _ -> dispatch <| SetCropSort RegrowTime) ] [ str "Regrow Time" ]
                      th [ OnClick (fun _ -> dispatch <| SetCropSort Seasons) ] [ str "Seasons" ]
                      th [ OnClick (fun _ -> dispatch <| SetCropSort SeedPrice) ] [ str "Seed Price" ] ] ]
              tbody [ ClassName "table-body" ]
                [ ofList
                    [ for crop in Model.sortedCrops model do
                        let priceStatuses = Model.priceStatuses crop.PriceFrom model
                        let status =
                          if (List.exists (fun ps -> snd ps = Valid) priceStatuses) then
                            Valid
                          elif (List.exists (fun ps -> snd ps = Warning) priceStatuses) then
                            Warning
                          else
                            Invalid
                        tr [ Key crop.Name ]
                          [ td [] [ statusCheckbox [] (ToggleCropSelected (Types.Name crop.Name)) crop.Selected status dispatch ]
                            td []
                              [ img
                                  [ ClassName "crop-img"
                                    Src ("/img/Crops/" + crop.Name + ".png") ]
                                str crop.Name ]
                            td [] [ ofInt crop.TotalGrowthTime ]
                            td [] [ ofOption (Option.bind (ofInt >> Some) crop.RegrowTime) ]
                            td [] [ for season in crop.Seasons do str (string season); br [] ]
                            td []
                              ( if not crop.PriceFrom.IsEmpty then
                                  viewPrices model (Types.Name crop.Name) crop.PriceFrom priceStatuses
                                else
                                  [ str "N/A" ] ) ] ] ] ]
          checkboxWithText "Show Crops That Cannot Give One Harvest" ToggleShowUselessCrops model.ShowUselessCrops dispatch ]
  | Fertilizers ->
      div [ classModifier "sidebar-table-content" "open" model.SidebarOpen ]
        [ table [ ClassName "table" ]
            [ thead [ ClassName "table-header" ]
                [ tr []
                    [ th [ OnClick (fun _ -> dispatch <| SetFertilizerSort FertilizerSort.Selected) ] []
                      th [ OnClick (fun _ -> dispatch <| SetFertilizerSort FertilizerSort.ByName) ] [ str "Fertilizer" ]
                      th [ OnClick (fun _ -> dispatch <| SetFertilizerSort Quality) ] [ str "Quality" ]
                      th [ OnClick (fun _ -> dispatch <| SetFertilizerSort Speed) ] [ str "Speed Bonus" ]
                      th [ OnClick (fun _ -> dispatch <| SetFertilizerSort FertilizerSort.Price) ] [ str "Price" ] ] ]
              tbody [ ClassName "table-body" ]
                [ ofList
                    [ for fert in Model.sortedFertilizers model do
                        let priceStatuses = Model.priceStatuses fert.PriceFrom model
                        let status =
                          if (List.exists (fun ps -> snd ps = Valid) priceStatuses) then
                            Valid
                          elif (List.exists (fun ps -> snd ps = Warning) priceStatuses) then
                            Warning
                          else
                            Invalid
                        tr [ Key fert.Name ]
                          [ td [] [ statusCheckbox [] (ToggleFertilizerSelected (Types.Name fert.Name)) fert.Selected status dispatch ] 
                            td []
                              [ img
                                  [ ClassName "fertilizer-img"
                                    Src ("/img/Fertilizers/" + fert.Name + ".png") ]
                                str fert.Name ]
                            td [] [ ofInt fert.Quality ]
                            td [] [ str (string (fert.Speed * 100.0) + "%") ]
                            td [] (viewPrices model (Types.Name fert.Name) fert.PriceFrom priceStatuses) ] ] ] ]
          label []
            [ str "Starting Fertilizer: "
              select
                [ valueOrDefault
                    ( match model.StartingFertilizer with
                      | Some fert -> ofName fert
                      | None -> "None")
                  OnChange (fun fert ->
                    ( match fert.Value with
                      | "None" -> None
                      | f -> Some (Types.Name f))
                    |> SetStartingFertilizer |> dispatch) ]
                [ option [ Value "None" ]
                    [ str "None" ]
                  for fert in model.FertilizerList do
                    option [ Value (ofName fert) ]
                      [ str (ofName fert) ] ] ] ]
  | Buy ->
      div [ classModifier "sidebar-content" "open" model.SidebarOpen ]
        [ buySources model.BuySourceList model.BuySources dispatch
          ul [ ClassName "match-condition-list" ]
            [ for cond in model.MatchConditionList do
              checkboxWithText (ofName cond) (ToggleMatchCondition cond) model.MatchConditions.[cond].Selected dispatch ] ]
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
          if not (Model.validDate model) then
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
                        @ sourceIcon (ofName processor)) ] ]
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
      viewTabsWith (fun t -> model.SidebarOpen && t = model.SidebarTab) string "sidebar" SetSidebarTab SidebarTab.List dispatch ]

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