namespace StardewValleyStonks

open Types

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
    BuySeeds: bool
    Replants: Map<Replant, bool>
    Crops: Map<Name<Crop>, Crop>
    CropList: Name<Crop> list
    CropSort: CropSort
    CropSortAscending: bool
    ShowUselessCrops: bool
    Fertilizers: Map<Name<Fertilizer>, Fertilizer>
    FertilizerList: Name<Fertilizer> list
    FertilizerSort: FertilizerSort
    FertilizerSortAscending: bool
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

module Model =
  let seedMakerRequirement = [ SkillLevel (Name "Farming", 9) ]

  let validDate model = model.StartDate |> isBefore model.EndDate

  let private requirementStatusHelper requirementMet = function
    | Ignore -> Valid
    | Warn | Invalidate when requirementMet -> Valid
    | Warn -> Warning
    | Invalidate -> Invalid
  
  let requirementStatus model = function
    | SkillLevel (skill, level) -> requirementStatusHelper (model.Skills.[skill].Level >= level) model.SkillLevelRequirementsShould
    | Year y -> requirementStatusHelper (model.Year >= y) model.YearRequirementsShould
  
  let requirementStatusData requirements overallStatus model =
    requirements
    |> List.filter (fun req -> requirementStatus model req = overallStatus)
    |> List.map UnmetRequirement
  
  let overallRequirementStatus requirements model =
    let requirementStatuses = List.map (fun req -> req, requirementStatus model req) requirements
    if (List.exists (fun cs -> snd cs = Invalid) requirementStatuses) then
      Invalid
    elif (List.exists (fun cs -> snd cs = Warning) requirementStatuses) then
      Warning
    else
      Valid
  
  let sourceStatus source model = if model.BuySources.[source].Selected then Valid else Invalid
  
  let sourceStatusData source model = [ if not model.BuySources.[source].Selected then Reason "Is not selected." ]
  
  let productSourceSelected model = function
    | RawCrop -> model.SellRawItem
    | Processor p -> model.Processors.[p].Selected
    | ProductSource.SeedMaker -> model.SellSeedsFromSeedMaker
  
  let productSourceStatus model = function
    | RawCrop -> if model.SellRawItem then Valid else Invalid
    | Processor p ->
        if model.Processors.[p].Selected then
          overallRequirementStatus model.Processors.[p].Requirements model
        else Invalid
    | ProductSource.SeedMaker ->
        if model.SellSeedsFromSeedMaker then
          overallRequirementStatus seedMakerRequirement model
        else
          Invalid
  
  let productSourceStatusData overallStatus model = function
    | RawCrop -> [ if not model.SellRawItem then Reason "Is not selected." ]
    | Processor p ->
        [ if not model.Processors.[p].Selected then Reason "Is not selected." ]
        @ requirementStatusData model.Processors.[p].Requirements overallStatus model
    | ProductSource.SeedMaker ->
        [ if not model.SellSeedsFromSeedMaker then Reason "Is not selected." ]
        @ requirementStatusData seedMakerRequirement overallStatus model
  
  let replantStatus model = function
    | Replant.SeedMaker ->
        if model.Replants.[Replant.SeedMaker] then
          overallRequirementStatus seedMakerRequirement model
        else
          Invalid
    | r -> if model.Replants.[r] then Valid else Invalid
  
  let replantStatusData overallStatus model = function
    | Replant.SeedMaker ->
        [ if not model.Replants.[Replant.SeedMaker] then Reason "Is not selected." ]
        @ requirementStatusData seedMakerRequirement overallStatus model
    | r -> [ if not model.Replants.[r] then Reason "Is not selected." ]
  
  let priceStatus price model =
    match price |> Price.overrideSource with
    | Some true -> Valid //true //ignore source conditions but consider local conditions
    | Some false -> Invalid //false //false, return display: manually overriden to false
    | None ->
        if (sourceStatus (price |> Price.source) model = Valid) then
          overallRequirementStatus (price |> Price.requirements) model
        else
          Invalid
  
  let priceStatusData (priceStatus: Price * Status) model =
    ( match fst priceStatus |> Price.overrideSource with
      | Some true -> []
      | Some false -> []
      | None -> sourceStatusData (fst priceStatus |> Price.source) model)
    @ requirementStatusData (fst priceStatus |> Price.requirements) (snd priceStatus) model
  
  let rec priceOf (priceFrom: Map<Name<Source>, Price>) model = function
    | Price.Price p -> p.Value
    | MatchPrice m -> if model.MatchConditions.[m.MatchCondition].Selected then priceOf priceFrom model priceFrom.[m.MatchSource] else m.Value
  
  let priceStatuses (priceFrom: Map<Name<Source>, Price>) model =
    let priceStatuses = Map.map (fun source _ -> priceStatus priceFrom.[source] model) priceFrom
    let validPrices = Map.filter (fun _ status -> status <> Invalid) priceStatuses
    ( if (validPrices.IsEmpty) then
        priceFrom
      else
        let bestPrice = Map.fold (fun currentMin source _ -> min (priceOf priceFrom model priceFrom.[source] ) currentMin) Integer.MaxValue validPrices
        Map.filter (fun _ price -> priceOf priceFrom model price = bestPrice) priceFrom)
    |> Map.fold (fun prices source price -> (price, priceStatuses.[source])::prices) []
  
  let overallPriceStatus priceStatuses =
    if (List.exists (fun ps -> snd ps = Warning) priceStatuses) then
      Warning
    elif (List.exists (fun ps -> snd ps = Valid) priceStatuses) then
      Valid
    else
      Invalid
  
  let bestPrice (priceFrom: Map<Name<Source>, Price>) priceStatuses model =
    if priceFrom.IsEmpty then
      Integer.MaxValue
    elif overallPriceStatus priceStatuses = Invalid then
      Integer.MaxValue - 1
    else
      priceOf priceFrom model (fst priceStatuses.Head)
  
  let bestPrice2 (priceFrom: Map<Name<Source>, Price>) model =
    bestPrice priceFrom (priceStatuses priceFrom model) model
  
  let daysIn season model =
    if season >= model.StartDate.Season && season <= model.EndDate.Season then
      if model.StartDate.Season = model.EndDate.Season then
        model.EndDate.Day - model.StartDate.Day + 1
      elif season = model.StartDate.Season then
        29 - model.StartDate.Day
      elif season = model.EndDate.Season then
        model.EndDate.Day
      else
        28
    else
      0

  let fastestFertSpeed model =
    model.FertilizerList
    |> List.filter (fun fert -> model.Fertilizers.[fert].Selected && List.exists (fun (_, s) -> s <> Invalid) (priceStatuses model.Fertilizers.[fert].PriceFrom model))
    |> (fun list -> if list.IsEmpty then 0.0 else model.Fertilizers.[ (List.maxBy (fun fert -> model.Fertilizers.[fert].Speed) list) ].Speed)


  let canGiveOneHarvest model crop =
    let seasons = seasonsFrom model.StartDate.Season model.EndDate.Season
    if List.exists crop.Seasons.Contains seasons then
      let lastConsecDays, maxDays =
        seasons
        |> List.fold
          (fun (consecDays, maxDays) season ->
            if crop.Seasons.Contains season then
              (consecDays + daysIn season model, maxDays)
            else
              (0, max consecDays maxDays))
          (0, 0)
      Crop.growthTimeWith (fastestFertSpeed model) crop <= (max lastConsecDays maxDays) - 1
    else
      false

  let sortMode<'t, 'key when 'key: comparison> ascending =
    if ascending then
      List.sortBy<'t, 'key>
    else
      List.sortByDescending<'t, 'key>

  let sortedFertilizers model =
    [ for KeyValue(_, fert) in model.Fertilizers do
        fert ]
    |> List.sortBy Fertilizer.name
    |> (fun list ->
      ( match model.FertilizerSort with
        | FertilizerSort.ByName -> list
        | FertilizerSort.Selected -> (sortMode model.FertilizerSortAscending) Fertilizer.selected list
        | Quality -> (sortMode model.FertilizerSortAscending) Fertilizer.quality list
        | Speed -> (sortMode model.FertilizerSortAscending) Fertilizer.speed list
        | FertilizerSort.Price -> (sortMode model.FertilizerSortAscending) (fun f -> bestPrice2 f.PriceFrom model) list))
  
  let sortedCrops model =
    [ for KeyValue(_, crop) in model.Crops do
        if model.ShowUselessCrops || crop |> canGiveOneHarvest model then crop ]
    |> List.sortBy Crop.name
    |> List.sortBy Crop.seasons
    |> (fun list ->
      ( match model.CropSort with
        | CropSort.ByName -> (sortMode model.CropSortAscending) Crop.name list
        | CropSort.Selected -> (sortMode model.CropSortAscending) Crop.selected list
        | Seasons -> list
        | TotalGrowthTime -> (sortMode model.CropSortAscending) Crop.totalGrowthTime list
        | RegrowTime -> (sortMode model.CropSortAscending) Crop.regrowTime list
        | SeedPrice -> (sortMode model.CropSortAscending) (fun c -> bestPrice2 c.PriceFrom model) list))
  
  let doubleCropProb model = 0.0001 + float model.LuckBuff / 1500.0 + (if model.SpecialCharm then 0.025 else 0.0)
  
  let noGiantCropProb model = (1.0 - model.BaseGiantCropChance) ** model.GiantCropChecksPerTile
  
  let amountOf crop model =
    let noGiantCropProb = if crop.IsGiantCrop then noGiantCropProb model else 1.0
    (noGiantCropProb,
     noGiantCropProb * (crop.ExtraCrops + (if crop.HasDoubleCropChance then crop.ExtraCrops * doubleCropProb model + doubleCropProb model else 0.0))
     + (1.0 - noGiantCropProb) * 2.0)
  
  let growthMultiplierValue model name =
    match model.Multipliers.[name] with
    | Multiplier m -> if m.Selected then m.Value else 0.0
    | Profession p ->
        let skill = model.Skills.[p.Skill]
        if skill.Professions.[p.Profession].Selected && (Skill.professionIsUnlocked p.Profession skill || model.SkillLevelRequirementsShould <> Invalidate) then
          p.Value
        else
          0.0
  
  let fastestGrowthTime fertSpeed (crop: Crop) model =
    Crop.growthTimeWith (fertSpeed + List.sumBy (growthMultiplierValue model) crop.GrowthMultipliers)