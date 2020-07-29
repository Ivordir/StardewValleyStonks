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
  | Date
  | Settings
  | Mod
  static member List =
    [ Skills
      Crops
      Fertilizers
      Buy
      Sell
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
    Skills: Map<NameOf<Skill>, Skill>
    SkillList: NameOf<Skill> list
    Multipliers: Map<NameOf<Multiplier>, Multiplier>
    IgnoreProfessions: bool
    BuySources: Map<NameOf<Source>, Source>
    BuySourceList: NameOf<Source> list //lists are necessary to enforce/preserve order, haven't thought of another way
    MatchConditions: Map<NameOf<MatchCondition>, MatchCondition>
    MatchConditionList: NameOf<MatchCondition> list
    Processors: Map<NameOf<Processor>, Processor>
    ProcessorList: NameOf<Processor> list
    SellRawItem: bool
    SellSeedsFromSeedMaker: bool
    ProductSources: ProductSource list
    BuySeeds: bool
    Replants: Map<Replant, bool>
    Crops: Map<NameOf<Crop>, Crop>
    CropList: NameOf<Crop> list
    CropSort: CropSort
    CropSortAscending: bool
    ShowUselessCrops: bool
    Fertilizers: Map<NameOf<Fertilizer>, Fertilizer>
    FertilizerList: NameOf<Fertilizer> list
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
    StartingFertilizer: NameOf<Fertilizer> option
    QualityProducts: bool
    QualitySeedMaker: bool
    QualitySeedMakerAmounts: Map<Quality, int> }

type DisplayPrices =
  | PriceData of (int * (NameOf<Source> * Status) list)
  | NoValidPrices
  | NoPrices

module Model =
  let seedMakerRequirement = [ SkillLevel (Name "Farming", 9) ]

  let notSelected = Reason "Is not selected."

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
  
  let flip f a b = f b a

  let overallRequirementStatus model =
    List.fold (flip ((requirementStatus model) >> Status.compareInvalid)) Valid
  
  let sourceActive source model = model.BuySources.[source].Selected
  
  let sourceStatusData source model = [ if not model.BuySources.[source].Selected then notSelected ]
  
  let productSourceSelected model = function
    | RawCrop -> model.SellRawItem
    | Processor p -> model.Processors.[p].Selected
    | ProductSource.SeedMaker -> model.SellSeedsFromSeedMaker
  
  let productSourceStatus model = function
    | RawCrop -> if model.SellRawItem then Valid else Invalid
    | Processor p ->
        if model.Processors.[p].Selected then
          overallRequirementStatus model model.Processors.[p].Requirements
        else
          Invalid
    | ProductSource.SeedMaker ->
        if model.SellSeedsFromSeedMaker then
          overallRequirementStatus model seedMakerRequirement
        else
          Invalid
  
  let productSourceStatusData overallStatus model = function
    | RawCrop -> [ if not model.SellRawItem then notSelected ]
    | Processor p ->
        [ if not model.Processors.[p].Selected then notSelected ]
        @ requirementStatusData model.Processors.[p].Requirements overallStatus model
    | ProductSource.SeedMaker ->
        [ if not model.SellSeedsFromSeedMaker then notSelected ]
        @ requirementStatusData seedMakerRequirement overallStatus model
  
  let replantStatus model = function
    | SeedOrCrop -> if model.Replants.[SeedOrCrop] then Valid else Invalid
    | Replant.SeedMaker ->
        if model.Replants.[SeedMaker] then
          overallRequirementStatus model seedMakerRequirement
        else
          Invalid
  
  let replantStatusData overallStatus model = function
    | SeedOrCrop -> [ if not model.Replants.[SeedOrCrop] then notSelected ]
    | Replant.SeedMaker ->
        [ if not model.Replants.[SeedMaker] then notSelected ]
        @ requirementStatusData seedMakerRequirement overallStatus model
  
  let priceStatus price model =
    match price |> Price.overrideSource with
    | Some true -> Valid //true //ignore source conditions but consider local conditions
    | Some false -> Invalid //false //false, return display: manually overriden to false
    | None ->
        if sourceActive (price |> Price.source) model then
          overallRequirementStatus model (price |> Price.requirements)
        else
          Invalid
  
  let priceStatusData priceStatus model =
    ( match fst priceStatus |> Price.overrideSource with
      | Some true -> []
      | Some false -> []
      | None -> sourceStatusData (fst priceStatus |> Price.source) model)
    @ requirementStatusData (fst priceStatus |> Price.requirements) (snd priceStatus) model
  
  let rec priceOf (priceFrom: Map<NameOf<Source>, Price>) model = function
    | Price.Price p -> p.Value
    | MatchPrice m -> if model.MatchConditions.[m.MatchCondition].Selected then priceOf priceFrom model priceFrom.[m.MatchSource] else m.Value
  
  let priceStatuses (priceFrom: Map<NameOf<Source>, Price>) model =
    let priceStatuses = Map.map (fun source _ -> priceStatus priceFrom.[source] model) priceFrom
    let validPrices = Map.filter (fun _ status -> status <> Invalid) priceStatuses
    ( if (validPrices.IsEmpty) then
        priceFrom
      else
        let bestPrice = Map.fold (fun currentMin source _ -> min (priceOf priceFrom model priceFrom.[source] ) currentMin) Integer.MaxValue validPrices
        Map.filter (fun _ price -> priceOf priceFrom model price = bestPrice) priceFrom)
    |> Map.fold (fun prices source price -> (price, priceStatuses.[source])::prices) []

  let priceData model (priceFrom: Map<NameOf<Source>, Price>) =
    if priceFrom.IsEmpty then
      NoPrices
    else
      let bestPrice =
        Map.fold
          (fun currentMin source _ ->
            if sourceActive source model then
              min (priceOf priceFrom model priceFrom.[source]) currentMin
            else
              currentMin)
          Integer.MaxValue
          priceFrom
      if bestPrice = Integer.MaxValue then
        NoValidPrices
      else
        PriceData
          ( bestPrice,
            [ for KeyValue(source, price) in priceFrom do
                let status = priceStatus price model
                if status <> Invalid && priceOf priceFrom model price = bestPrice then source, status ] )

  let bestPrice (priceFrom: Map<NameOf<Source>, Price>) priceStatuses model =
    if priceFrom.IsEmpty then
      Integer.MaxValue
    elif List.fold Status.compareWVI Invalid (priceStatuses |> List.unzip |> snd) = Invalid then
      Integer.MaxValue - 1
    else
      priceOf priceFrom model (fst priceStatuses.Head)
  
  let bestPrice2 priceFrom model = bestPrice priceFrom (priceStatuses priceFrom model) model
  
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

  let growthMultiplierValue model name =
    match model.Multipliers.[name] with
    | Multiplier m -> if m.Selected then m.Value else 0.0
    | Profession p ->
        let skill = model.Skills.[p.Skill]
        if skill.Professions.[p.Profession].Selected && (p.Profession |> Profession.isUnlocked skill || model.SkillLevelRequirementsShould <> Invalidate) then
          p.Value
        else
          0.0
  
  let fastestGrowthTime model crop =
    Crop.growthTimeWith (fastestFertSpeed model + List.sumBy (growthMultiplierValue model) crop.GrowthMultipliers) crop

  let seasons model = Season.from model.StartDate.Season model.EndDate.Season

  let isInSeason model crop = List.exists crop.SelectedSeasons.Contains (seasons model)

  let canGiveOneHarvest model crop =
    let lastConsecDays, maxDays =
      seasons model
      |> List.fold
        (fun (consecDays, maxDays) season ->
          if crop.Seasons.Contains season then
            (consecDays + daysIn season model, maxDays)
          else
            (0, max consecDays maxDays))
        (0, 0)
    fastestGrowthTime model crop <= (max lastConsecDays maxDays) - 1

  let productStatus model source product =
    match Product.sourceOverride product with
    | Some true -> Valid
    | Some false -> Invalid
    | None -> productSourceStatus model source

  let productStatuses model =
    Map.fold (fun status source product -> productStatus model source product |> Status.compareValid status) Invalid

  let cropProductStatus model crop =
    [ productStatuses model crop.Products
      for KeyValue(_, item) in crop.HarvestedItems do
        productStatuses model item.Products ]
    |> List.fold Status.compareValid Invalid

  let cropReplantStatus model replantOverride = function
    | Some r ->
        match replantOverride with
        | Some true -> Valid
        | Some false -> Invalid
        | None -> replantStatus model r
    | None -> Invalid

  let cropReplantStatuses model crop =
    [ if (match crop.BuySeedsOverride with
          | Some true -> true
          | Some false -> false
          | None -> model.BuySeeds) then
        match priceData model crop.PriceFrom with
        | PriceData (_, list) -> List.fold Status.compareValid Invalid (list |> List.unzip |> snd)
        | _ -> Invalid
      else
        Invalid
      cropReplantStatus model crop.ReplantOverride crop.Replant
      for KeyValue(_, item) in crop.HarvestedItems do
        cropReplantStatus model item.ReplantOverride item.Replant ]
    |> List.fold Status.compareValid Invalid

  let cropStatus model crop =
    if crop.Selected
      && crop |> isInSeason model
      && crop |> canGiveOneHarvest model then
        [ crop |> cropProductStatus model
          crop |> cropReplantStatuses model ]
        |> List.fold Status.compareWVI Invalid
    else
      Invalid

  let fertilizerStatus model (fertilizer: Fertilizer) =
    if fertilizer.Selected then
      match priceData model fertilizer.PriceFrom with
        | PriceData (_, list) -> List.fold Status.compareValid Invalid (list |> List.unzip |> snd)
        | _ -> Invalid
    else
      Invalid

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
        if model.ShowUselessCrops || (crop |> isInSeason model && crop |> canGiveOneHarvest model) then crop ]
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

  //let validCrops model =
  //  [ for KeyValue(_, crop) in model.Crops do
  //      crop ]
  //  |> L
  //  |> List.fitler (canGiveOneHarvest crop)

  open System.Collections.Generic
  let calculate model =
    let cache = Dictionary<Date, _>()
    //let crops = Model.validCrops model
    model