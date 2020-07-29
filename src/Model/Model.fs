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

module SidebarTab =
  let all =
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
    IgnoreProfessionRelationships: bool
    Multipliers: Map<NameOf<Multiplier>, Multiplier>
    RawMultipliers: NameOf<Multiplier> list
    BuySources: Map<NameOf<Source>, Source>
    BuySourceList: NameOf<Source> list //lists are necessary to enforce/preserve order, haven't thought of another way
    MatchConditions: Map<NameOf<MatchCondition>, MatchCondition>
    MatchConditionList: NameOf<MatchCondition> list
    Processors: Map<NameOf<Processor>, Processor>
    ProcessorList: NameOf<Processor> list
    SellRawCrop: bool
    SellSeedsFromSeedMaker: bool
    BuySeeds: bool
    SeedMakerReplant: bool
    SeedOrCropReplant: bool
    Crops: Map<NameOf<Crop>, Crop>
    CropSort: CropSort
    CropSortAscending: bool
    ShowUselessCrops: bool
    Fertilizers: Map<NameOf<Fertilizer>, Fertilizer>
    FertilizerSort: FertilizerSort
    FertilizerSortAscending: bool
    FertilizerList: NameOf<Fertilizer> list
    StartingFertilizer: NameOf<Fertilizer> option
    YearRequirementsShould: RequirementsShould
    SkillLevelRequirementsShould: RequirementsShould
    SpecialCharm: bool
    LuckBuff: int
    BaseGiantCropChance: float
    GiantCropChecksPerTile: float
    SeedMakerProb: float
    AncientSeedProb: float
    GreenhouseMode: bool
    QualityProducts: bool
    QualitySeedMaker: bool
    QualitySeedMakerAmounts: Map<Quality, int> }

type DisplayPrices =
  | PriceData of (int * (NameOf<Source> * Status) list)
  | NoValidPrices
  | NoPrices

module Model =
  let seedMakerRequirements = [ SkillLevel (Name "Farming", 9) ]

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
  
  let overallRequirementStatus model = Status.listOverallInvalidWith (requirementStatus model)

  let sourceActive source model = model.BuySources.[source].Selected
  
  let sourceStatusData source model = [ if not model.BuySources.[source].Selected then Alert.notSelected ]

  let processorStatus model processor =
    if model.Processors.[processor].Selected then
      overallRequirementStatus model model.Processors.[processor].Requirements
    else
      Invalid
  
  let processorStatusData overallStatus model processor =
    [ if not model.Processors.[processor].Selected then Alert.notSelected ]
    @ requirementStatusData model.Processors.[processor].Requirements overallStatus model
  
  let seedMakerStatus sellOrReplant model =
    if sellOrReplant then
      overallRequirementStatus model seedMakerRequirements
    else
      Invalid
  
  let seedMakerStatusData status model =
    [ if not model.SeedMakerReplant then Alert.notSelected ]
    @ requirementStatusData seedMakerRequirements status model
  
  let priceStatus price model =
    match Price.overrideSource price with
    | Some true -> Valid
    | Some false -> Invalid
    | None ->
        if sourceActive (Price.source price) model then
          overallRequirementStatus model (Price.requirements price)
        else
          Invalid
  
  let priceStatusData (price, status) model =
    match Price.overrideSource price with
    | Some true -> []
    | Some false -> [ Alert.overridden ]
    | None ->
      sourceStatusData (Price.source price) model
      @ requirementStatusData (Price.requirements price) status model
  
  let rec priceOf (priceFrom: Map<NameOf<Source>, Price>) model = function
    | BuyPrice p -> p.Value
    | MatchPrice m ->
        if model.MatchConditions.[m.MatchCondition].Selected then
          priceOf priceFrom model priceFrom.[m.MatchSource]
        else
          m.Value
  
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
    elif Status.listOverallWVI (priceStatuses |> List.unzip |> snd) = Invalid then
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

  let fertilizerStatus model (fertilizer: Fertilizer) =
    if fertilizer.Selected then
      match priceData model fertilizer.PriceFrom with
        | PriceData (_, list) -> Status.listOverallValid (list |> List.unzip |> snd)
        | _ -> Invalid
    else
      Invalid

  let fastestFertSpeed model =
    model.Fertilizers
    |> Map.filter (fun _ fert -> fert.Selected && fertilizerStatus model fert <> Invalid)
    |> Map.fold (fun speed _ fert -> max speed fert.Speed) 0.0

  let multiplierValue defaultValue model multiplier =
    match model.Multipliers.[multiplier] with
    | Multiplier m -> if m.Selected then m.Value else defaultValue
    | Profession p ->
        let skill = model.Skills.[p.Skill]
        if skill.Professions.[p.Profession].Selected && (p.Profession |> Profession.isUnlocked skill || model.SkillLevelRequirementsShould <> Invalidate) then
          p.Value
        else
          defaultValue

  let growthMultiplierValue = multiplierValue 0.0
  let priceMultiplierValue = multiplierValue 1.0
  
  let growthMultipler model crop =
    List.sumBy (growthMultiplierValue model) crop.GrowthMultipliers

  let fastestGrowthTime model crop =
    Crop.growthTimeWith (fastestFertSpeed model + growthMultipler model crop) crop

  let seasons model = Season.from model.StartDate.Season model.EndDate.Season

  let isInSeason model crop = Seq.exists crop.SelectedSeasons.Contains (seasons model)

  let canGiveOneHarvest model crop =
    let lastConsecDays, maxDays =
      seasons model
      |> Seq.fold (fun (consecDays, maxDays) season ->
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
    | None -> processorStatus model source

  let productStatuses model = Status.mapOverallValid (productStatus model)

  let sellSeedsFromSeedMakerStatus model = function
    | SeedMaker (o, _) ->
        match o with
        | Some true -> Valid
        | Some false -> Invalid
        | None -> seedMakerStatus model.SellSeedsFromSeedMaker model
    | _ -> Invalid

  let rawItemStatus model = function
    | Some true -> Valid
    | Some false -> Invalid
    | None -> if model.SellRawCrop then Valid else Invalid

  let cropProductStatus model crop =
    [ rawItemStatus model crop.SellRawCropOverride
      productStatuses model crop.Products
      for KeyValue(_, item) in crop.OtherHarvestedItems do
        rawItemStatus model item.SellRawItemOverride
        productStatuses model item.Products
        sellSeedsFromSeedMakerStatus model item.Replant
      sellSeedsFromSeedMakerStatus model crop.Replant ]
    |> Status.listOverallValid

  let replantStatus model = function
    | SeedMaker (_, o) ->
        match o with
        | Some true -> Valid
        | Some false -> Invalid
        | None -> seedMakerStatus model.SeedMakerReplant model
    | SeedOrCrop o ->
        match o with
        | Some true -> Valid
        | Some false -> Invalid
        | None -> if model.SeedOrCropReplant then Valid else Invalid
    | NoReplant -> Invalid

  let cropReplantStatuses model crop =
    [ if (match crop.BuySeedsOverride with
          | Some true -> true
          | Some false -> false
          | None -> model.BuySeeds) then
        match priceData model crop.PriceFrom with
        | PriceData (_, list) -> Status.listOverallValid (list |> List.unzip |> snd)
        | _ -> Invalid
      else
        Invalid
      replantStatus model crop.Replant
      for KeyValue(_, item) in crop.OtherHarvestedItems do
        replantStatus model item.Replant ]
    |> Status.listOverallValid

  let cropStatus model crop =
    if crop.Selected
      && crop |> isInSeason model
      && crop |> canGiveOneHarvest model then
        [ crop |> cropProductStatus model
          crop |> cropReplantStatuses model ]
        |> Status.listOverallWVI
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
  
  let cachePrices = function
    | PriceData (price, sources) -> price, sources |> List.unzip |> fst
    | _ -> invalidArg "displayPrices" "Plz no"

  let itemPrice model item =
    match item.Multiplier with
    | Some m -> priceMultiplierValue model m * float item.BasePrice |> int
    | None -> item.BasePrice

  let compareProduct model item seed = function
    | Process p -> itemPrice model p.Output |> float
    | RatioProcess r -> float (itemPrice model r.Output) * r.OutputAmount / float r.InputAmount

  let compareProducts model item seed a b =
    let scoreA = compareProduct model item seed a
    let scoreB = compareProduct model item seed b
    if scoreA > scoreB then
      scoreA
    else
      scoreB

  let bestProducts model item seed products =
    let validProducts =
      [ for KeyValue(source, product) in products do
          if productStatus model source product <> Invalid then
            product ]
    let bestProduct =
      List.fold
        (fun a product ->
          let score = compareProduct model item seed product
          if score > a then
            score
          else
            a)
        0.0
        validProducts
    List.fold (fun list product -> if compareProduct model item seed product = bestProduct then product::list else list) [] validProducts

  open System.Collections.Generic
  let calculate model =
    let cache = Dictionary<Date, _>()
    let fertilizers =
      [ for KeyValue(_, fert) in model.Fertilizers do
          if fertilizerStatus model fert <> Invalid then
            let price, sources = fert.PriceFrom |> priceData model |> cachePrices 
            { Fertilizer = fert
              Price = price
              Sources = sources } ]
    let crops =
      [ for KeyValue(_, crop) in model.Crops do
          if cropStatus model crop <> Invalid then
            let temp = priceData model crop.PriceFrom
            { Crop = crop
              GrowthMultiplier = growthMultipler model crop
              BestProducts = Map.empty
              SeedPrice =
                match temp with
                | PriceData (price, _) -> Some price
                | _ -> None
              SeedSources =
                match temp with
                | PriceData (_, list) -> list |> List.unzip |> fst
                | _ -> List.empty
              HarvestedItems = Map.empty
              BestReplants = List.empty } ]
    model