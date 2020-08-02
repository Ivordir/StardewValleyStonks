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
    QualitySeedMakerAmounts: Map<Quality, float> }

type DisplayPrices =
  | PriceData of (int * (NameOf<Source> * Status) list)
  | NoValidPrices
  | NoPrices

module Model =
  let validDate model = model.StartDate |> Date.isBefore model.EndDate

  let requirementStatus model = function
    | SkillLevel (skill, level) -> Requirement.status (model.Skills.[skill].Level >= level) model.SkillLevelRequirementsShould
    | Year y -> Requirement.status (model.Year >= y) model.YearRequirementsShould

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
      overallRequirementStatus model Requirement.seedMaker
    else
      Invalid

  let seedMakerStatusData status model =
    [ if not model.SeedMakerReplant then Alert.notSelected ]
    @ requirementStatusData Requirement.seedMaker status model

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

  let rec priceOf (priceFrom: Map<_,_>) model = function
    | BuyPrice p -> p.Value
    | MatchPrice m ->
        if model.MatchConditions.[m.MatchCondition].Selected then
          priceOf priceFrom model priceFrom.[m.MatchSource]
        else
          m.Value

  let priceStatuses (priceFrom: Map<_,_>) model =
    let priceStatuses = Map.map (fun source _ -> priceStatus priceFrom.[source] model) priceFrom
    let validPrices = Map.filter (fun _ status -> status <> Invalid) priceStatuses
    ( if validPrices.IsEmpty then
        priceFrom
      else
        let bestPrice =
          Map.fold (fun currentMin source _ ->
            min (priceOf priceFrom model priceFrom.[source]) currentMin)
            Integer.MaxValue
            validPrices
        Map.filter (fun _ price -> priceOf priceFrom model price = bestPrice) priceFrom)
    |> Map.fold (fun prices source price -> (price, priceStatuses.[source])::prices) []

  let priceData model (priceFrom: Map<_,_>) =
    if priceFrom.IsEmpty then
      NoPrices
    else
      let bestPrice =
        Map.fold (fun currentMin source _ ->
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

  let bestPrice (priceFrom: Map<_,_>) priceStatuses model =
    if priceFrom.IsEmpty then
      Integer.MaxValue
    elif Status.listOverallWVI (List.unzip priceStatuses |> snd) = Invalid then
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
      | PriceData (_, list) -> Status.listOverallValid (List.unzip list |> snd)
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

  let growthMultipler model crop = List.sumBy (growthMultiplierValue model) crop.GrowthMultipliers

  let fastestGrowthTime model crop = Crop.growthTimeWith (fastestFertSpeed model + growthMultipler model crop) crop

  let seasons model = Season.from model.StartDate.Season model.EndDate.Season

  let cropIsInSeason model crop = Seq.exists crop.SelectedSeasons.Contains (seasons model)

  let cropCanGiveOneHarvest model crop =
    let lastConsecDays, maxDays =
      Seq.fold (fun (consecDays, maxDays) season ->
        if crop.Seasons.Contains season then
          (consecDays + daysIn season model, maxDays)
        else
          (0, max consecDays maxDays))
        (0, 0)
        (seasons model)
    fastestGrowthTime model crop <= (max lastConsecDays maxDays) - 1

  let productStatus model source product =
    match Product.sourceOverride product with
    | Some true -> Valid
    | Some false -> Invalid
    | None -> processorStatus model source

  let productStatuses model = Status.mapOverallValid (productStatus model)

  let useSeedMakerStatus model = function
    | Some true -> Valid
    | Some false -> Invalid
    | None -> seedMakerStatus model.SellSeedsFromSeedMaker model

  let rawItemStatus model = function
    | Some true -> Valid
    | Some false -> Invalid
    | None -> if model.SellRawCrop then Valid else Invalid

  let cropProductStatus model crop =
    [ for KeyValue(_, item) in crop.HarvestedItems do
        rawItemStatus model item.SellRawItemOverride
        productStatuses model item.Products
      match crop.SeedMaker with
      | Some s -> useSeedMakerStatus model s.SellSeedsOverride
      | None -> Invalid ]
    |> Status.listOverallValid

  let cropReplantStatuses model crop =
    [ if (match crop.BuySeedsOverride with
          | Some true -> true
          | Some false -> false
          | None -> model.BuySeeds) then
        match priceData model crop.PriceFrom with
        | PriceData (_, list) -> Status.listOverallValid (List.unzip list |> snd)
        | _ -> Invalid
      else
        Invalid
      match crop.SeedMaker with
      | Some s -> useSeedMakerStatus model s.SellSeedsOverride
      | None -> Invalid
      if crop.HarvestedItems.ContainsKey (Item.nameOf crop.Seed) then
        match crop.SeedOrCropOverride with
        | Some true -> Valid
        | Some false -> Invalid
        | None -> if model.SeedOrCropReplant then Valid else Invalid ]
    |> Status.listOverallValid

  let cropStatus model crop =
    if crop.Selected
      && cropIsInSeason model crop
      && cropCanGiveOneHarvest model crop then
        [ cropProductStatus model crop
          cropReplantStatuses model crop ]
        |> Status.listOverallInvalid
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
      match model.FertilizerSort with
      | FertilizerSort.ByName -> list
      | FertilizerSort.Selected -> (sortMode model.FertilizerSortAscending) Fertilizer.selected list
      | Quality -> (sortMode model.FertilizerSortAscending) Fertilizer.quality list
      | Speed -> (sortMode model.FertilizerSortAscending) Fertilizer.speed list
      | Price -> (sortMode model.FertilizerSortAscending) (fun f -> bestPrice2 f.PriceFrom model) list)
  
  let sortedCrops model =
    [ for KeyValue(_, crop) in model.Crops do
        if model.ShowUselessCrops || (cropIsInSeason model crop && cropCanGiveOneHarvest model crop) then crop ]
    |> List.sortBy Crop.name
    |> List.sortBy Crop.seasons
    |> (fun list ->
      match model.CropSort with
      | CropSort.ByName -> (sortMode model.CropSortAscending) Crop.name list
      | CropSort.Selected -> (sortMode model.CropSortAscending) Crop.selected list
      | Seasons -> list
      | TotalGrowthTime -> (sortMode model.CropSortAscending) Crop.totalGrowthTime list
      | RegrowTime -> (sortMode model.CropSortAscending) Crop.regrowTime list
      | SeedPrice -> (sortMode model.CropSortAscending) (fun c -> bestPrice2 c.PriceFrom model) list)

  let doubleCropProb model = 0.0001 + float model.LuckBuff / 1500.0 + (if model.SpecialCharm then 0.025 else 0.0)

  let noGiantCropProb model = (1.0 - model.BaseGiantCropChance) ** model.GiantCropChecksPerTile

  let amountValue model = function
    | CropAmount (extra, chance) -> 1.0, extra + (if chance then extra * doubleCropProb model + doubleCropProb model else 0.0)
    | GiantCrop ->
        let noGiantCrop = noGiantCropProb model
        noGiantCrop, noGiantCrop * doubleCropProb model + (1.0 - noGiantCrop) * 2.0
    | Amount x -> 0.0, x

  let cachePrices = function
    | PriceData (price, sources) -> price, List.unzip sources |> fst
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
      List.fold (fun a product ->
        let score = compareProduct model item seed product
        if score > a then
          score
        else
          a)
        0.0
        validProducts
    List.fold (fun list product ->
      if compareProduct model item seed product = bestProduct then
        product::list
      else
        list)
      []
      validProducts

  open System.Collections.Generic
  let calculate model =
    let cache = Dictionary<Date, _>()
    let fertilizers =
      [ for KeyValue(_, fert) in model.Fertilizers do
          if fertilizerStatus model fert <> Invalid then
            let price, sources = priceData model fert.PriceFrom |> cachePrices
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
                | PriceData (_, list) -> List.unzip list |> fst
                | _ -> List.empty
              HarvestedItemCache = Map.empty
              BestReplants = List.empty } ]
    model

  let initial =
    { Page = Home
      StartDate =
        { Season = Spring
          Day = 1 }
      EndDate =
        { Season = Fall
          Day = 28 }
      Year = 1
      SidebarTab = Skills
      SidebarOpen = false
      Skills = Skill.all |> listToMap Skill.nameOf
      SkillList = Skill.all |> List.map Skill.nameOf
      IgnoreProfessionRelationships = false
      Multipliers = Multiplier.all |> listToMap Multiplier.nameOf
      RawMultipliers =
        Multiplier.all
        |> List.filter Multiplier.isRawMultiplier
        |> List.map Multiplier.nameOf
      BuySourceList = Source.all |> List.map Source.nameOf
      BuySources = Source.all |> listToMap Source.nameOf
      MatchConditions = MatchCondition.all |> listToMap MatchCondition.nameOf
      MatchConditionList = MatchCondition.all |> List.map MatchCondition.nameOf
      Processors = Processor.all |> listToMap Processor.nameOf
      ProcessorList = Processor.all |> List.map Processor.nameOf
      SellRawCrop = true
      SellSeedsFromSeedMaker = true
      BuySeeds = true
      SeedMakerReplant = true
      SeedOrCropReplant = true
      Crops = Crop.all |> listToMap Crop.nameOf
      CropSort = Seasons
      CropSortAscending = true
      ShowUselessCrops = false
      Fertilizers = Fertilizer.all |> listToMap Fertilizer.nameOf
      FertilizerSort = Speed
      FertilizerSortAscending = true
      FertilizerList = Fertilizer.all |> List.map Fertilizer.nameOf
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
          [ Normal, 2.0
            Silver, 3.0
            Gold, 4.0
            Iridium, 5.0 ] }