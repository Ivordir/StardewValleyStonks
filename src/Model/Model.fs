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

  let requirementsStatus model = Status.allValidWith (requirementStatus model)

  let requirementAlerts model =
    List.fold (fun (warnings, errors) requirement ->
      match requirementStatus model requirement with
      | Valid -> (warnings, errors)
      | Warning -> (UnmetRequirement requirement::warnings, errors)
      | Invalid -> (warnings, UnmetRequirement requirement::errors))
      ([], [])

  let sourceActive source model = model.BuySources.[source].Selected

  let sourceAlert source model = [ if not model.BuySources.[source].Selected then Alert.notSelected ]

  let processorStatus model processor =
    if model.Processors.[processor].Selected then
      requirementsStatus model model.Processors.[processor].Requirements
    else
      Invalid

  let processorAlert model processor =
    if model.Processors.[processor].Selected then
      requirementAlerts model model.Processors.[processor].Requirements
    else
      [], []

  let seedMakerStatus selected model =
    if selected then
      requirementsStatus model Requirement.seedMaker
    else
      Invalid

  let seedMakerAlert selected model =
    if selected then
      requirementAlerts model Requirement.seedMaker
    else
      [], []

  let priceStatus price model =
    match Buy.overrideSource price with
    | Some true -> Valid
    | Some false -> Invalid
    | None ->
        if sourceActive (Buy.source price) model then
          requirementsStatus model (Buy.requirements price)
        else
          Invalid

  let priceAlert price model =
    match Buy.overrideSource price with
    | Some true -> [], []
    | Some false -> [], [ Alert.overridden ]
    | None ->
        let warnings, errors = requirementAlerts model (Buy.requirements price)
        warnings, sourceAlert (Buy.source price) model @ errors

  let rec priceValue (priceFrom: Map<_,_>) model = function
    | BuyPrice p -> p.Value
    | MatchPrice m ->
        if model.MatchConditions.[m.MatchCondition].Selected then
          priceValue priceFrom model priceFrom.[m.MatchPrice]
        else
          m.BasePrice.Value

  let priceStatuses (priceFrom: Map<_,_>) model =
    let priceStatuses = Map.map (fun source _ -> priceStatus priceFrom.[source] model) priceFrom
    let validPrices = Map.filter (fun _ status -> status <> Invalid) priceStatuses
    ( if validPrices.IsEmpty then
        priceFrom
      else
        let bestPrice =
          Map.fold (fun currentMin source _ ->
            min (priceValue priceFrom model priceFrom.[source]) currentMin)
            Integer.MaxValue
            validPrices
        Map.filter (fun _ price -> priceValue priceFrom model price = bestPrice) priceFrom)
    |> Map.fold (fun prices source price -> (price, priceStatuses.[source])::prices) []

  let priceData model (priceFrom: Map<_,_>) =
    if priceFrom.IsEmpty then
      NoPrices
    else
      let bestPrice =
        Map.fold (fun currentMin source _ ->
          if sourceActive source model then
            min (priceValue priceFrom model priceFrom.[source]) currentMin
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
                if status <> Invalid && priceValue priceFrom model price = bestPrice then source, status ] )

  let bestPrice (priceFrom: Map<_,_>) priceStatuses model =
    if priceFrom.IsEmpty then
      Integer.MaxValue
    elif Status.warningPrecedence (List.unzip priceStatuses |> snd) = Invalid then
      Integer.MaxValue - 1
    else
      priceValue priceFrom model (fst priceStatuses.Head)

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
      | PriceData (_, list) -> Status.oneValid (List.unzip list |> snd)
      | _ -> Invalid
    else
      Invalid

  let fastestFertSpeed model =
    model.Fertilizers
    |> Map.filter (fun _ fert -> fertilizerStatus model fert <> Invalid)
    |> Map.fold (fun speed _ fert -> max speed fert.Speed) 0.0

  let multiplierValue defaultValue model multiplier =
    match model.Multipliers.[multiplier] with
    | RawMultiplier m -> if m.Selected then m.Value else defaultValue
    | Profession p ->
        let skill = model.Skills.[p.Skill]
        if skill.Professions.[p.Profession].Selected && (p.Profession |> Profession.isUnlocked skill || model.SkillLevelRequirementsShould <> Invalidate) then
          p.Value
        else
          defaultValue

  let growthMultiplierValue = multiplierValue 0.0
  let priceMultiplierValue = multiplierValue 1.0

  let growthMultipler model crop =
    Set.fold (fun sum multiplier ->
      sum + growthMultiplierValue model multiplier)
      0.0
      (Crop.growthMultipliers crop)

  let fastestGrowthTime model crop = Crop.growthTimeWith (fastestFertSpeed model + growthMultipler model crop) crop

  let seasons model = Season.from model.StartDate.Season model.EndDate.Season

  let cropIsInSeason model crop = Seq.exists (Crop.selectedSeasons crop).Contains (seasons model)

  let cropCanGiveOneHarvest model crop =
    let lastConsecDays, maxDays =
      Seq.fold (fun (consecDays, maxDays) season ->
        if (Crop.selectedSeasons crop).Contains season then
          (consecDays + daysIn season model, maxDays)
        else
          (0, max consecDays maxDays))
        (0, 0)
        (seasons model)
    fastestGrowthTime model crop <= (max lastConsecDays maxDays) - 1

  let productStatus model source product = Status.ofOverride (processorStatus model source) (Product.processorOverride product)

  let harvestedCropStatus model harvestedCrop =
    [ Status.ofOverrideBool model.SellRawCrop harvestedCrop.SellRawItemOverride
      for KeyValue(source, product) in harvestedCrop.Products do
        productStatus model source product ]
    |> Status.oneValid

  let useSeedMakerStatus model selected = Status.ofOverride (seedMakerStatus selected model)

  let cropProductStatus model crop =
    match crop with
    | RegularCrop c ->
        [ harvestedCropStatus model c.Crop
          match c.HarvestedItem with
          | Some (_, item) -> harvestedCropStatus model item
          | None -> Invalid ]
    | GiantCrop g ->
        [ harvestedCropStatus model g.Crop
          match g.Base.SeedMaker with
          | Some s -> useSeedMakerStatus model model.SellSeedsFromSeedMaker s.SellSeedsOverride
          | None -> Invalid ]
    | ForageCrop _ -> [ Valid ]
    |> Status.oneValid

  let cropReplantStatuses model crop =
    [ match Crop.rawCropReplantOverride crop with
      | Some o -> Status.ofOverrideBool model.SeedOrCropReplant o
      | None -> Invalid
      if defaultArg (Crop.buySeedsOverride crop) model.BuySeeds then
        match priceData model (Crop.priceFrom crop) with
        | PriceData (_, list) -> Status.oneValid (List.unzip list |> snd)
        | _ -> Invalid
      match Crop.seedMaker crop with
      | Some s -> useSeedMakerStatus model model.SeedMakerReplant s.ReplantOverride
      | None -> Invalid ]
    |> Status.oneValid

  let cropStatus model crop =
    if Crop.selected crop
      && cropIsInSeason model crop
      && cropCanGiveOneHarvest model crop then
        [ cropProductStatus model crop
          cropReplantStatuses model crop ]
        |> Status.allValid
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
      | SeedPrice -> (sortMode model.CropSortAscending) (fun c -> bestPrice2 (Crop.priceFrom c) model) list)

  let doubleCropProb model = 0.0001 + float model.LuckBuff / 1500.0 + (if model.SpecialCharm then 0.025 else 0.0)

  let noGiantCropProb model = (1.0 - model.BaseGiantCropChance) ** model.GiantCropChecksPerTile

  let cachePrices = function
    | PriceData (price, sources) -> Some price, List.unzip sources |> fst |> Set.ofList
    | _ -> None, Set.empty

  let itemPrice model item =
    match item.Multiplier with
    | Some m -> priceMultiplierValue model m * float item.BasePrice |> int
    | None -> item.BasePrice

  let productProfit model = function
    | FromProcess p -> itemPrice model p.Output |> float
    | RatioProcess r -> float (itemPrice model r.Process.Output) * r.OutputAmount / float r.InputAmount

  //let seedMakerProfit model seed =

  let bestProducts model seedMaker (seed: Item) (harvestedCrop: HarvestedCrop) =
    [ for KeyValue(source, product) in harvestedCrop.Products do
        if productStatus model source product <> Invalid then
          product ]
    |> List.fold (fun (maxProfit, bestProducts) product ->
      let profit = productProfit model product
      match compare profit maxProfit with
      | 1 -> profit, [ Product.nameOf product ]
      | 0 -> profit, (Product.nameOf product)::bestProducts
      | _ -> maxProfit, bestProducts)
      (0.0, [ Item.nameOf harvestedCrop.Crop ] )
    |> (fun (prof, prods) ->
      match seedMaker with
      | Some x ->
          if useSeedMakerStatus model model.SellSeedsFromSeedMaker x.SellSeedsOverride <> Invalid then
            match compare prof (float seed.BasePrice) with
            | 1 -> [ Item.nameOf seed ]
            | 0 -> (Item.nameOf seed)::prods
            | _ -> prods
          else
            prods
      | None -> prods)

  open System.Collections.Generic
  let calculate model =
    let cache = Dictionary<Date, _>()
    let fertilizers =
      [ for KeyValue(_, fert) in model.Fertilizers do
          if fertilizerStatus model fert <> Invalid then
            let price, sources = priceData model fert.PriceFrom |> cachePrices
            { Fertilizer = fert
              Price = Option.get price
              Sources = sources } ]
      // |> List.fold (fun prev tail ->
      //   match Fertilizer.compareCache )
      //   []
    // let crops =
    //   [ for KeyValue(_, crop) in model.Crops do
    //       if cropStatus model crop <> Invalid then
    //        let temp = priceData model crop.PriceFrom
    //         { Crop = crop
    //           GrowthMultiplier = growthMultipler model crop
    //           SeedPrice =
    //             match temp with
    //             | PriceData (price, _) -> Some price
    //             | _ -> None
    //           SeedSources =
    //             match temp with
    //             | PriceData (_, list) -> List.unzip list |> fst
    //             | _ -> List.empty
    //           HarvestedItemCache = Map.empty
    //           BestReplants = List.empty } ]
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