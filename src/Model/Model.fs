namespace StardewValleyStonks

open Types

type Mode =
  | Compare
  | Plan
  | Find

module Mode =
  let tryParseUrl str =
    match str with
    | "compare" -> Ok Compare
    | "plan" -> Ok Plan
    | "find" -> Ok Find
    | _ -> Error <| sprintf "'%s' is not a Mode." str
  
  open Elmish.UrlParser
  let parseUrl state = custom "Mode" tryParseUrl state

  let all =
    [ Compare
      Plan
      Find ]


type Page =
  | Home
  | Mode of Mode
  | Help

module Page =
  let url = function
    | Home -> ""
    | Mode mode -> "#" + (string mode).ToLower()
    | Help -> "#help"

  open Elmish.UrlParser

  let parseUrl: Parser<_, Page> =
    oneOf
      [ map Home top
        map Mode Mode.parseUrl
        map Help (s "help") ]

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

[<Fable.Core.StringEnum>]
type CompareMode =
  | [<CompiledName("Combinations")>] Combos
  | [<CompiledName("Crops")>] CompareCrops
  | [<CompiledName("Fertilizers")>] CompareFertilizers

module CompareMode =
  let all =
    [ Combos
      CompareCrops
      CompareFertilizers ]

[<Fable.Core.StringEnum>]
type ProfitMode =
  | [<CompiledName("Net Profit")>] NetProfit
  | [<CompiledName("Profit Margin")>] NetProfitMargin
  | [<CompiledName("Return On Investment")>] ROI

module ProfitMode =
  let all =
    [ NetProfit
      NetProfitMargin
      ROI ]

type Model =
  { // Display
    Page: Page
    SidebarTab: SidebarTab
    SidebarOpen: bool

    // Data
    Skills: Map<NameOf<Skill>, Skill>
    SkillList: NameOf<Skill> list
    IgnoreProfessionRelationships: bool

    BuySources: Map<NameOf<Source>, Source>
    BuySourceList: NameOf<Source> list //lists are necessary to enforce a display order, haven't thought of another way

    MatchConditions: Map<NameOf<MatchCondition>, MatchCondition>
    MatchConditionList: NameOf<MatchCondition> list

    Processors: Map<NameOf<Processor>, Processor>
    ProcessorList: NameOf<Processor> list

    SellRawCrop: bool
    SellSeedsFromSeedMaker: bool

    BuySeeds: bool
    SeedMakerReplant: bool
    HarvestReplant: bool

    Crops: Map<NameOf<Crop>, Crop>
    CropSort: CropSort
    CropSortAscending: bool
    CropList: NameOf<Crop> list
    SelectedCrop: NameOf<Crop> option
    ShowOutOfSeasonCrops: bool
    AllowCropClearings: bool
    AllowCrossSeason: bool
    AccountForReplant: bool

    Fertilizers: Map<NameOf<Fertilizer>, Fertilizer>
    FertilizerSort: FertilizerSort
    FertilizerSortAscending: bool
    FertilizerList: NameOf<Fertilizer> list
    SelectedFertilizer: NameOf<Fertilizer> option
    AccountForFertilizerCost: bool
    FertilizerLossProb: float
  
    Multipliers: Map<NameOf<Multiplier>, Multiplier>
    RawMultipliers: NameOf<Multiplier> list

    // Date
    StartDate: Date
    EndDate: Date
    Year: int

    // Compare Mode
    CompareMode: CompareMode
    ShowUnprofitableCombos: bool
    SelectedCombo: (NameOf<Crop> * NameOf<Fertilizer> option) option

    SelectedCompareCrop: NameOf<Crop> option
    CompareCropsUsingFertilizer: NameOf<Fertilizer> option

    SelectedCompareFertilizer: NameOf<Fertilizer> option option
    CompareFertilizersUsingCrop: NameOf<Crop>

    // Plan Mode

    // Find Mode
    StartingFertilizer: NameOf<Fertilizer> option


    // Settings
    ProfitMode:ProfitMode
    Greenhouse: bool

    ShowTips: bool
    SaveSettings: bool

    SkillLevelRequirementsShould: RequirementsShould
    YearRequirementsShould: RequirementsShould

    SpecialCharm: bool
    LuckBuff: int

    TrelisPenalty: bool
    TrelisPercentage: float
    AllowTrelisPair: bool

    BaseGiantCropChance: float
    GiantCropChecksPerTile: float

    SeedMakerProb: float
    AncientSeedProb: float

    QualityProducts: bool
    QualitySeedMaker: bool
    QualitySeedMakerAmounts: Map<Quality, float> }


module Model =
  let requirementMet model = function
    | SkillLevel (skill, level) -> model.SkillLevelRequirementsShould <> Require || model.Skills.[skill].Level >= level
    | Year y -> model.YearRequirementsShould <> Require || model.Year >= y

  let requirementsMet model = List.forall (requirementMet model)

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


  let sourceActive model source = model.BuySources.[source].Selected

  let sourceAlert model source = [ if not model.BuySources.[source].Selected then Alert.notSelected ]


  let active model selected requirements = selected && requirementsMet model requirements


  let processorActive model processor = active model model.Processors.[processor].Selected model.Processors.[processor].Requirements

  let processorStatus model processor =
    if model.Processors.[processor].Selected
    then requirementsStatus model model.Processors.[processor].Requirements
    else Invalid

  let processorAlert model processor =
    if model.Processors.[processor].Selected
    then requirementAlerts model model.Processors.[processor].Requirements
    else [], []


  let seedMakerActive model selected = active model selected Requirement.seedMaker
  let sellSeedsFromSeedMakerActive model seedMaker = seedMakerActive model (defaultArg seedMaker.SellSeeds model.SellSeedsFromSeedMaker)
  let seedMakerReplantActive model seedMaker = seedMakerActive model (defaultArg seedMaker.Replant model.SeedMakerReplant)

  let seedMakerStatus model selected =
    if selected
    then requirementsStatus model Requirement.seedMaker
    else Invalid

  let seedMakerAlert model selected =
    if selected
    then requirementAlerts model Requirement.seedMaker
    else [], []


  let priceActive model source price =
    defaultArg
      (Buy.overrideSource price)
      (active
        model
        (sourceActive model source)
        (Buy.requirements price))

  let hasAPrice model = Map.exists (priceActive model)

  let cropHasAPrice model (baseCrop: BaseCrop) = defaultArg baseCrop.BuySeeds model.BuySeeds && hasAPrice model baseCrop.PriceFrom

  let priceStatus model price =
    match Buy.overrideSource price with
    | Some true -> Valid
    | Some false -> Invalid
    | None ->
        if sourceActive model (Buy.source price)
        then requirementsStatus model (Buy.requirements price)
        else Invalid

  let priceAlert model price =
    match Buy.overrideSource price with
    | Some true -> [], []
    | Some false -> [], [ Alert.overridden ]
    | None ->
        let warnings, errors = requirementAlerts model (Buy.requirements price)
        warnings, sourceAlert model (Buy.source price) @ errors

  let rec priceValue model (priceFrom: Map<_,_>) = function
    | BuyPrice p -> p.Value
    | MatchPrice m ->
        if model.MatchConditions.[m.MatchCondition].Selected
        then priceValue model priceFrom priceFrom.[m.MatchPriceFrom]
        else m.OwnPrice.Value

  type Integer = int

  let priceData model (priceFrom: Map<_,_>) =
    let bestPrice, sources =
      Map.fold (fun (currentMin, set) source price ->
        if priceActive model source price then
          let thisMin = priceValue model priceFrom priceFrom.[source]
          if thisMin = currentMin then currentMin, set |> Set.add source
          elif thisMin < currentMin then thisMin, Set.singleton source
          else currentMin, set
        else
          currentMin, set)
        (Integer.MaxValue, Set.empty)
        priceFrom
    if bestPrice = Integer.MaxValue then None else Some (bestPrice, sources)

  let bestPriceOption model priceFrom =
    let bestPrice =
      Map.fold (fun currentMin source price ->
        if priceActive model source price
        then min (priceValue model priceFrom priceFrom.[source]) currentMin
        else currentMin)
        Integer.MaxValue
        priceFrom
    if bestPrice = Integer.MaxValue then None else Some bestPrice

  let priceSort model (priceFrom: Map<_,_>) =
    if priceFrom.IsEmpty then
      Integer.MaxValue
    else
      match bestPriceOption model priceFrom with
      | Some price -> price
      | None -> Integer.MaxValue - 1


  let fertilizerActive model fertilizer =
    Fertilizer.selected fertilizer
    && hasAPrice model fertilizer.PriceFrom

  let fastestFertSpeed model =
    model.Fertilizers
    |> Map.filter (fun _ fert -> fertilizerActive model fert)
    |> Map.fold (fun speed _ fert -> max speed fert.Speed) 0.0

  let multiplierValue defaultValue model multiplier =
    match model.Multipliers.[multiplier] with
    | RawMultiplier m -> if m.Selected then m.Value else defaultValue
    | Profession p ->
        let skill = model.Skills.[p.Skill]
        if skill.Professions.[p.Profession].Selected
          && (model.SkillLevelRequirementsShould <> Require || p.Profession |> Profession.isUnlocked skill)
        then p.Value
        else defaultValue

  let growthMultiplierValue = multiplierValue 0.0
  let priceMultiplierValue = multiplierValue 1.0

  let growthMultipler model crop =
    Set.fold (fun sum multiplier ->
      sum + growthMultiplierValue model multiplier)
      0.0
      (Crop.growthMultipliers crop)

  let cropIsInSeason model = Crop.isInSeason model.StartDate model.EndDate

  let cropCanGiveOneHarvest model crop = Crop.harvestsWithin model.StartDate model.EndDate (fastestFertSpeed model + growthMultipler model crop) crop > 0

  let productStatus model source product = Status.ofOverride (processorStatus model source) (Product.processorOverride product)

  let productActive model source product = defaultArg (Product.processorOverride product) (processorActive model source)

  let harvestedCropStatus model harvestedCrop =
    [ Status.ofBoolOverride model.SellRawCrop harvestedCrop.SellRaw
      for KeyValue(source, product) in harvestedCrop.Products do
        productStatus model source product ]
    |> Status.oneValid

  let harvestCropHasOneProduct model harvestCrop =
    defaultArg harvestCrop.SellRaw model.SellRawCrop
    || Map.exists (productActive model) harvestCrop.Products

  let cropHasAProduct model = function
    | RegularCrop c ->
        harvestCropHasOneProduct model c.Crop
        || sellSeedsFromSeedMakerActive model c.SeedMaker
        || match c.HarvestItem with
           | Some h -> harvestCropHasOneProduct model h.Item
           | None -> false
    | RegrowCrop r ->
        harvestCropHasOneProduct model r.Crop
        || match r.Replant with
           | SeedMakerPlant s -> sellSeedsFromSeedMakerActive model s
           | RawPlant _ -> false
    | GiantCrop g ->
        harvestCropHasOneProduct model g.Crop
        || sellSeedsFromSeedMakerActive model g.SeedMaker
    | ForageCrop f -> false
    | Bush t -> harvestCropHasOneProduct model t.Crop

  let cropHasAReplant model = function
    | RegularCrop c ->
      cropHasAPrice model c.Base
      || seedMakerReplantActive model c.SeedMaker
      || match c.HarvestItem with
         | Some h ->
             match h.Replant with
             | Some x -> defaultArg x model.HarvestReplant
             | None -> false
         | None -> false
    | RegrowCrop r ->
        cropHasAPrice model r.Base
        || match r.Replant with
           | SeedMakerPlant s -> seedMakerReplantActive model s
           | RawPlant p -> defaultArg p model.HarvestReplant
    | GiantCrop g ->
        cropHasAPrice model g.Base
        || seedMakerReplantActive model g.SeedMaker
    | ForageCrop f -> false
    | Bush t -> cropHasAPrice model t.Base

  let cropActive model crop =
    Crop.selected crop
    && cropIsInSeason model crop
    && cropCanGiveOneHarvest model crop
    && cropHasAProduct model crop
    && cropHasAReplant model crop

  let cropAlerts model crop =
    [ if not <| Crop.selected crop then
        Alert.notSelected
      if not <| cropIsInSeason model crop then
        Message "Is not in season."
      if not <| cropCanGiveOneHarvest model crop then
        Message "Crop z cannot give at least one harvest. Using fertilizer x for the highest possible speed bonus of y, crop z needs w+1 days for a harvest, but there are only d days in season(s) s1, s2... ."
      if not <| cropHasAProduct model crop then
        AlertList ("Cannot sell any products:", [])
      if not <| cropHasAReplant model crop then
        AlertList ("Cannot be replanted:", []) ]

  let allCrops model = mapValues model.Crops
  let allFertilizers model = mapValues model.Fertilizers

  let activeCrops model = List.filter (fun c -> cropActive model model.Crops.[c]) model.CropList
  let activeFertilizers model = List.filter (fun f -> fertilizerActive model model.Fertilizers.[f]) model.FertilizerList

  let sortedFertilizers model =
    allFertilizers model
    //|> List.sortBy Fertilizer.name // already sorted by name, since the key of the map is the fertilizer's name
    |> (match model.FertilizerSort with
        | FertilizerSort.ByName -> (fun list -> if model.FertilizerSortAscending then list else List.rev list)
        | FertilizerSort.Selected -> sortMode model.FertilizerSortAscending Fertilizer.selected
        | Quality -> sortMode model.FertilizerSortAscending Fertilizer.quality
        | Speed -> sortMode model.FertilizerSortAscending Fertilizer.speed
        | Price -> sortMode model.FertilizerSortAscending (Fertilizer.priceFrom >> priceSort model))
  
  let sortedCrops model =
    [ for KeyValue(_, crop) in model.Crops do
        if model.ShowOutOfSeasonCrops || cropIsInSeason model crop then
          crop ]
    //|> List.sortBy Crop.name
    |> List.sortBy Crop.seasons
    |> ( match model.CropSort with
        | CropSort.ByName -> sortMode model.CropSortAscending Crop.name
        | CropSort.Selected -> sortMode model.CropSortAscending Crop.selected
        | Seasons -> (fun list -> if model.CropSortAscending then list else List.rev list)
        | TotalGrowthTime -> sortMode model.CropSortAscending Crop.totalGrowthTime
        | RegrowTime -> sortMode model.CropSortAscending Crop.regrowTime
        | SeedPrice -> sortMode model.CropSortAscending (Crop.priceFrom >>priceSort model))

  let doubleCropProb model = 0.0001 + float model.LuckBuff / 1500.0 + if model.SpecialCharm then 0.025 else 0.0

  let noGiantCropProb model = (1.0 - model.BaseGiantCropChance) ** model.GiantCropChecksPerTile

  let itemPrice model item =
    match item.Multiplier with
    | Some m -> priceMultiplierValue model m |> applyTo item.BasePrice
    | None -> item.BasePrice

  let qualityPrice model quality item = Quality.multiplier quality |> applyTo (itemPrice model item)

  let processorPreservesQuality model processor =
    model.QualityProducts
    && model.Processors.[processor].PreservesQuality

  let qualityProcess model quality ``process`` =
    if processorPreservesQuality model ``process``.Processor
    then qualityPrice model quality ``process``.Output
    else itemPrice model ``process``.Output

  let productProfit model quality = function
    | FromProcess p -> qualityProcess model quality p |> float
    | RatioProcess r -> float (qualityProcess model quality r.Process) * r.OutputAmount / float r.InputAmount

  let seedMakerAmount model quality (seed: Item) =
    model.SeedMakerProb
    * if model.QualitySeedMaker then model.QualitySeedMakerAmounts.[quality] else 2.0
    + if seed.Name = "Ancient Seeds" then model.AncientSeedProb else 0.0

  let seedMakerProfit model quality seed = seedMakerAmount model quality seed * (itemPrice model seed |> float)

  let qualitySellPrice model quality = function
    | RawCrop item -> qualityPrice model quality item |> float
    | Product product -> productProfit model quality product
    | SeedsFromSeedMaker seed -> seedMakerProfit model quality seed


  let activeProducts model seedMakerData harvestCrop =
    [ if defaultArg harvestCrop.SellRaw model.SellRawCrop then
        RawCrop harvestCrop.Item
      for KeyValue(source, product) in harvestCrop.Products do
        if productActive model source product then
          Product product
      if Option.isSome seedMakerData then
        let seed, seedMaker = Option.get seedMakerData
        if sellSeedsFromSeedMakerActive model seedMaker then
          SeedsFromSeedMaker seed ]

  let bestSellPrice model quality =
    List.fold (fun sell product ->
      max sell (qualitySellPrice model quality product))
      0.0

  let bestSellOne model quality harvestCrop = bestSellPrice model quality (activeProducts model None harvestCrop)

  let bestSellWith qualities model seedMakerData harvestCrop =
    let products = activeProducts model seedMakerData harvestCrop
    qualities
    |> List.map (fun quality -> quality, bestSellPrice model quality products)
    |> Map.ofList

  let bestSell = bestSellWith Quality.common
  let bestSellNormal = bestSellWith [ Normal ]
  let bestSellIridium = bestSellWith [ Iridium ]
  let bestSellAll = bestSellWith Quality.all

  let bestProductsWith qualities model seedMakerData harvestCrop =
    let products = activeProducts model seedMakerData harvestCrop
    qualities
    |> List.map (fun quality -> quality, allMaxsSet (qualitySellPrice model quality) products)
    |> Map.ofList

  let bestProducts = bestProductsWith Quality.common
  let bestProductsAll = bestProductsWith Quality.all

  let validFertilizers model =
    [ for KeyValue(_, fertilizer) in model.Fertilizers do
        if fertilizerActive model fertilizer then
          fertilizer ]

  let cacheFertilizers model =
    List.map (fun (fert: Fertilizer) ->
      let data = Option.get <| priceData model fert.PriceFrom
      { Fertilizer = fert
        Price = fst data
        Sources = snd data } )

  let fertilizerPrices model = listToMapByValue (Fertilizer.priceFrom >> priceData model >> Option.get >> fst)

  open System.Collections.Generic
  let calculate model =
    let cache = Dictionary<Date * Fertilizer, _>()

    let validFerts = cacheFertilizers model (validFertilizers model)

    let compareFertilizer fert (better, equal) other =
      match CacheFertilizer.compare model.AccountForFertilizerCost fert other with
      | Some Better -> better |> tryAddToSet other fert, equal
      | Some Equal -> better, equal |> tryAddToSet fert other
      | Some Worse -> better |> tryAddToSet fert other, equal
      | None -> better, equal

    let compareFertilizers =
      let rec helper (better, equal) = function
        | [] -> better, equal
        | fert::tail -> helper (List.fold (compareFertilizer fert) (better, equal) tail) tail
      helper (Map.empty, Map.empty)

    let betterFert, equalFert = compareFertilizers <| validFerts
 
    let fertilizers = validFerts |> List.filter (fun x -> not (betterFert.ContainsKey x || equalFert.ContainsKey x))

    // let validCrops =
    //   [ for KeyValue(_, crop) in model.Crops do
    //       if cropStatus model crop <> Invalid then
    //         let cacheBase model c =
    //           let price, sources = priceData model (Crop.priceFrom c) |> cachePrices
    //           { BaseCrop = Crop.baseCrop c
    //             GrowthMultiplier = growthMultipler model c
    //             SeedPrice = defaultArg price -1
    //             SeedSources = sources }
    //         match crop with
    //         | RegularCrop c ->
    //           RegularCache
    //             {| Base = cacheBase model crop
    //                CropSell = bestSell model c.Base c.Crop
    //                HarvestedItemSell =
    //                  match c.HarvestedItem with
    //                  | Some (_, item) -> bestProducts model c.Base Normal item
    //                  | None -> Set.empty
    //                Replant = List.empty |}
    //         | GiantCrop g ->
    //           GiantCache
    //             {| Base = cacheBase model crop
    //                CropSell = bestSell model g.Base g.Crop
    //                Replant = List.empty |}
    //         | ForageCrop f ->
    //           ForageCache
    //             {| Base = cacheBase model crop
    //                ForageSell =
    //                  Map.map (fun _ item ->
    //                    bestSell model f.Base item)
    //                    f.Crops
    //                SellForageSeeds = false
    //                ForageReplant = false
    //                Replant = List.empty |}
    //         | Tea t ->
    //           TeaCache
    //             {| Base = cacheBase model crop
    //                TeaSell = bestProducts model t.Base Normal t.TeaLeaves |} ]
    model

  let singleAmount quality amount : Map<Quality, float> = Map.ofList [ quality, amount ]

  let qualityDistribution fertQuality farmBuffLevel =
    let gold = 0.01 + 0.2 * (float farmBuffLevel / 10.0 + float fertQuality * float (farmBuffLevel + 2) / 12.0)
    let silver = (min (2.0 * gold) 0.75) * (1.0 - gold)
    let normal = 1.0 - silver - gold
    [ Normal, normal
      Silver, silver
      Gold, gold ]
    |> Map.ofList

  let doCompare model crops ferts =
    let doubleCrops = doubleCropProb model
    let noGiantProb = noGiantCropProb model
    let giantProb = 1.0 - noGiantProb

    let bestReplants (baseCrop: BaseCrop) seedMakerData rawReplantPrices =
      [ for KeyValue(quality, sellPrice) in rawReplantPrices do
          UseRawCrop quality, sellPrice
        if defaultArg baseCrop.BuySeeds model.BuySeeds then
          let price = bestPriceOption model baseCrop.PriceFrom
          if price.IsSome then
            BuySeeds, float price.Value
        if Option.isSome seedMakerData then
          let seed, seedMaker, sell = Option.get seedMakerData
          if seedMakerReplantActive model seedMaker then
            for KeyValue(quality, sellPrice) in sell do
              SeedMaker quality, sellPrice / seedMakerAmount model quality seed ]
      |> List.groupBy snd
      |> List.map (fun (cost, r) -> cost, List.unzip r |> fst)

    let replantCost seed (seedMakerAmounts: Map<_,_>) (rawAmounts: Map<_,_>) =
      let rec helper totalCost seedsNeeded = function
        | [] -> totalCost, seedsNeeded
        | (thisCost, first)::rest ->
            let rec helper2 cost seeds = function
              | [] -> helper cost seeds rest
              | head::tail ->
                  match head with
                  | BuySeeds -> cost + thisCost * seeds, 0.0
                  | UseRawCrop quality ->
                      let amount = rawAmounts.[quality]
                      if amount < seeds
                      then helper2 (cost + amount * thisCost) (seeds - amount) tail
                      else cost + thisCost * seeds, 0.0
                  | SeedMaker quality ->
                      let maxSeeds = seedMakerAmounts.[quality] * seedMakerAmount model quality seed
                      if maxSeeds < seeds
                      then helper2 (cost + maxSeeds * thisCost) (seeds - maxSeeds) tail
                      else cost + seeds * thisCost, 0.0
            helper2 totalCost seedsNeeded first
      helper 0.0 1.0

    let cropProfit crop (fertilizers: (_*_* Map<_,_>) seq) =
      let speed = growthMultipler model crop

      match crop with
      | RegularCrop c ->
          let cropSell = bestSell model (Some (c.Base.Seed, c.SeedMaker)) c.Crop
          let replants, harvestItemAmount, harvestItemProfit =
            let replantsWith = bestReplants c.Base (Some (c.Base.Seed, c.SeedMaker, cropSell))
            match c.HarvestItem with
            | Some h ->
                let amount = singleAmount Normal h.Amount
                let profit = h.Amount * bestSellOne model Normal h.Item
                match h.Replant with
                | Some over when defaultArg over model.HarvestReplant -> replantsWith (bestSellNormal model None h.Item), amount, profit
                | _ -> replantsWith Map.empty, amount, profit
            | None -> replantsWith Map.empty, Map.empty, 0.0

          let extraCrops =
            if c.DoubleCrops
            then c.ExtraCrops * doubleCrops + doubleCrops
            else 0.0
            + c.ExtraCrops

          [ for fertilizer, fertilizerCost, qualityDistribution in fertilizers do
            let numHarvests = Crop.harvestsWithin model.StartDate model.EndDate (speed + Fertilizer.speedOfOption fertilizer) crop
            if numHarvests > 0 then
              let cropAmounts = qualityDistribution.Add(Normal, qualityDistribution.[Normal] + extraCrops)
              let costPerHarvest, seedsLeft = replantCost c.Base.Seed cropAmounts harvestItemAmount replants
              if seedsLeft = 0.0 then
                let profitPerHarvest =
                  Map.fold (fun sum quality amount ->
                    sum + amount * cropSell.[quality])
                    0.0
                    cropAmounts
                  + harvestItemProfit

                
                Crop.nameOf crop,
                Option.bind (Fertilizer.nameOf >> Some) fertilizer,
                profitPerHarvest * float numHarvests,
                costPerHarvest * float numHarvests,
                float fertilizerCost ]

      | RegrowCrop r ->
          let cropSell, replants =
            match r.Replant with
            | SeedMakerPlant s ->
                let sell = bestSell model (Some (r.Base.Seed, s)) r.Crop
                sell, bestReplants r.Base (Some (r.Base.Seed, s, sell)) Map.empty
            | RawPlant over ->
                let sell = bestSell model None r.Crop
                if defaultArg over model.HarvestReplant
                then sell, bestReplants r.Base None sell
                else sell, bestReplants r.Base None Map.empty

          let extraCrops = r.ExtraCrops * doubleCrops + doubleCrops + r.ExtraCrops

          [ for fertilizer, fertilizerCost, qualityDistribution in fertilizers do
            let numHarvests = Crop.harvestsWithin model.StartDate model.EndDate (speed + Fertilizer.speedOfOption fertilizer) crop
            if numHarvests > 0 then
              let cropAmounts = qualityDistribution.Add(Normal, qualityDistribution.[Normal] + extraCrops)
              let replantCost, seedsLeft = replantCost r.Base.Seed cropAmounts cropAmounts replants
              if seedsLeft = 0.0 then
                let profitPerHarvest =
                  Map.fold (fun sum quality amount ->
                    sum + amount * cropSell.[quality])
                    0.0
                    cropAmounts

                Crop.nameOf crop,
                Option.bind (Fertilizer.nameOf >> Some) fertilizer,
                profitPerHarvest * float numHarvests,
                replantCost,
                float fertilizerCost ]

      | GiantCrop g ->
          let cropSell = bestSell model (Some (g.Base.Seed, g.SeedMaker)) g.Crop
          let replants = bestReplants g.Base (Some (g.Base.Seed, g.SeedMaker, cropSell)) Map.empty

          let giantCrops = giantProb * 2.0

          [ for fertilizer, fertilizerCost, qualityDistribution in fertilizers do
            let numHarvests = Crop.harvestsWithin model.StartDate model.EndDate (speed + Fertilizer.speedOfOption fertilizer) crop
            if numHarvests > 0 then
              let cropAmounts =
                qualityDistribution
                |> Map.map (fun _ amount -> amount * noGiantProb)
                |> (fun map -> map.Add(Normal, map.[Normal] + giantCrops))
              let costPerHarvest, seedsLeft = replantCost g.Base.Seed cropAmounts Map.empty replants
              if seedsLeft = 0.0 then
                let profitPerHarvest =
                  Map.fold (fun sum quality amount ->
                    sum + amount * cropSell.[quality])
                    0.0
                    cropAmounts

                Crop.nameOf crop,
                Option.bind (Fertilizer.nameOf >> Some) fertilizer,
                profitPerHarvest * float numHarvests,
                costPerHarvest * float numHarvests,
                (1.0 + (numHarvests - 1 |> float) * model.FertilizerLossProb * giantProb) * float fertilizerCost ]

      | ForageCrop f -> []
      | Bush b ->
          let numHarvests = Crop.harvestsWithin model.StartDate model.EndDate speed crop
          [ if Seq.exists (fun (f, _, _) -> f = None) fertilizers && numHarvests > 0 then
              let profitPerHarvest = bestSellPrice model Normal (activeProducts model None b.Crop)
              let seedCost = bestPriceOption model b.Base.PriceFrom |> Option.get

              Crop.nameOf crop,
              None,
              profitPerHarvest * float numHarvests,
              float seedCost,
              0.0 ]
    
    [ for crop in crops do
        for c, f, profit, replantCost, fertCost in cropProfit crop ferts do
          let netProfit =
            profit
            - if model.AccountForReplant then replantCost else 0.0
            - if model.AccountForFertilizerCost then fertCost else 0.0 
          
          if model.ShowUnprofitableCombos || netProfit > 0.0 then
            c,
            f,
            netProfit ]
    |> List.sortByDescending (fun (_,_, p) -> p)

  let fertilizerData model farmBuffLevel = function
    | Some fert as f ->
        f,
        fert |> Fertilizer.priceFrom |> bestPriceOption model |> Option.get,
        qualityDistribution fert.Quality farmBuffLevel
    | None -> None, 0, qualityDistribution 0 farmBuffLevel

  let validCrops model =
    [ for KeyValue(_, crop) in model.Crops do
        if cropActive model crop then
          crop ]

  let farmBuffLevel model = Skill.buffedLevel model.Skills.[Name "Farming"]

  let allFertilizerData model =
    validFertilizers model
    |> listWithNone
    |> List.map (fertilizerData model (farmBuffLevel model))

  let doComboCompare model =
    doCompare
      model
      (validCrops model)
      (allFertilizerData model)

  let doCropCompare model withFertilizer =
    doCompare
      model
      (validCrops model)
      [ fertilizerData model (farmBuffLevel model) withFertilizer ]

  let doFertilizerCompare model withCrop =
    doCompare
      model
      [ withCrop ]
      (allFertilizerData model)



  let initial =
    { Page = Home
      SidebarTab = Skills
      SidebarOpen = false

      Skills = Skill.all |> listToMapByKey Skill.nameOf
      SkillList = Skill.all |> List.map Skill.nameOf
      IgnoreProfessionRelationships = false

      BuySourceList = Source.all |> List.map Source.nameOf
      BuySources = Source.all |> listToMapByKey Source.nameOf

      MatchConditions = MatchCondition.all |> listToMapByKey MatchCondition.nameOf
      MatchConditionList = MatchCondition.all |> List.map MatchCondition.nameOf

      Processors = Processor.all |> listToMapByKey Processor.nameOf
      ProcessorList = Processor.all |> List.map Processor.nameOf

      SellRawCrop = true
      SellSeedsFromSeedMaker = true

      BuySeeds = true
      SeedMakerReplant = true
      HarvestReplant = true

      Crops = Crop.all |> listToMapByKey Crop.nameOf
      CropSort = Seasons
      CropSortAscending = true
      CropList = Crop.all |> List.map Crop.nameOf
      SelectedCrop = None
      ShowOutOfSeasonCrops = false
      AllowCropClearings = false
      AllowCrossSeason = true
      AccountForReplant = true

      Fertilizers = Fertilizer.all |> listToMapByKey Fertilizer.nameOf
      FertilizerSort = Speed
      FertilizerSortAscending = true
      FertilizerList = Fertilizer.all |> List.map Fertilizer.nameOf
      SelectedFertilizer = None
      AccountForFertilizerCost = true
      FertilizerLossProb = 0.1

      Multipliers = Multiplier.all |> listToMapByKey Multiplier.nameOf
      RawMultipliers =
        Multiplier.all
        |> List.filter Multiplier.isRawMultiplier
        |> List.map Multiplier.nameOf

      StartDate =
        { Season = Spring
          Day = 1 }
      EndDate =
        { Season = Fall
          Day = 28 }
      Year = 1

      CompareMode = Combos
      ShowUnprofitableCombos = false
      SelectedCombo = None

      SelectedCompareCrop = None
      CompareCropsUsingFertilizer = None

      SelectedCompareFertilizer = None
      CompareFertilizersUsingCrop = Crop.nameOf Crop.all.Head

      StartingFertilizer = None

      ProfitMode = NetProfit
      Greenhouse = false

      ShowTips = true
      SaveSettings = false

      YearRequirementsShould = Warn
      SkillLevelRequirementsShould = Warn

      SpecialCharm = false
      LuckBuff = 0

      TrelisPenalty = false
      TrelisPercentage = 0.66
      AllowTrelisPair = false

      BaseGiantCropChance = 0.01
      GiantCropChecksPerTile = 8.0

      SeedMakerProb = 0.975
      AncientSeedProb = 0.005

      QualityProducts = false
      QualitySeedMaker = false
      QualitySeedMakerAmounts =
        Map.ofList
          [ Normal, 2.0
            Silver, 3.0
            Gold, 4.0
            Iridium, 5.0 ] }