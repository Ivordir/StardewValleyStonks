namespace StardewValleyStonks

open Types

[<Fable.Core.StringEnum>]
type CropTab =
  | [<CompiledName("Growth")>] Growth
  | [<CompiledName("Seed Prices")>] SeedPrices
  | [<CompiledName("Products")>] Products
  | [<CompiledName("Replant")>] Replants

module CropTab =
  let all =
    [ Growth
      SeedPrices
      Products
      Replants ]

[<Fable.Core.StringEnum>]
type Mode =
  | [<CompiledName("Compare")>] Compare
  | [<CompiledName("Plan")>] Plan
  | [<CompiledName("Find")>] Find

module Mode =
  let tryParse str =
    match str with
    | "Compare" -> Ok Compare
    | "Plan" -> Ok Plan
    | "Find" -> Ok Find
    | _ -> Error <| sprintf "'%s' is not a Mode." str
  
  open Elmish.UrlParser
  let parseUrl state = custom "Mode" tryParse state

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
    | Mode mode -> "#" + string mode
    | Help -> "#Help"

  open Elmish.UrlParser

  let parseUrl: Parser<_, Page> =
    oneOf
      [ map Home top
        map Mode Mode.parseUrl
        map Help (s "Help") ]

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

module SidebarTab =
  let all =
    [ Skills
      Crops
      Fertilizers
      Buy
      Sell
      Replant
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
  | [<CompiledName("Gold Per Day")>] GoldPerDay
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
    Skills: Map<Skills, Skill>
    IgnoreProfessionRelationships: bool

    RawMultipliers: Map<NameOf<RawMultiplier>, RawMultiplier>
    ProfessionMultipliers: Map<NameOf<Profession>, ProfessionMultiplier>

    BuySources: Map<NameOf<Source>, Source>
    BuySourceList: NameOf<Source> list //lists are necessary to enforce a display order, haven't thought of another way
    PriceMultipliers: Map<NameOf<PriceMultiplier>, PriceMultiplier>
    PriceMultiplierList: NameOf<PriceMultiplier> list

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
    CropTab: CropTab
    ShowOutOfSeasonCrops: bool
    ShowInvalidCrops: bool
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

    // Date
    StartDate: Date
    EndDate: Date

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
    SkillLevelPolicy: RequirementPolicy

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
  let skillUnlocked model (skillUnlock: SkillUnlock) =
    model.Skills.[skillUnlock.Skill].Level >= skillUnlock.UnlockLevel

  let sourceActive model source = model.BuySources.[source].Selected

  let processorActive model name =
    let processor = model.Processors.[name]
    processor.Selected
      && defaultProjection true (skillUnlocked model) processor.SkillUnlock

  let seedMakerActive model selected = selected && skillUnlocked model SkillUnlock.seedMaker
  let sellSeedsFromSeedMakerActive model seedMaker = seedMakerActive model (defaultArg seedMaker.SellSeeds model.SellSeedsFromSeedMaker)
  let seedMakerReplantActive model seedMaker = seedMakerActive model (defaultArg seedMaker.Replant model.SeedMakerReplant)


  let priceActive model source price =
    defaultArg (Buy.sourceOverride price) (sourceActive model source)

  let hasAPrice model = Map.exists (priceActive model)

  let cropHasAPrice model (baseCrop: BaseCrop) = defaultArg baseCrop.BuySeeds model.BuySeeds && hasAPrice model baseCrop.PriceFrom

  let rec priceValue model (priceFrom: Map<_,_>) source =
    match priceFrom.[source] with
    | BuyPrice p -> p.Value
    | RelativePrice r ->
        let multiplier = model.PriceMultipliers.[r.Multiplier]
        priceValue model priceFrom r.RelativeTo |> applyWhen (multiplier.Selected = multiplier.MultiplyWhenSelected) multiplier.Value

  type Integer = int

  let priceData model (priceFrom: Map<_,_>) =
    let bestPrice, sources =
      Map.fold (fun (currentMin, set) source price ->
        if priceActive model source price then
          let thisMin = priceValue model priceFrom source
          if thisMin = currentMin then currentMin, set |> Set.add source
          elif thisMin < currentMin then thisMin, Set.singleton source
          else currentMin, set
        else
          currentMin, set)
        (Integer.MaxValue, Set.empty)
        priceFrom
    if bestPrice = Integer.MaxValue then None else Some (bestPrice, sources)

  let bestPriceOption model (priceFrom: Map<_,_>) =
    let bestPrice =
      Map.fold (fun currentMin source price ->
        if priceActive model source price
        then min (priceValue model priceFrom source) currentMin
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

  let fertilizerValid model = Fertilizer.priceFrom >> hasAPrice model

  let fertilizerActive model fertilizer = Fertilizer.selected fertilizer && fertilizerValid model fertilizer

  let fastestFertSpeed model =
    model.Fertilizers
    |> Map.filter (fun _ fert -> fertilizerActive model fert)
    |> Map.fold (fun speed _ fert -> max speed fert.Speed) 0.0

  let professionActive model skill profession =
    model.Skills.[skill].Professions.[profession].Selected
    && (model.SkillLevelPolicy <> Enforce || profession |> Profession.isUnlocked model.Skills.[skill])

  let distribution model name =
    let skill = model.Skills.[name]
    match skill.Name with
    | Farming -> Skill.farmingDistribution skill
    | Foraging -> Skill.foargeDistribution (Name "Botanist" |> professionActive model Foraging) skill

  let multiplierActive model = function
    | Raw r -> model.RawMultipliers.[r].Selected
    | Profession (s, p) -> professionActive model s p

  let multiplierValue model = function
    | Raw r -> model.RawMultipliers.[r].Value
    | Profession (_, p) -> model.ProfessionMultipliers.[p].Value

  let growthMultipler model =
    Crop.growthMultipliers >>
    Set.fold (fun sum multiplier ->
      if multiplierActive model multiplier then
        sum + multiplierValue model multiplier
      else
        sum)
      0.0

  let cropIsInSeason model = Crop.isInSeason model.StartDate model.EndDate

  let cropCanGiveOneHarvest model crop = Crop.harvestsWithin model.StartDate model.EndDate (fastestFertSpeed model) (growthMultipler model crop) crop > 0

  let productActive model source product = defaultArg (Product.processorOverride product) (processorActive model source)


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

  let cropValid model crop =
    cropIsInSeason model crop
    && cropCanGiveOneHarvest model crop
    && cropHasAProduct model crop
    && cropHasAReplant model crop

  let cropActive model crop = Crop.selected crop && cropValid model crop

  let allCrops model = mapValues model.Crops
  let allFertilizers model = mapValues model.Fertilizers

  let activeCrops model = List.filter (fun c -> cropActive model model.Crops.[c]) model.CropList
  let activeFertilizers model = List.filter (fun f -> fertilizerActive model model.Fertilizers.[f]) model.FertilizerList

  let sortedFertilizers model =
    allFertilizers model
    //|> Seq.sortBy Fertilizer.name // already sorted by name, since the key of the map is the fertilizer's name
    |> Seq.sortBy Fertilizer.speed
    |> match model.FertilizerSort with
       | FertilizerSort.ByName -> sortMode model.FertilizerSortAscending Fertilizer.name
       | Quality -> sortMode model.FertilizerSortAscending Fertilizer.quality
       | Speed -> fun list -> if model.FertilizerSortAscending then list else Seq.sortByDescending Fertilizer.speed list
       | Price -> sortMode model.FertilizerSortAscending (Fertilizer.priceFrom >> priceSort model)
  
  let sortedCrops model =
    [ for KeyValue(_, crop) in model.Crops do
        if (model.ShowInvalidCrops || cropValid model crop)
          && (model.ShowOutOfSeasonCrops || cropIsInSeason model crop)
        then crop ]
    //|> Seq.sortBy Crop.name
    |> Seq.sortBy Crop.seasons
    |> match model.CropSort with
       | CropSort.ByName -> sortMode model.CropSortAscending Crop.name
       | Seasons -> fun list -> if model.CropSortAscending then list else Seq.sortByDescending Crop.seasons list
       | TotalGrowthTime -> sortMode model.CropSortAscending Crop.totalGrowthTime
       | RegrowTime -> sortMode model.CropSortAscending Crop.regrowTime
       | SeedPrice -> sortMode model.CropSortAscending (Crop.priceFrom >> priceSort model)

  let doubleCropProb model = 0.0001 + float model.LuckBuff / 1500.0 + if model.SpecialCharm then 0.025 else 0.0

  let noGiantCropProb model = (1.0 - model.BaseGiantCropChance) ** model.GiantCropChecksPerTile

  let itemPrice model item =
    match item.Multiplier with
    | Some m -> item.BasePrice |> applyWhen (multiplierActive model m) (multiplierValue model m)
    | None -> item.BasePrice

  let qualityPrice = itemPrice >>| flip Quality.multiply

  let processorPreservesQuality model processor =
    model.QualityProducts
    && model.Processors.[processor].PreservesQuality

  let qualityProcess model quality ``process`` =
    if processorPreservesQuality model ``process``.Processor
    then qualityPrice model ``process``.Output quality
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
    | RawCrop item -> qualityPrice model item quality |> float
    | Product product -> productProfit model quality product
    | SeedsFromSeedMaker seed -> seedMakerProfit model quality seed


  let activeProducts model seedMakerData harvestCrop =
    [ if defaultArg harvestCrop.SellRaw model.SellRawCrop then
        RawCrop harvestCrop.Crop
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

  let fertilizerPrices model = toMapByValue (Fertilizer.priceFrom >> priceData model >> Option.get >> fst)

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
                let amount = Quality.singleWith h.Amount Normal
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
            let numHarvests = Crop.harvestsWithin model.StartDate model.EndDate (Fertilizer.speedOfOption fertilizer) speed crop
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
            let numHarvests = Crop.harvestsWithin model.StartDate model.EndDate (Fertilizer.speedOfOption fertilizer) speed crop
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
            let numHarvests = Crop.harvestsWithin model.StartDate model.EndDate (Fertilizer.speedOfOption fertilizer) speed crop
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
          let numHarvests = Crop.harvestsWithin model.StartDate model.EndDate 0.0 speed crop
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
            c, f, netProfit ]
    |> List.sortByDescending (fun (_,_, p) -> p)

  let fertilizerData model = function
    | Some fert as f ->
        f,
        fert |> Fertilizer.priceFrom |> bestPriceOption model |> Option.get,
        Skill.farmingDistributionWith fert model.Skills.[Farming]
    | None -> None, 0, Skill.farmingDistribution model.Skills.[Farming]

  let validCrops model =
    [ for KeyValue(_, crop) in model.Crops do
        if cropActive model crop then
          crop ]

  let farmBuffLevel model = Skill.buffedLevel model.Skills.[Farming]

  let allFertilizerData model =
    validFertilizers model
    |> listWithNone
    |> List.map (fertilizerData model)

  let doComboCompare model =
    doCompare
      model
      (validCrops model)
      (allFertilizerData model)

  let doCropCompare model withFertilizer =
    doCompare
      model
      (validCrops model)
      [ fertilizerData model withFertilizer ]

  let doFertilizerCompare model withCrop =
    doCompare
      model
      [ withCrop ]
      (allFertilizerData model)