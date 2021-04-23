namespace StardewValleyStonks

[<Fable.Core.StringEnum>]
type RequirementPolicy =
  | [<CompiledName("Enforce")>] Enforce
  | [<CompiledName("Warn")>] Warn
  | [<CompiledName("Ignore")>] Ignore

module RequirementPolicy =
  let all =
    [ Enforce
      Warn
      Ignore ]

[<Fable.Core.StringEnum>]
type CompareMode =
  | [<CompiledName("Net Profit")>] NetProfit
  | [<CompiledName("Gold Per Day")>] GoldPerDay
  //| [<CompiledName("Return On Investment")>] ROI

module CompareMode =
  let all =
    [ NetProfit
      GoldPerDay ]


type Combo =
  { Description: string
    Crop: Crop name
    Fertilizer: Fertilizer name option
    Value: float option }

module Combo =
  let description pair = pair.Description
  let crop pair = pair.Crop
  let fertilizer pair = pair.Fertilizer
  let value pair = pair.Value

  let create comparisonValue crop fertilizer =
    { Description =
        match fertilizer with
        | Some f -> sprintf "%s with %s" !@crop !@f
        | None -> !@crop
      Crop = crop
      Fertilizer = fertilizer
      Value = comparisonValue crop fertilizer }


type DataModel =
  { // Static Data
    PriceMultipliers: PriceMultiplier seq
    ProfessionMultipliers: (Skills * Profession * float) seq
    RawMultipliers: RawMultiplier seq
    ProductMultipliers: Multiplier Set
    ProductProcessors: Processor name option seq
    ProductReplants: Processor name option seq
    SeedSources: Source seq
    FertilizerSources: Source seq
    FertilizerLossProb: float
    QualityProcessors: Processor name seq
    BaseGiantProb: float
    GiantYield: float
    SeedMakerSeedProb: float
    SeedMakerSeedAmount: int
    
    // Dynamic Data
    Skills: Map<Skills, Skill>

    SelectedPriceMultipliers: PriceMultiplier Set
    SelectedRawMultipliers: RawMultiplier Set

    Processors: Map<Processor name, Processor>

    Crops: Map<Crop name, Crop>
    CropSelection: Crop name Set
    AccountForReplant: bool
    //AllowCropClearings: bool
    //AllowCrossSeason: bool
    GiantChecksPerTile: float
    SpecialCharm: bool
    LuckBuff: int
    NoGiantProb: float
    DoubleCropProb: float

    Fertilizers: Map<Fertilizer name, Fertilizer>
    FertilizerSelection: Fertilizer name Set
    NoneFertilizerSelected: bool
    AccountForFertilizerCost: bool
    AccountForReplacementFertilizer: bool
    //AllowFertilizerChange

    StartDate: Date
    EndDate: Date
    Greenhouse: bool
    //Island: bool

    // Compare Mode
    CompareMode: CompareMode

    // Plan Mode

    // Find Mode
    StartingFertilizer: Fertilizer name option
    //UseMaxSeedMoney: bool
    //MaxSeedMoney: int

    // Settings
    SkillLevelPolicy: RequirementPolicy
    //SaveSettings: bool
    //TrelisPenalty: bool
    //TrelisPercentage: float
    //AllowTrelisPair: bool

    // Mod
    QualityProducts: bool
    QualitySeedMaker: bool
    QualitySeedMakerAmounts: int array
    
    Combos: Combo array Lazy }

module DataModel =
  let skills data = data.Skills.Find
  let skillUnlocked = skills >> SkillUnlock.unlocked

  let professionActive data skill profession =
    Skill.professionSelected skill profession
    && (data.SkillLevelPolicy <> Enforce || Skill.professionUnlocked skill profession)

  let professionActiveAlt data skill = professionActive data data.Skills.[skill]

  let farmingDistribution data = Skill.farmingDistribution data.Skills.[Farming]
  let foragingAmounts data = Skill.foragingAmounts (professionActive data) data.Skills.[Foraging]
  //let foragingDistribution model = Skill.foragingDistribution (professionActive model) model.Skills.[Foraging]


  let priceValue data = Price.value data.SelectedPriceMultipliers.Contains
  let lowestPrice = priceValue >> Prices.lowest
  let seedPrice = lowestPrice >> Crop.lowestSeedPrice
  let fertilizerPrice = lowestPrice >> Fertilizer.lowestPrice
  let fertilizerPriceOption = lowestPrice >> Fertilizer.lowestPriceOption


  let processors data = data.Processors.Find

  let processorActive data processor =
    data.SkillLevelPolicy <> Enforce
    || Processor.unlocked (skills data) data.Processors.[processor]
  let processorActiveOption = processorActive >> Option.forall

  let preservesQuality data =
    Option.map (processors data) >> Processor.preservesQualityOption data.QualityProducts


  let multiplierActive data = function
    | Raw r -> data.SelectedRawMultipliers.Contains r
    | Profession (s, p, _) -> professionActive data data.Skills.[s] p

  let itemPrice = multiplierActive >> Item.price
  let productPrice data = Product.price (itemPrice data) (preservesQuality data)
  let unitOutput data = Product.unitOutput data.Processors.Find
  let productUnitProfit data = Product.unitProfit (productPrice data) (unitOutput data)
  let rawCropUnitProfit data = RawCrop.unitProfit (productUnitProfit data) (processorActiveOption data)

  // let profit (rawCropProfit, rawCropProfitNormal, cropAmounts) forageDist farmingDist crop : float option =
  //   match crop with
  //   | RegularCrop c -> rawCropProfit (farmingDist |> Seq.mapi (enum<Quality> >> CropAmount.amountMapping cropAmounts.DoubleCropProb c.Amount)) c.RawCrop
  //   | RegrowCrop r -> rawCropProfit (farmingDist |> Seq.mapi (enum<Quality> >> CropAmount.amountMapping cropAmounts.DoubleCropProb r.Amount)) r.RawCrop
  //   | GiantCrop g -> rawCropProfit (farmingDist |> Seq.mapi (enum<Quality> >> CropAmount.giantAmount cropAmounts))  g.RawCrop
  //   | MultiCrop m ->
  //       Option.combine (+) (rawCropProfit farmingDist m.RawCrop) (m.OtherItem |> rawCropProfitNormal m.OtherAmount)
  //   | ForageCrop f ->
  //       let prof = rawCropProfit (forageDist |> Seq.map (CropAmount.forageAmount f.RawCrops.Count))
  //       f.RawCrops |> Map.fold (fun sum _ rawCrop ->
  //         prof rawCrop |> Option.combine (+) sum)
  //         None
  //   | BushCrop b -> b.RawCrop |> rawCropProfitNormal (float b.Amount)

  let private replantCostCalc harvests seeds replants =
    Array.sortInPlaceBy fst replants
    let mutable seedsLeft = float seeds
    let mutable totalCost = 0.0
    let mutable i = 0

    while i < replants.Length && seedsLeft > 0.0 do
      let cost, seedsPerHarvest = replants.[i]
      let seedsMade = (seedsPerHarvest * float harvests) |> min seedsLeft
      totalCost <- totalCost + cost * seedsMade
      seedsLeft <- seedsLeft - seedsMade
      i <- i + 1

    if seedsLeft = 0.0
    then Some totalCost
    else None

  let profitAndReplantCost lowestPriceCache unitProfitCache data =
    let processorActive = processorActiveOption data
    let unitOutput = unitOutput data
    let farmingDist = farmingDistribution data |> memoize
    let forageDist = foragingAmounts data
    let fertDist = Fertilizer.qualityOption >> farmingDist

    let rawCropProfit = RawCrop.profitPerHarvestCalc
    let rawCropProfitNormal = RawCrop.profitPerHarvestNormal unitProfitCache
    let deps = unitProfitCache, unitOutput, processorActive
    let rawCropReplant = RawCrop.replantCost deps
    let rawCropReplantNormal = RawCrop.replantCostNormal deps


    let helperCalc product quality profit amount =
      let output = unitOutput quality product
      profit / output, output * amount

    let helperNormal product (profit: _ array) amount =
      let output = unitOutput Quality.Normal product
      [| profit.[0] / output, output * amount |]


    let helper product =
      Seq.mapi2 (fun i profit amount ->
        let output = unitOutput (enum<Quality> i) product
        profit / output, output * amount)

    let singleReplant =
      if data.AccountForReplant then
        fun cropBase rawCrop rcReplants h s ->
          let replants = ResizeArray()
          cropBase.SeedPrices
          |> Option.bind (fst >> lowestPriceCache)
          |> Option.iter (fun seedPrice -> replants.Add((float seedPrice, System.Double.MaxValue)))

          RawCrop.activeReplantProduct processorActive rawCrop
          |> Option.iter (fun product -> replants.AddRange(rcReplants product))

          replants.ToArray() |> replantCostCalc h s
      else
        fun _ _ _ _ _ -> Some 0.0


    fun cropName fertilizerName ->
      let crop = data.Crops.[cropName]
      let fertilizer = fertilizerName |> Option.map data.Fertilizers.Find

      option {
        let! fertCost =
          if data.AccountForFertilizerCost
          then fertilizer |> Fertilizer.lowestPriceOption lowestPriceCache
          else Some 0

        let! harvests, totalDays, periods =
          let cb = Crop.cropBase crop
          let speed = Crop.growthSpeedBase (multiplierActive data) cb + Fertilizer.speedOption fertilizer
          Crop.growthData
            (Crop.bushHarvests data.StartDate data.EndDate)
            (Crop.growthPeriods data.StartDate data.EndDate)
            (Crop.growthTimeSpeed speed cb)
            crop

        let! netProfit =
          option {
            match crop with
            | RegularCrop c ->
                let! profit = unitProfitCache c.RawCrop
                let amounts = farmingDist (Fertilizer.qualityOption fertilizer) |> CropAmount.mappedAmount data.DoubleCropProb c.Amount
                let! replantCostPerHarvest = singleReplant c.Base c.RawCrop (fun product -> helper product profit amounts) 1 1
                //let! replantCost = singleReplant c.Base c.RawCrop (fun product -> helper product profit amounts) harvests harvests
                let profitPerHarvest = Seq.map2 (*) profit amounts |> Seq.sum

                return (float harvests) * (profitPerHarvest - replantCostPerHarvest) - float (periods * fertCost)

            | GiantCrop g ->
                let! profit = unitProfitCache g.RawCrop
                let amounts = farmingDist (Fertilizer.qualityOption fertilizer) |> CropAmount.mappedGiantAmount data.GiantYield data.NoGiantProb data.DoubleCropProb
                let! replantCostPerHarvest = singleReplant g.Base g.RawCrop (fun product -> helper product profit amounts) 1 1
                let profitPerHarvest = Seq.map2 (*) profit amounts |> Seq.sum
                let fertNeeded =
                  (float periods)
                  + if data.AccountForReplacementFertilizer
                    then (1.0 - data.NoGiantProb) * data.FertilizerLossProb * float (harvests - periods) 
                    else 0.0

                return (float harvests) * (profitPerHarvest - replantCostPerHarvest) - fertNeeded * float fertCost

            | MultiCrop m ->
                let profit = unitProfitCache m.RawCrop
                let otherProfit = unitProfitCache m.OtherItem
                if profit = None && otherProfit = None then
                  return! None

                let amounts = farmingDist (Fertilizer.qualityOption fertilizer)
                let! replantCostPerHarvest =
                  if data.AccountForReplant then
                    let replants = ResizeArray()

                    m.Base.SeedPrices
                    |> Option.bind (fst >> lowestPriceCache)
                    |> Option.iter (fun seedPrice -> replants.Add((float seedPrice, System.Double.MaxValue)))

                    Option.iter2 (fun product (p: _ array) ->
                      replants.AddRange(helper product p amounts))
                      (RawCrop.activeReplantProduct processorActive m.RawCrop)
                      profit

                    Option.iter2 (fun product (p: _ array) ->
                      replants.Add(helperCalc product Quality.Normal p.[0] m.OtherAmount))
                      (RawCrop.activeReplantProduct processorActive m.OtherItem)
                      otherProfit
                      

                    replants.ToArray() |> replantCostCalc 1 1
                  else
                    Some 0.0
                
                let profitPerHarvest =
                  (profit |> defaultMap 0.0 (Seq.map2 (*) amounts >> Seq.sum))
                  + (otherProfit |> defaultMap 0.0 (fun p -> p.[0] * m.OtherAmount))

                return (float harvests) * (profitPerHarvest - replantCostPerHarvest) - float (periods * fertCost)

            | RegrowCrop r ->
                let! profit = unitProfitCache r.RawCrop
                let amounts = farmingDist (Fertilizer.qualityOption fertilizer) |> CropAmount.mappedAmount data.DoubleCropProb r.Amount
                let! replantCost = singleReplant r.Base r.RawCrop (fun product -> helper product profit amounts) harvests periods
                let profitPerHarvest = Seq.map2 (*) profit amounts |> Seq.sum

                return (float harvests) * profitPerHarvest - (float periods) * (replantCost + float fertCost)

            | BushCrop b ->
                let! profit = unitProfitCache b.RawCrop
                let! replantCost = singleReplant b.Base b.RawCrop (fun product -> helper product profit (float b.Amount |> Seq.singleton)) harvests periods
                let profitPerHarvest = profit.[0] * float b.Amount

                return (float harvests) * profitPerHarvest - (float periods) * (replantCost + float fertCost)

            | ForageCrop f -> return! None
          }

        return
          match data.CompareMode with
          | NetProfit -> netProfit
          | GoldPerDay -> netProfit / float totalDays
      }

  let fertilizerOrder model =
    model.Fertilizers.Pairs
    |> Seq.sortBy (snd >> Fertilizer.quality)
    |> Seq.sortBy (snd >> Fertilizer.speed)


  let growthMultipler = multiplierActive >> Crop.growthMultiplier

  // let cropValid model crop =
  //   cropInSeason model crop
  //   && cropHasAProduct model crop
  //   && cropHasAReplant model crop
  //   && cropCanGiveOneHarvest model crop


  // let rawCropProfit model = RawCrop.profitPerHarvest (rawCropunitProfit model)
  // let rawCropProfitNormal model = RawCrop.profitPerHarvestNormal (rawCropunitProfit model)
  // let rawCRopReplantCost model = RawCrop.replantCost ()
  // let profit model = Crop.profit (rawCropProfit model) (rawCropProfitNormal model) model.CropAmounts (foragingAmounts model)
  // let replanCost mode = Crop.replantCost (raw)

  // let fertilizerValid model fertilizer =
  //   not model.AccountForFertilizerCost
  //   || Fertilizer.hasAPrice fertilizer

  // let fastestFertSpeed model =
  //   model.Fertilizers
  //   |> Map.filter (fun _ v -> fertilizerValid model v)
  //   |> Map.fold (fun speed _ fert -> Fertilizer.speed fert |> max speed) 0.0

  let cropOrder model =
    model.Crops.Pairs
    |> Seq.sortBy (snd >> Crop.seasons)


  let combos lowestCache unitProfitCache model =

    let combo = Combo.create (profitAndReplantCost lowestCache unitProfitCache model)

    let fertilizers = model.FertilizerSelection |> Seq.map Some |> Seq.append (Seq.singleton None)

    model.CropSelection
    |> Seq.collect (fun crop ->
      match model.Crops.[crop] with
      | BushCrop _ ->
          if model.NoneFertilizerSelected
          then Seq.singleton <| combo crop None
          else Seq.empty
      | _ ->
          fertilizers |> Seq.map (combo crop))
    |> Seq.sortByDescending (fun x -> x.Value)
    |> Seq.toArray



[<Fable.Core.StringEnum>]
type CropTableTab =
  | [<CompiledName("Select Crops")>] SelectCrops
  | [<CompiledName("Seed Prices")>] SeedPrices
  | [<CompiledName("Products")>] Products
  | [<CompiledName("Replants")>] Replants

module CropTableTab =
  let all =
    [ SelectCrops
      SeedPrices
      Products
      Replants ]

[<Fable.Core.StringEnum>]
type CropTab =
  | [<CompiledName("Growth")>] Growth
  | [<CompiledName("Seed Price")>] SeedPrice
  | [<CompiledName("Product")>] Product
  | [<CompiledName("Replant")>] Replant

[<Fable.Core.StringEnum>]
type FertilizerTableTab =
  | [<CompiledName("Custom Selection")>] SelectFertilizers
  | [<CompiledName("Prices")>] FertilizerPrices

module FertilizerTableTab =
  let all =
    [ SelectFertilizers
      FertilizerPrices ]

[<Fable.Core.StringEnum>]
type Mode =
  | [<CompiledName("Compare")>] Compare
  | [<CompiledName("Plan")>] Plan
  | [<CompiledName("Find")>] Find

module Mode =
  let tryParse = function
    | "Compare" -> Ok Compare
    | "Plan" -> Ok Plan
    | "Find" -> Ok Find
    | str -> Error <| sprintf "'%s' is not a Mode." str
  
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


[<Fable.Core.StringEnum>]
type SettingsTab =
  | [<CompiledName("Skills")>] Skills
  | [<CompiledName("Crops")>] Crops
  | [<CompiledName("Fertilizers")>] Fertilizers
  | [<CompiledName("Date")>] DateTab
  | [<CompiledName("Misc")>] Misc
  | [<CompiledName("Mod")>] Mod

module SettingsTab =
  let all =
    [ Skills
      Crops
      Fertilizers
      DateTab
      Misc
      Mod ]


type UIModel =
  { Page: Page
    SettingsTab: SettingsTab
    CropTableTab: CropTableTab
    CropTab: CropTab
    FertilizerTableTab: FertilizerTableTab
    IgnoreProfessionRelationships: bool
    CropSort: CropSort
    CropSortAscending: bool
    // InSeasonFilter: bool
    // ProfitableFilter: bool //ValidFilter?
    // SeasonsFilter: Season Set
    // CrossSeasonFilter: bool
    // CropTypeFilter: CropType option
    FertilizerSort: FertilizerSort
    FertilizerSortAscending: bool
    //ShowUnprofitable: bool
    //ShowInvalid: bool
    SelectedPair: (Crop name * Fertilizer name option) option }



module Model =
  let sortedFertilizers (ui, data) =
    (match ui.FertilizerSort with
    | FertilizerName ->
        if ui.FertilizerSortAscending
        then Seq.cache
        else Seq.rev
    | Quality ->
        Seq.sortByMode
          (not ui.FertilizerSortAscending)
          (snd >> Fertilizer.quality)
    | Speed ->
        Seq.sortByMode
          (not ui.FertilizerSortAscending)
          (snd >> Fertilizer.speed)
    | Cost ->
        Seq.sortByMode
          ui.FertilizerSortAscending
          (snd >> DataModel.fertilizerPrice data))
      data.Fertilizers.Pairs

  let sortedCrops (ui, data) =
    (match ui.CropSort with
    | CropName ->
        if ui.CropSortAscending
        then Seq.cache
        else Seq.rev
    | Seasons ->
        Seq.sortByMode
          ui.CropSortAscending
          (snd >> Crop.seasons)
    | TotalGrowthTime ->
        Seq.sortByMode
          ui.CropSortAscending
          (snd >> Crop.totalGrowthTime)
    | RegrowTime ->
        Seq.sortWith
          (Option.noneHighestCompareByMode
            ui.CropSortAscending
            (snd >> Crop.regrowTime))
    | SeedCost ->
        Seq.sortWith
          (Option.noneHighestCompareByMode
            ui.CropSortAscending
            (snd >> DataModel.seedPrice data)))
      data.Crops.Pairs
