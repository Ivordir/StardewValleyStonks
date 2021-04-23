namespace StardewValleyStonks

type Season =
  | Spring = 1
  | Summer = 2
  | Fall = 3
  | Winter = 4

module Season =
  let name season = System.Enum.GetName(typeof<Season>, season)

  let ofInt value =
    let v = value % 4
    if v > 0
    then enum<Season> v
    else v + 4 |> enum

  let add value (season: Season) =
    value + int season |> ofInt

  let next = add 1
  let previous = add -1

  let isBetween start finish season =
    if start <= finish
    then start <= season && season <= finish
    else start <= season || season <= finish

  let overlaps start1 finish1 start2 finish2 =
    match start1 <= finish1, start2 <= finish2 with
    | false, false -> true
    | true, true -> start1 <= finish2 && start2 <= finish1
    | _ -> start1 <= finish2 || start2 <= finish1

  let all =
    [ Season.Spring
      Season.Summer
      Season.Fall
      Season.Winter ]


[<Fable.Core.Erase>]
type Date = Date of season: Season * day: int

module Date =
  let inline season (Date (season, _)) = season
  let inline day (Date (_, day)) = day

  let inline toTuple (Date (season, day)) = season, day

  let addDays days (Date (season, day)) =
    let d = day + days - 1
    Date (season |> Season.add (d / 28), d % 28 + 1)

  // let inDateSpan startDate endDate aSeason =
  //   let afterStart = season startDate <= aSeason
  //   let beforeEnd = aSeason <= season endDate
  //   if startDate < endDate
  //   then afterStart && beforeEnd
  //   else afterStart || beforeEnd

  let daysBetween (Date (endSeason, endDay)) (Date (startSeason, startDay)) =
    let days = 28 * (int endSeason - int startSeason) + endDay - startDay
    if days < 0
    then 112 + days // 28 * 4 + days
    else days

  let inline withDay day (Date(season, _)) = Date(season, day)
  let inline withSeason season (Date(_, day)) = Date(season ,day)

  let validDay = clamp 1 28

type Date with
  static member (-) (a, b) = Date.daysBetween a b



type CropAmount =
  | FarmingAmount
  | ExtraChance of extraProb: float
  | DoubleChance
  //| MoreYield of int
  | DoubleAndExtra of extraProb: float
  //| DoubleAndYield of int
  | DoubleYieldExtra of cropYield: int * extraProb: float

module CropAmount =
  let doubleCropProb luckBuff specialCharm = (float luckBuff / 1500.0) + (if specialCharm then 0.025 / 1200.0 else 0.0)
  let noGiantCropProb baseChance checkPerTile = (1.0 - baseChance) ** checkPerTile

  let validGiantChecks = clamp 0.0 9.0
  let validBuff: int -> _ = positive

  let cropsFromExtraChance extraChance = (1.0 / (1.0 - min extraChance 0.9)) - 1.0

  //let cropsFromDoubleChance doubleChance baseAmount = doubleChance * baseAmount + baseAmount - 1.0
  let cropsFromDoubleChance doubleChance extraAmount = doubleChance * extraAmount + extraAmount + doubleChance

  let additionalNormal additional quality amount =
    if quality = Quality.Normal
    then amount + additional
    else amount

  let extraChanceAmount = cropsFromExtraChance >> additionalNormal
  let doubleExtraAmount doubleProb e = cropsFromExtraChance e |> cropsFromDoubleChance doubleProb |> additionalNormal
  let doubleYieldExtraAmount doubleProb y e = (float (y - 1) + cropsFromExtraChance e) |> cropsFromDoubleChance doubleProb |> additionalNormal

  let giantAmount giantYield noGiantProb doubleCropProb quality amount =
    if quality = Quality.Normal
    then (1.0 - noGiantProb) * giantYield + noGiantProb * (amount + doubleCropProb)
    else noGiantProb * amount

  let idAmount _ a = a

  let forageAmount numKinds amount =
    amount / float numKinds

  let amountMapping doubleCropProb = function
    | FarmingAmount -> idAmount
    | DoubleChance -> additionalNormal doubleCropProb // cropsFromDoubleChance, extraAmount = 0.0
    | ExtraChance e -> extraChanceAmount e
    | DoubleAndExtra e -> doubleExtraAmount doubleCropProb e
    | DoubleYieldExtra (y, e) -> doubleYieldExtraAmount doubleCropProb y e

  let mappedGiantAmount giantYield noGiantProb doubleCropProb amounts =
    let a = Array.copy amounts
    a.[0] <- (1.0 - noGiantProb) * giantYield + noGiantProb * (a.[0] + doubleCropProb)
    for i = 1 to a.Length - 1 do
      a.[i] <- noGiantProb * a.[i]
    a


  let addAdditionalNormal amount amounts =
    let a = Array.copy amounts
    a.[0] <- a.[0] + amount
    a

  let mappedAmount doubleCropProb = function
    | FarmingAmount -> id
    | DoubleChance -> addAdditionalNormal doubleCropProb
    | ExtraChance e -> cropsFromExtraChance e |> addAdditionalNormal
    | DoubleAndExtra e -> cropsFromExtraChance e |> cropsFromDoubleChance doubleCropProb |> addAdditionalNormal
    | DoubleYieldExtra (y, e) -> (float (y - 1) + cropsFromExtraChance e) |> cropsFromDoubleChance doubleCropProb |> addAdditionalNormal


type RawCrop =
  { Item: Item
    Products: Map<Processor name option, Product>
    Selected: Product Set
    Replant: (Product * bool) option } // assume there is only one way to create seeds from this item.

module RawCrop =
  let item rawCrop = rawCrop.Item
  let products rawCrop = rawCrop.Products
  let selected rawCrop = rawCrop.Selected
  let processorSelected processor rawCrop = rawCrop.Products.TryFind processor |> Option.forall rawCrop.Selected.Contains
  let replantData rawCrop = rawCrop.Replant
  let replant = replantData >> Option.map fst
  let replantSelected = replantData >> Option.exists snd
  let replantProcessorSelected processor rawCrop =
    match rawCrop.Replant with
    | Some (product, s) when product.Processor = processor -> s
    | _ -> true
  let activeReplantProduct processorActive rawCrop =
    match rawCrop.Replant with
    | Some (product, true) when processorActive product.Processor -> Some product
    | _ -> None

  let unitProfit productUnitProfit processorActive rawCrop =
    let active = rawCrop.Selected |> Seq.filter (Product.processor >> processorActive) |> Seq.toArray
    if active.Length = 0 then
      None
    else
      Array.init 4 (fun i ->
        active |> Seq.map (productUnitProfit <| enum<Quality> i) |> Seq.max)
      |> Some

  let profitPerHarvestCalc: float seq -> float seq -> _ = Seq.map2 (*) >>| Seq.sum

  let profitPerHarvest unitProfit amounts (rawCrop: RawCrop) =
    unitProfit rawCrop |> Option.map (profitPerHarvestCalc amounts)

  let profitPerHarvestNormal (unitProfit: RawCrop -> _) (amount: float) = unitProfit >> Option.map (Array.item 0 >> (*) amount)

  let replantCostCalc (unitProfit: float array) unitOutput amounts product =
    amounts |> Seq.mapi (fun i a ->
      let output = unitOutput (enum<Quality> i) product
      unitProfit.[i] / output, Some (output * a))

  let replantCostNormal (unitProfit: _ -> float array option, unitOutput, processorActive) amount rawCrop =
    match rawCrop.Replant with
    | Some (product, true) when processorActive product.Processor ->
        unitProfit rawCrop |> Option.map (fun profit ->
          let output = unitOutput Quality.Normal product
          profit.[0] / output, Some (output * amount))
    | _ -> None

  let replantCost (unitProfit: _ -> float array option, unitOutput, processorActive) amounts rawCrop =
    match rawCrop.Replant with
    | Some (product, true) when processorActive product.Processor ->
        unitProfit rawCrop |> Option.map (fun profit ->
          amounts |> Seq.mapi (fun i a ->
            let output = unitOutput (enum<Quality> i) product
            profit.[i] / output, Some (output * a)))
    | _ -> None


  let setSelectedProduct add processor rawCrop =
    match rawCrop.Products.TryFind processor with
    | Some product ->
        { rawCrop with Selected = rawCrop.Selected |> Set.tryAddOrRemove add product }
    | None -> rawCrop

  let toggleProduct product (rawCrop: RawCrop) =
    { rawCrop with Selected = rawCrop.Selected |> Set.addOrRemove product }

  let toggleReplant rawCrop =
    { rawCrop with Replant = rawCrop.Replant |> Option.map (fun (r, s) -> r, not s) }

  let setSelectedReplant select processor rawCrop =
    match rawCrop.Replant with
    | Some (product, _) when product.Processor = processor -> { rawCrop with Replant = Some (product, select) }
    | _ -> rawCrop

  let create item seed productList =
    let products = Product.createRaw item :: (productList item seed)

    { Item = item
      Products = products |> Map.ofValues Product.processor
      Selected = set products
      Replant =
        products
        |> Seq.tryFind (fun product -> product.Item = seed) // again, assume there is at most one way to create seeds
        |> Option.map (fun product -> product, true) }



type CropBase =
  { Name: string
    Seasons: Season * Season
    GrowthStages: int list
    TotalGrowthTime: int
    GrowthMultipliers: Multiplier Set
    Seed: Item
    SeedPrices: (Prices * bool) option }

type Crop =
  | RegularCrop of
      {| Base: CropBase
         RawCrop: RawCrop
         Amount: CropAmount |}
  | GiantCrop of
      {| Base: CropBase
         RawCrop: RawCrop |}
  | RegrowCrop of
      {| Base: CropBase
         RawCrop: RawCrop
         Amount: CropAmount
         RegrowTime: int
         Trelis: bool
         IndoorsOnly: bool |}
  | BushCrop of
      {| Base: CropBase
         RawCrop: RawCrop
         Amount: int
         RegrowTime: int
         HarvestStartDay: int
         HarvestEndDay: int |}
  | MultiCrop of
      {| Base: CropBase
         RawCrop: RawCrop
         //Amount: CropAmount // always = FarmingAmount (even for sunflower)
         OtherItem: RawCrop
         OtherAmount: float |}
  | ForageCrop of
      {| Base: CropBase
         RawCrops: Map<Item, RawCrop>
         SeedUnlockLevel: int
         SellForageSeeds: bool
         ForageSeedsReplant: bool |}

type CropType =
  | Regular
  | Giant
  | Regrow of trelis: bool
  | Forage
  | Bush

[<Fable.Core.StringEnum>]
type CropSort =
  | [<CompiledName("Crop Name")>] CropName
  | [<CompiledName("Seasons")>] Seasons
  | [<CompiledName("Total Growth Time")>] TotalGrowthTime
  | [<CompiledName("Regrow Time")>] RegrowTime
  | [<CompiledName("Seed Cost")>] SeedCost

module Crop =
  let private nameBase crop = crop.Name
  let private seasonsBase crop = crop.Seasons
  let private crossSeasonBase crop =
    let start, finish = seasonsBase crop
    start <> finish
  let private startSeasonsBase = seasonsBase >> fst
  let private endSeasonBase = seasonsBase >> snd
  let private inSeasonBase start finish crop = seasonsBase crop ||> Season.overlaps start finish
  let private growthStagesBase crop = crop.GrowthStages
  let private totalGrowthTimeBase crop = crop.TotalGrowthTime
  let private growthMultipliersBase crop = crop.GrowthMultipliers
  let private seedBase crop = crop.Seed
  let private seedDataBase crop = crop.SeedPrices
  let private selectedSeedPriceBase crop =
    match crop.SeedPrices with
    | Some (p, true) -> Some p
    | _ -> None

  let cropBase = function
    | RegularCrop c -> c.Base
    | RegrowCrop r -> r.Base
    | GiantCrop g -> g.Base
    | MultiCrop m -> m.Base
    | ForageCrop f -> f.Base
    | BushCrop b -> b.Base

  let name = cropBase >> nameBase
  let nameKey: _ -> Crop name = name >> Name
  let startSeason = cropBase >> startSeasonsBase
  let endSeason = cropBase >> endSeasonBase
  let seasons = cropBase >> seasonsBase
  let crossSeason = cropBase >> crossSeasonBase
  let inSeason start finish = cropBase >> inSeasonBase start finish
  let growthStages = cropBase >> growthStagesBase
  let totalGrowthTime = cropBase >> totalGrowthTimeBase
  let growthMultipliers = cropBase >> growthMultipliersBase
  let growthMultiplier multiplierActive = growthMultipliers >> Seq.filter multiplierActive >> Seq.sumBy Multiplier.value
  let seedData = cropBase >> seedDataBase
  let seedPrices = seedData >> Option.map fst
  let seedSelected = seedData >> Option.map snd
  let lowestSeedPrice (lowest: Prices -> int option) = seedPrices >> Option.bind lowest
  let selectedSeedPrice = cropBase >> selectedSeedPriceBase
  let buySeeds = seedData >> Option.exists snd
  let seed = cropBase >> seedBase

  let rawCrops = function
    | RegularCrop c -> Seq.singleton c.RawCrop
    | RegrowCrop r -> Seq.singleton r.RawCrop
    | GiantCrop g -> Seq.singleton g.RawCrop
    | MultiCrop m -> [| m.RawCrop; m.OtherItem |] :> _ seq
    | ForageCrop f -> f.RawCrops |> Map.values
    | BushCrop b -> Seq.singleton b.RawCrop

  // let hasProfit = function
  //   | RegularCrop c -> c.RawCrop |> RawCrop.hasProfit
  //   | RegrowCrop r -> r.RawCrop |> RawCrop.hasProfit
  //   | GiantCrop g -> g.RawCrop |> RawCrop.hasProfit
  //   | MultiCrop m -> m.RawCrop |> RawCrop.hasProfit || m.OtherItem |> RawCrop.hasProfit
  //   | ForageCrop f -> f.RawCrops |> Map.exists (fun _ v -> RawCrop.hasProfit v)
  //   | BushCrop b -> b.RawCrop |> RawCrop.hasProfit

  // let products = function
  //   | RegularCrop c -> Seq.singleton c.RawCrop.Products
  //   | RegrowCrop r -> Seq.singleton r.RawCrop.Products
  //   | GiantCrop g -> Seq.singleton g.RawCrop.Products
  //   | MultiCrop m -> [ m.RawCrop.Products; m.OtherItem.Products ] :> seq<_>
  //   | ForageCrop f -> f.RawCrops |> Map.values |> Seq.map (fun rc -> rc.Products)
  //   | BushCrop b -> Seq.singleton b.RawCrop.Products

  // let hasAProductReplant processorActive = selectedReplants >> Set.exists (ProductReplant.valid processorActive)
  // let hasAReplant map processorActive crop = hasAPrice map crop || hasAProductReplant processorActive crop

  let regrowTime = function
    | RegrowCrop r -> Some r.RegrowTime
    | BushCrop b -> Some b.RegrowTime
    | _ -> None

  let isRegular = function
    | RegularCrop _ -> true
    | _ -> false

  let isGiant = function
    | GiantCrop _ -> true
    | _ -> false
  
  let isRegrow = function
    | RegrowCrop _ -> true
    | _ -> false

  let isBush = function
    | BushCrop _ -> true
    | _ -> false

  let isForage = function
    | ForageCrop _ -> true
    | _ -> false

  let isTrelis = function
    | RegrowCrop r -> r.Trelis
    | _ -> false

  // Stardew Valley passes speed around as a float32. This then gets coverted to a float, introducing some small error.
  // And this small error is sometimes enough to give an extra day of reduction since the value is later passed into the ceiling function.
  //       Case 1: 0.2f < 0.2
  //   float32 (expected)    -> float (after conversion, actual)
  //   0.2                   0.200000002980232
  //   * 10.0 growth days    * 10.0 growth days
  //   = 2.0                 = 2.00000002980232
  //   |> ceil |> int        |> ceil |> int
  //   = 2                   = 3 (not the same!)
  //   This effect can be seen on crops with a total of 10 growth days (e.g green bean, coffee)
  //   A 0.1 speed reduces the total time to 8 days (instead of 9),
  //   and a 0.2 speed reduces the total time to 7 days (instead of 8).
  //
  //      Case 2: 0.25f = 0.25 (all numbers 1/(2^n))
  //  float32        -> float
  //  0.25           0.25
  //  ...   Same    ...
  //
  //      Case3: 0.35f > 0.35
  //  float32               -> float
  //  0.35                  0.349999994039536
  //  * 20.0 growth days    * 20.0 growth days
  //  = 7.0                 = 6.99999988079071
  //  |> ceil |> int        |> ceil |> int
  //  = 7                   = 7 (wouldn't be equal if a floor was used instead of ceil)

  // But since (x: float) |> float32 |> float doesn't actually do a conversion (thanks javascript...), then here we use typed arrays to achieve the same effect.
  let private toF32AndBack =
    let toF32 = Fable.Core.JS.Constructors.Float32Array.Create 1
    fun value ->
      toF32.[0] <- float32 value
      float toF32.[0]

  let growthSpeedBase multiplierActive = growthMultipliersBase >> Seq.filter multiplierActive >> Seq.sumBy Multiplier.value

  let growthTimeSpeed speed crop =
    if speed = 0.0 then
      crop.TotalGrowthTime
    else
      let growthStages = crop.GrowthStages |> List.toArray
      let mutable daysToReduce = (toF32AndBack speed) * (float crop.TotalGrowthTime) |> ceil |> int
      let mutable daysReduced = 0
      let mutable traverses = 0

      while daysReduced < daysToReduce && traverses < 3 do
        // Handle the first stage
        if growthStages.[0] > 1 then
          growthStages.[0] <- growthStages.[0] - 1
          daysReduced <- daysReduced + 1

        // Handle the other stages
        let mutable stage = 1
        while daysReduced < daysToReduce && stage < growthStages.Length do
          if growthStages.[stage] > 0 then
            growthStages.[stage] <- growthStages.[stage] - 1
            daysReduced <- daysReduced + 1
          else // A reduction day was wasted reducing a stage below 0 days. Potentially possible?, as the game code does not prevent this from happening on stages except the first.
            daysToReduce <- daysToReduce - 1
          stage <- stage + 1

        traverses <- traverses + 1

      crop.TotalGrowthTime - daysReduced

  let growthTimeWith multiplierActive fertSpeed crop =
    let speed = fertSpeed + growthSpeedBase multiplierActive crop
    growthTimeSpeed speed crop

  let growthTime multiplierActive crop =
    growthTimeWith multiplierActive 0.0 (cropBase crop)

  let consecutiveGrowthPeriods (Date (startSeason, startDay)) (Date (endSeason, endDay)) cropStart cropEnd =
    let growthSeason = Season.isBetween cropStart cropEnd
    if startSeason = endSeason && startDay < endDay then
      if growthSeason startSeason
      then [| endDay - startDay |]
      else Array.empty
    else
      let growthPeriods = ResizeArray()
      // First season
      let mutable days =
        if growthSeason startSeason
        then 29 - startDay
        else 0
      // Middle seasons
      let mutable season = Season.next startSeason
      while season <> endSeason do
        if growthSeason season then
          days <- days + 28
        else
          if days > 1 then
            growthPeriods.Add(days - 1)
          days <- 0
        season <- Season.next season
      // Last season
      if growthSeason endSeason then
        days <- days + endDay

      if days > 1 then
        growthPeriods.Add(days - 1)
      
      growthPeriods.ToArray()

  let private growthDataWith harvestAdd periods growthTime =
    let mutable harvests = 0
    let mutable numPeriods = 0
    let mutable totalDays = 0
    for period in periods do
      if period >= growthTime then
        harvests <- harvests + harvestAdd period
        numPeriods <- numPeriods + 1
      totalDays <- totalDays + period
    if totalDays = 0
    then None
    else Some (harvests, totalDays, numPeriods)

  let growthData bushHarvests (growthPeriods: _ -> int array) growthTime crop =
    match crop with
    | BushCrop b ->
        let periods = growthPeriods crop
        if periods.Length = 0
        then None
        else Some (bushHarvests growthTime b, periods |> Array.sum, 1)
    | RegrowCrop r ->
        growthDataWith
          (fun period -> 1 + ((period - growthTime) / r.RegrowTime))
          (growthPeriods crop)
          growthTime
    | _ ->
        growthDataWith
          (fun period -> period / growthTime)
          (growthPeriods crop)
          growthTime


  let growthPeriods startDate endDate crop = seasons crop ||> consecutiveGrowthPeriods startDate endDate 

  //FIX!
  let bushHarvests
    startDate
    (Date (endSeason, endDay) as endDate)
    growthTime
    (bush:
      {| Base: CropBase
         RawCrop: RawCrop
         Amount: int
         RegrowTime: int
         HarvestStartDay: int
         HarvestEndDay: int |} ) =
    if growthTime > endDate - startDate then
      0
    else
      //bush start = 1, bush end = 28?
      let harvestsCalc finish start = 1 + (finish - start) / bush.RegrowTime
      let harvestSeason = bush.Base.Seasons ||> Season.isBetween
      let matureDate = startDate |> Date.addDays growthTime
      let mutable season, day = matureDate |> Date.toTuple

      if season = endSeason && (startDate <= endDate || matureDate <= endDate) then
        if harvestSeason season && day <= bush.HarvestEndDay
        then harvestsCalc (min bush.HarvestEndDay endDay) (max day bush.HarvestStartDay)
        else 0
      else
        let mutable harvests =
          if harvestSeason season && day <= bush.HarvestEndDay
          then harvestsCalc bush.HarvestEndDay (max day bush.HarvestStartDay)
          else 0

        season <- Season.next season
        while season <> endSeason do
          if harvestSeason season then
            harvests <- harvests + harvestsCalc bush.HarvestEndDay bush.HarvestStartDay
          season <- Season.next season

        if harvestSeason endSeason && bush.HarvestStartDay <= endDay then
          harvests <- harvests + harvestsCalc (min endDay bush.HarvestEndDay)  bush.HarvestStartDay

        harvests


  let validRegrowTime = max 1
  let validHarvestStartDay = Date.validDay
  let validHarvestEndDay = Date.validDay

  let private mapBase mapping = function
    | RegularCrop c ->
      RegularCrop {| c with Base = mapping c.Base |}
    | GiantCrop g ->
      GiantCrop {| g with Base = mapping g.Base |}
    | RegrowCrop r ->
      RegrowCrop {| r with Base = mapping r.Base |}
    | MultiCrop m ->
      MultiCrop {| m with Base = mapping m.Base |}
    | ForageCrop f ->
      ForageCrop {| f with Base = mapping f.Base |}
    | BushCrop b ->
      BushCrop {| b with Base = mapping b.Base |}

  let private mapPriceData mapping = mapBase (fun b -> { b with SeedPrices = mapping b.SeedPrices } )

  let private mapPrices mapping = mapPriceData (Option.map (fun (a, b) -> mapping a, b))
  let private mapBuySeeds mapping = mapPriceData (Option.map (fun (a, b) -> a, mapping b))

  let togglePrice = Prices.toggle >> mapPrices
  let setPriceSelected = Prices.setSelected >>| mapPrices

  let toggleBuySeeds = mapBuySeeds not
  let setBuySeeds select = mapBuySeeds (fun _ -> select)

  let private mapRawCrop mapping item = function
    | RegularCrop c ->
      RegularCrop {| c with RawCrop = mapping c.RawCrop |}
    | GiantCrop g ->
      GiantCrop {| g with RawCrop = mapping g.RawCrop |}
    | RegrowCrop r ->
      RegrowCrop {| r with RawCrop = mapping r.RawCrop |}
    | MultiCrop m ->
        if item = m.RawCrop.Item
        then {| m with RawCrop = mapping m.RawCrop |}
        else {| m with OtherItem = mapping m.OtherItem |}
        |> MultiCrop 
    | ForageCrop f ->
      ForageCrop {| f with RawCrops = f.RawCrops |> Map.update item mapping |}
    | BushCrop b ->
      BushCrop {| b with RawCrop = mapping b.RawCrop |}

  let toggleProduct = RawCrop.toggleProduct >> mapRawCrop
  let toggleReplant = mapRawCrop RawCrop.toggleReplant

  let private mapAllRawCrops mapping = function
    | RegularCrop c ->
      RegularCrop {| c with RawCrop = mapping c.RawCrop |}
    | GiantCrop g ->
      GiantCrop {| g with RawCrop = mapping g.RawCrop |}
    | RegrowCrop r ->
      RegrowCrop {| r with RawCrop = mapping r.RawCrop |}
    | MultiCrop m ->
      MultiCrop {| m with RawCrop = mapping m.RawCrop; OtherItem = mapping m.OtherItem |}
    | ForageCrop f ->
      ForageCrop {| f with RawCrops = f.RawCrops |> Map.mapByValue mapping |}
    | BushCrop b ->
      BushCrop {| b with RawCrop = mapping b.RawCrop |}

  let setSelectedProduct = RawCrop.setSelectedProduct >>| mapAllRawCrops
  let setSelectedReplant = RawCrop.setSelectedReplant >>| mapAllRawCrops

  let createPrices prices = Some (Prices.create prices, true)

  let createPricesOption prices =
    if prices |> Seq.isEmpty
    then None
    else createPrices prices

  let private createBase growthMultipliers name seed seasons growthStages prices =
    { Name = name
      Seasons = seasons
      GrowthStages = growthStages
      TotalGrowthTime = Seq.sum growthStages
      GrowthMultipliers = growthMultipliers
      Seed = seed
      SeedPrices = prices seed.BasePrice }

  let createRegularWith growthMultipliers amount (name, seed) seasons growthStages prices item products =
    RegularCrop
      {| Base = createBase growthMultipliers name seed seasons growthStages prices
         RawCrop = RawCrop.create (item name) seed products
         Amount = amount |}

  let createScythe gm = createRegularWith gm FarmingAmount
  let createRegularExtra gm = DoubleAndExtra >> createRegularWith gm
  let createRegularYield gm = tuple2 >>| DoubleYieldExtra >>| createRegularWith gm
  let createRegular gm = createRegularWith gm DoubleChance

  let createGiant growthMultipliers (name, seed) seasons growthStages prices item products =
    GiantCrop
      {| Base = createBase growthMultipliers name seed seasons growthStages prices
         RawCrop = RawCrop.create (item name) seed products |}

  let createRegrowWith indoorsOnly growthMultipliers trelis amount (name, seed) seasons growthStages regrowTime prices item products =
    RegrowCrop
      {| Base = createBase growthMultipliers name seed seasons growthStages prices
         RawCrop = RawCrop.create (item name) seed products
         RegrowTime = regrowTime
         Amount = amount
         Trelis = trelis
         IndoorsOnly = indoorsOnly |}

  let private createOutDoors gm = createRegrowWith true gm

  let createTrelisAmount gm = createOutDoors gm true
  //let createTrelisYield = DoubleYieldExtra >> createTrelisAmount
  //let createTrelisExtra = DoubleAndExtra >> createTrelisAmount
  let createTrelis gm = createTrelisAmount gm DoubleChance

  let createRegrowAmount gm = createOutDoors gm false
  let createRegrowYield gm = DoubleYieldExtra >> createRegrowAmount gm
  let createRegrowExtra gm = DoubleAndExtra >> createRegrowAmount gm
  let createRegrow gm = createRegrowAmount gm DoubleChance

  let createMulti growthMultipliers (name, seed) seasons growthStages prices item products otherItem otherProducts otherAmount =
    MultiCrop
      {| Base = createBase growthMultipliers name seed seasons growthStages prices
         RawCrop = RawCrop.create (item name) seed products
         OtherItem = RawCrop.create otherItem seed otherProducts
         OtherAmount = otherAmount |}

  let createBushWith growthMultipliers amount (name, seed) seasons growthStages regrowTime harvestDayStart harvestDayEnd prices item products =
    BushCrop
      {| Base = createBase growthMultipliers name seed seasons growthStages prices
         RawCrop = RawCrop.create (item name) seed products
         RegrowTime = regrowTime
         Amount = amount
         HarvestStartDay = harvestDayStart
         HarvestEndDay = harvestDayEnd |}
  
  let createBush n = createBushWith Set.empty 1 n

  let createForage growthMultipliers season seedSell seedUnlock prices products =
    let seasonName = Season.name season
    let seed = Item.create (seasonName + " Seeds") seedSell
    ForageCrop
      {| Base = createBase growthMultipliers (seasonName + " Forage") seed (season, season) [ 3; 4 ] (fun _ -> createPricesOption prices)
         RawCrops = products |> Seq.map (fun (item, product) -> RawCrop.create item seed product) |> Map.ofValues RawCrop.item
         SeedUnlockLevel = seedUnlock
         SellForageSeeds = true
         ForageSeedsReplant = true |}
