module StardewValleyStonks.WebApp.Solver

(*
A description of the problem:
Basically, this is an unbounded knapsack problem with some extra bells and whistles (e.g., investment constraints).

First consider a single season:
Given a set of fertilizers and crops, the goal is find some
(fertilizer, a set of crops, and a number of harvests for each crop)
that provides the maxiumum possible profit.

There are also the following caveats:

Giant crops and forage crops may destroy their fertilizer after becoming fully grown.
Fertilizer should be replaced between harvests (accounting for some cost) in order to
uphold the growth time and expected profit per harvest (linearity).
But the last harvest of the season does not have to have its fertilizer replaced,
and this should be reflected in the model to save cost.

The cost of replacing the fertilizer may make the crop less profitable (e.g., high custom fertilizer prices),
such that the crop is more profitable when planted without any fertilizer.
Since fertilizer can be always be added to ground with no fertilizer,
but fertilizers cannot be swapped without destroying the previous one,
then the season may "start" with some amount of harvests of these crops without fertilizer.

Regrow crops must "end" the season, since they remain in the ground until they become out of season.
As a consequence, there can only be one distinct regrow crop per season / dateSpan.

The growth time and regrow time of a regrow crop often differ.
The growth time is for the first harvest, and only once it happens can the regrow harvests happen.

In some cases, the seed cost for regrow crops may not be constant with respect to the number of harvests.
If the stockpile seeds strategy is chosen, the seed maker is available, and item qualities are preserved on the sold items,
then using the lowest quality input into the seedmaker will minimize the seed cost.
But one harvest may not provide enough low quality items to create enough of these low cost seeds.
In this case, the seed cost monotonically decreases as the the number of harvests increase,
until the mimimum possible seed cost is eventually reached.
This is not exactly linear, and considering the rarity of this case,
a variable will be created for each number of harvests below the required number of harvests to reach the minimum seed cost.

On top of this, crops can grow in two consecutive seasons.
If a crop is planted at the end of the season and grows into the next season,
then the fertilizer it was planted with is not lost, but rather carried over to the next season.
So, this saves the cost of having to buy the same fertilizer again.
This crop may be regrow crop or just a regular crop that does not regrow.
In the case of a regular crop, it will be referred to as a "bridge" crop or harvest.
Additionally, this crop also gains a day of growth that would normally not be utilized
(from the last day of the previous season to the first day of the next).

Because of these reasons, instead of solving a model for each pair of fertilizer and season,
a model/subproblem must be solved for each pair of fertilizer and consecutive season range / dateSpan.

Put together, the variables should model the following timeline of harvests/crops for some fertilizer and dateSpan:
- 0 or more harvests without fertilizer in the starting season, for crops that can destroy fertilizer (and the fertilizer has a non-zero cost)
- 0 or more non-regrow crop harvests in the starting season
- 0 or more of the following:
  - a bridge harvest followed by 0 or more non-regrow harvests in the next season
- at most one of the following:
  - a harvest with unreplaced fertilizer in the last season
  - harvests of a regrow crop for the remaining days/season(s)

All seasons in the dateSpan must be covered by bridge harvests or regrow crops as to carry over the fertilizer.
If this is not possible, then no subproblem is genereated/solved for the given dateSpan.

Taking the sequence of non-overlapping solved subproblems that provide the maximum profit
yields the final optimal solution for the given game data and settings.
*)

open StardewValleyStonks

// assume no two fertilizers have same (quality, speed)
let private fertilizerStrictlyWorse (fert1, cost1) (fert2, cost2) =
  Fertilizer.Opt.name fert1 <> Fertilizer.Opt.name fert2
  && cost1 >= cost2
  && Fertilizer.Opt.quality fert1 <= Fertilizer.Opt.quality fert2
  && Fertilizer.Opt.speed fert1 <= Fertilizer.Opt.speed fert2

// naive O(n^2)
let private removeStrictlyWorseFertilizers fertilizersAndCost =
  fertilizersAndCost |> Array.filter (fun fertCost -> fertilizersAndCost |> Array.exists (fertilizerStrictlyWorse fertCost) |> not)

open YALPS
open YALPS.Operators

type Variable =
  | PlantCrop of SeedId
  | PlantCropUnreplacedFertilizer of SeedId
  | PlantCropNoFertilizer of SeedId
  | PlantBridgeCrop of SeedId
  | DayUsedForBridgeCropIntoNextSeason
  | DayUsedForBridgeCropFromPrevSeason
  | PlantRegrowCropFixedHarvests of SeedId * harvests: nat
  | PlantRegrowCropFirstHarvest of SeedId
  | PlantRegrowCropRegrowHarvests of SeedId
  | DayUsedForRegrowCrop

type private FertilizerDateSpanRequest = {
  Start: int
  Stop: int
  Fertilizer: FertilizerName option
  FertilizerCost: nat
  Variables: (Season * Variable) array
}

type FertilizerDateSpan = {
  Start: int
  Stop: int
  Fertilizer: FertilizerName option
  Variables: ((Season * Variable) * nat) array
}

let [<Literal>] private Profit = "Profit"
let [<Literal>] private RegrowDays = "RegrowDays"
let [<Literal>] private BridgeDays = "BridgeDays"
let [<Literal>] private BridgeHarvests = "BridgeHarvests"
let [<Literal>] private EndingCrop = "EndingCrop"

let inline private (@) str1 str2 = str1 + "@" + str2

(*
There is one case where the solver may not produce a correct answer.
This is if a crop can both destroy fertilizer and grow in two consecutive seasons.
Since no giant or forage crops can do so, this is fine for now.
Otherwise, this can happen if is chosen as the bridge crop
where it assumed it is not the last crop, and so the replacement cost is incurred.
However, it may be the case that it is indeed the last crop in the season range.
E.g., it is planted on the last day of the second to last season and the number of days
in the last season exactly covers the remaining growthTime of the crop.
*)

let inline private sndOf3 (_, y, _) = y

let private fertilizerSpans data settings =
  // refactor content into methods

  // assume SeedStrategy = Stockpile | IgnoreSeeds
  // TODO: SeedStrategy = BuyFirstSeed

  let seasons, days =
    let seasons, days = Date.seasonsAndDays settings.Game.StartDate settings.Game.EndDate
    if settings.Game.Location = Farm
    then seasons, days
    else [| settings.Game.StartDate.Season |], [| Array.sum days |]

  let fertilizerData =
    Query.selectedFertilizersOpt data settings
    |> Seq.choose (fun fertilizer ->
      match Query.lowestFertilizerCostOpt data settings (Fertilizer.Opt.name fertilizer) with
      | Some cost -> Some (fertilizer, cost)
      | None -> None)
    |> Array.ofSeq
    |> removeStrictlyWorseFertilizers
    |> Array.map (fun (fert, cost) -> Fertilizer.Opt.name fert, fert, cost)

  let crops, regrowCrops =
    Query.selectedInSeasonCrops data settings
    |> Array.ofSeq
    |> Array.partition (Crop.regrowTime >> Option.isNone)

  let groupBySeason arr =
    if settings.Game.Location = Farm
    then seasons |> Array.map (fun season -> arr |> Array.filter (fun x -> sndOf3 x |> Crop.growsInSeason season))
    else [| arr |]

  let cropData =
    crops
    |> Array.choose (fun crop ->
      match Query.nonRegrowData data settings crop with
      | Some data -> Some (Crop.seed crop, crop, data)
      | None -> None)
    |> groupBySeason

  let regrowCropData =
    regrowCrops
    |> Array.choose (fun crop ->
      match Query.regrowSeedData data settings crop with
      | Some data -> Some (Crop.seed crop, crop, fertilizerData |> Array.map (sndOf3 >> data))
      | None -> None)
    |> groupBySeason

  [
    for endSeason = seasons.Length - 1 downto 0 do
      let rec nextSeason
        prevDays
        startSeason
        bridgeCropVars
        regrowCropVars
        constraintsAndVars
        regrowValues
        models
        =
        let season = seasons[startSeason]
        let seasonDays = Season.name season
        let totalDays = prevDays + days[startSeason]
        let regularCropVars = cropData[startSeason]

        let constraintsAndVars =
          (constraintsAndVars, fertilizerData) ||> Array.mapi2 (fun i (constraints, vars) (_, fert, fertCost) ->
            let bridgeVars = bridgeCropVars |> Array.choose (fun (seed, crop, profit) ->
              match profit fert with
              | Some profit ->
                Some (
                  (season, PlantBridgeCrop seed), [
                    Profit, profit
                    BridgeDays @ seasonDays, Game.growthTime settings.Game fert crop |> float
                    BridgeHarvests @ seasonDays, 1.0
                  ])
              | None -> None)

            let regularVars = regularCropVars |> Array.choose (fun (seed, crop, profit) ->
              match profit fert with
              | Some profit ->
                let profit = profit - Query.replacementFertilizerPerHarvest settings crop * float fertCost
                if profit < 0.0 then None else Some (
                  (season, PlantCrop seed), [
                    Profit, profit
                    seasonDays, Game.growthTime settings.Game fert crop |> float
                  ])
              | None -> None
            )

            let regrowVars, constraints = (constraints, regrowCropVars) ||> Array.mapFold (fun constraints (seed, crop, (data: Query.RegrowData option array)) ->
              match data[i] with
              | None -> [], constraints
              | Some data ->
              let regrowTime = Crop.regrowTime crop |> Option.get
              let minHarvests = Growth.harvestsWith (Some regrowTime) data.GrowthTime prevDays
              let maxHarvests = Growth.harvestsWith (Some regrowTime) data.GrowthTime totalDays
              let fixedVars = [
                for h in (minHarvests |> max 1u)..(min maxHarvests (data.HarvestsForMinCost - 1u)) do
                  let cost = data.Cost h
                  if cost.IsSome then
                    let usedDays = data.GrowthTime + (h - 1u) * regrowTime
                    ((season, PlantRegrowCropFixedHarvests (seed, h)),
                      (Profit, data.Profit * float h - cost.Value)
                      :: (seasonDays, float (if prevDays > usedDays then 0u else usedDays - prevDays))
                      :: regrowValues)
              ]

              if maxHarvests >= data.HarvestsForMinCost then
                let harvests = max data.HarvestsForMinCost minHarvests
                let usedDays = data.GrowthTime + regrowTime * (harvests - 1u)
                let harvestsName = string seed @ seasonDays

                ((season, PlantRegrowCropFirstHarvest seed),
                  (Profit, data.Profit * float harvests - data.MinCost)
                  :: (RegrowDays @ seasonDays, -float(prevDays - usedDays))
                  :: (harvestsName, -float(maxHarvests - harvests))
                  :: regrowValues)
                :: ((season, PlantRegrowCropRegrowHarvests seed), [
                    Profit, data.Profit
                    harvestsName, 1.0
                    RegrowDays @ seasonDays, float regrowTime
                  ])
                :: fixedVars, (harvestsName <== 0.0) :: constraints
              else
                fixedVars, constraints)

            let regrowVars = Array.collect List.toArray regrowVars
            let vars =
              bridgeVars
              :: regularVars
              :: regrowVars
              :: vars

            let constraints, vars =
              if regrowVars.Length = 0 then constraints, vars else
                (RegrowDays @ seasonDays === 0.0) :: constraints,
                [|
                  (season, DayUsedForRegrowCrop), [
                    seasonDays, 1.0
                    RegrowDays @ seasonDays, -1.0
                  ]
                |]
                :: vars

            constraints, vars)

        let noFertilizerVars =
          if settings.Selected.NoFertilizer then
            regularCropVars |> Array.choose (fun (seed, crop, profit) ->
              if Query.replacementFertilizerPerHarvest settings crop = 0.0 then None else
              match profit None with
              | Some profit when profit >= 0.0 ->
                Some (
                  (season, PlantCropNoFertilizer seed), [
                    Profit, profit
                    seasonDays, Game.growthTime settings.Game None crop |> float
                  ])
              | _ -> None)
          else
            [| |]

        let newModels = (constraintsAndVars, fertilizerData) ||> Array.map2 (fun (constraints, vars) (fertName, fert, fertCost) ->
          // need to convert lists into arrays in order to be sent to web worker
          // similarly, need to convert variable keys into indicies
          let constraints = Array.ofList constraints
          let variables =
            (if fert = None then vars else noFertilizerVars :: vars)
            |> Array.concat
            |> Array.map (fun (k, v) -> k, Array.ofList v)

          {
            Start = startSeason
            Stop = endSeason
            Variables = variables |> Array.map fst
            Fertilizer = fertName
            FertilizerCost = fertCost
          },
          Model.createAllInteger Maximize Profit constraints (variables |> Array.mapi (fun i (_, var) -> i, var)))

        let models = newModels :: models

        if startSeason = 0 then models else

        let startSeason' = startSeason - 1
        let season' = seasons[startSeason']
        let seasonDays' = Season.name season'

        let bridgeCropsVars = regularCropVars |> Array.filter (sndOf3 >> Crop.growsInSeason season')
        let regrowCropsVars = regrowCropVars |> Array.filter (sndOf3 >> Crop.growsInSeason season')

        if bridgeCropsVars.Length + regrowCropsVars.Length = 0 then models else

        let constraintsAndVars = constraintsAndVars |> Array.map (fun (constraints, vars) ->
          let constraints =
            (seasonDays' <== float (days[startSeason'] - 1u))
            :: (BridgeDays @ seasonDays' === 1.0)
            :: (BridgeHarvests @ seasonDays' === 1.0)
            :: constraints

          let vars =
            if bridgeCropsVars.Length = 0 then vars else
              [|
                (season, DayUsedForBridgeCropFromPrevSeason), [
                  seasonDays, 1.0
                  BridgeDays @ seasonDays', -1.0
                ]
                (season', DayUsedForBridgeCropIntoNextSeason), [
                  seasonDays', 1.0
                  BridgeDays @ seasonDays', -1.0
                ]
              |]
              :: vars

          constraints, vars)

        let regrowValues =
          (BridgeDays @ seasonDays', 1.0)
          :: (BridgeHarvests @ seasonDays', 1.0)
          :: (seasonDays, float (days[startSeason] - 1u))
          :: regrowValues

        nextSeason totalDays startSeason' bridgeCropsVars regrowCropsVars constraintsAndVars regrowValues models

      let season = seasons[endSeason]
      let seasonDays = Season.name season
      let constraintsAndVars = fertilizerData |> Array.map (fun (_, fert, fertCost) ->
        [ seasonDays <== float (days[endSeason] - 1u); EndingCrop <== 1.0 ],
        [
          cropData[endSeason] |> Array.choose (fun (seed, crop, profit) ->
            if fertCost = 0u || Query.replacementFertilizerPerHarvest settings crop = 0.0 then None else
            match profit fert with
            | Some profit when profit >= 0.0 ->
              Some (
                ((season, PlantCropUnreplacedFertilizer seed), [
                  Profit, profit
                  seasonDays, Game.growthTime settings.Game fert crop |> float
                  EndingCrop, 1.0
                ])
              )
            | _ -> None)
        ]
      )

      nextSeason 0u endSeason [||] regrowCropData[endSeason] constraintsAndVars [ EndingCrop, 1.0 ] []
  ]
  |> List.concat
  |> Array.concat


let private weightedIntervalSchedule (spans: (FertilizerDateSpanRequest * int Solution) array) =
  let bestDownTo = Array.create (spans.Length + 1) ([], 0.0)
  spans |> Array.sortInPlaceBy (fun (span, _) -> span.Start)

  for i = spans.Length - 1 downto 0 do
    let span, solution = spans[i]
    let mutable j = i + 1
    while j < spans.Length && span.Stop >= (fst spans[j]).Start do
      j <- j + 1
    let prevSpan, prevProfit = bestDownTo[j]
    bestDownTo[i] <- maxBy snd bestDownTo[i + 1] ((span, solution) :: prevSpan, solution.result - float span.FertilizerCost + prevProfit)

  bestDownTo[0]

open Fable.Core
open Browser.Worker

let [<Global>] private import: {| meta: {| url: string |} |} = jsNative

type [<AllowNullLiteral>] private URLType =
  [<Emit("new $0($1...)")>] abstract Create: url: string * ?``base``: string -> string //Browser.Types.URL

let [<Global>] private URL: URLType = jsNative

let mutable private inProgressRequest = None
let mutable private nextRequest = None

let private worker = Worker.Create (URL.Create ("Worker.js", import.meta.url), unbox {| ``type`` = Browser.Types.WorkerType.Module |} )

let private postWorker data settings =
  let spans = fertilizerSpans data settings
  inProgressRequest <- Some spans
  worker.postMessage (spans |> Array.mapi (fun i (_, model) -> i, model))

let queueRequest data settings =
  if inProgressRequest.IsNone
  then postWorker data settings
  else nextRequest <- Some (data, settings)

let workerSub dispatch =
  worker.onmessage <- (fun e ->
    match e.data with
    | :? ((int * int Solution) array) as solutions ->
      match inProgressRequest with
      | None -> assert false
      | Some spans ->
        match nextRequest with
        | None ->
          inProgressRequest <- None

          let solution, totalProfit =
            solutions
            |> Array.map (fun (i, solution) -> fst spans[i], solution)
            |> weightedIntervalSchedule

          let solution = solution |> List.map (fun (span, solution) -> {
            Start = span.Start
            Stop = span.Stop
            Fertilizer = span.Fertilizer
            Variables = solution.variables |> Array.map (fun (i, n) -> span.Variables[i], nat n)
          })

          dispatch (solution, totalProfit)

        | Some (data, settings) ->
          postWorker data settings
          nextRequest <- None

    | _ -> ())
