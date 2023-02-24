module StardewValleyStonks.WebApp.Optimizer

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
- 0 or more non-regrow crop harvests in the starting season
- 0 or more of the following:
  - a bridge harvest followed by 0 or more non-regrow harvests in the next season
- at most one of the following:
  - a harvest with unreplaced fertilizer in the last season
  - harvests of a regrow crop for the remaining days/season(s)

All latter seasons in the dateSpan must be covered by bridge harvests or regrow crops as to carry over the fertilizer.
If this is not possible, then no subproblem is genereated/solved for the given dateSpan.

Taking the sequence of non-overlapping solved subproblems that provide the maximum profit
yields the final optimal solution for the given game data and settings.
*)

open StardewValleyStonks

open YALPS
open YALPS.Operators

// assume no two fertilizers have same (quality, speed)
let private fertilizerStrictlyWorse (fert1, cost1) (fert2, cost2) =
  Fertilizer.Opt.name fert1 <> Fertilizer.Opt.name fert2
  && cost1 >= cost2
  && Fertilizer.Opt.quality fert1 <= Fertilizer.Opt.quality fert2
  && Fertilizer.Opt.speed fert1 <= Fertilizer.Opt.speed fert2

// naive O(n^2), since n <= 7
let private removeStrictlyWorseFertilizers fertilizersAndCost =
  fertilizersAndCost |> Array.filter (fun fertCost ->
    fertilizersAndCost |> Array.exists (fertilizerStrictlyWorse fertCost) |> not)

let private groupFertilizersBySpeed fertilizersAndCost =
  fertilizersAndCost
  |> Array.groupBy (fst >> Fertilizer.Opt.speed)
  |> Array.map (fun (_, ferts) -> ferts |> Array.minBy snd |> fst, 0u)

type private Variable =
  | DayUsedForBridgeCrop
  | DayUsedForRegrowCrop
  | PlantCrop of Crop
  | PlantBridgeCrop of Crop
  | PlantCropUnreplacedFertilizer of Crop
  | PlantRegrowCropFixedHarvests of FarmCrop * nat
  | PlantRegrowCropFirstHarvests of FarmCrop * nat
  | PlantRegrowCropRegrowHarvests of FarmCrop

type private FertilizerSpanRequest = {
  Start: int
  Stop: int
  Fertilizer: Fertilizer option
  FertilizerCost: nat
  Variables: (int * Variable) array
}

type FertilizerDateSpan = {
  StartDate: Date
  EndDate: Date
  Fertilizer: Fertilizer option
  CropHarvests: (Crop * nat * bool) array
  RegrowCrop: (int * FarmCrop * nat) option
}

let [<Literal>] private Objective = "Objective"
let [<Literal>] private RegrowDays = "RegrowDays"
let [<Literal>] private BridgeDays = "BridgeDays"
let [<Literal>] private BridgeHarvests = "BridgeHarvests"
let [<Literal>] private EndingCrop = "EndingCrop"

let inline private (@) str1 str2 = str1 + "@" + str2

let private bridgeCropVariables settings objectiveValue season (fertilizer, fertCost) prevSeasonCrops =
  prevSeasonCrops |> Array.map (fun (crop, objectiveData) ->
    (season, PlantBridgeCrop crop), [|
      Objective, objectiveValue crop objectiveData fertilizer fertCost
      BridgeDays @ string season, Game.growthTime settings.Game fertilizer crop |> float
      BridgeHarvests @ string season, 1.0
    |])

let private regularCropVariables settings objectiveValue season (fertilizer, fertCost) crops =
  crops |> Array.choose (fun (crop, objectiveData) ->
    let value = objectiveValue crop objectiveData fertilizer fertCost
    if value < 0.0 then None else Some (
      (season, PlantCrop crop), [|
        Objective, value
        string season, Game.growthTime settings.Game fertilizer crop |> float
      |]
    ))

let private unreplacedFertilizerVariables settings season (fertilizer, fertCost) crops =
  crops |> Array.choose (fun (crop, profit) ->
    if fertCost = 0u || Query.replacedFertilizerPerHarvest settings crop = 0.0 then None else
    let profit = profit fertilizer
    if profit < 0.0 then None else Some (
      (season, PlantCropUnreplacedFertilizer crop), [|
        Objective, profit
        string season, Game.growthTime settings.Game fertilizer crop |> float
        EndingCrop, 1.0
      |]
    ))

let private regrowVariablesAndConstraints settings season prevDays totalDays fertIndex fertilizer regrowValues constraints regrowCrops =
  if Array.isEmpty regrowCrops then constraints, [||] else
  let constraints, variables =
    ((constraints, []), regrowCrops) ||> Array.fold (fun (constraints, variables) (crop, (data: Query.RegrowCropProfitData array)) ->
      let data = data[fertIndex]
      let growthTime = Game.growthTime settings.Game fertilizer (FarmCrop crop)
      let minHarvests = Growth.maxHarvests crop.RegrowTime growthTime prevDays
      let maxHarvests = Growth.maxHarvests crop.RegrowTime growthTime totalDays
      let fixedVariables = [
        for h in (max minHarvests 1u)..(min maxHarvests (data.HarvestsForMinCost - 1u)) do
          let cost = data.Cost h
          let usedDays = Growth.daysNeededFor crop.RegrowTime growthTime h
          (season, PlantRegrowCropFixedHarvests (crop, h)), regrowValues |> Array.append [|
            Objective, data.ProfitPerHarvest * float h - cost
            string season, float (if prevDays > usedDays then 0u else usedDays - prevDays)
          |]
      ]

      if maxHarvests >= data.HarvestsForMinCost then
        let harvests = max data.HarvestsForMinCost minHarvests
        let usedDays = Growth.daysNeededFor crop.RegrowTime growthTime harvests
        let harvestsName = string crop.Seed @ string season
        let firstVariable =
          (season, PlantRegrowCropFirstHarvests (crop, harvests)), regrowValues |> Array.append [|
            Objective, data.ProfitPerHarvest * float harvests - data.MinCost
            RegrowDays @ string season, float (int usedDays - int prevDays)
            harvestsName, -float (maxHarvests - harvests)
          |]

        let regrowVariable =
          (season, PlantRegrowCropRegrowHarvests crop), [|
            Objective, data.ProfitPerHarvest
            harvestsName, 1.0
            RegrowDays @ string season, float crop.RegrowTime.Value
          |]

        (harvestsName <== 0.0) :: constraints,
        (firstVariable :: regrowVariable :: fixedVariables) :: variables
      else
        constraints, fixedVariables :: variables)

  let regrowDayVariable =
    (season, DayUsedForRegrowCrop), [|
      string season, 1.0
      RegrowDays @ string season, -1.0
    |]

  let constraints = (RegrowDays @ string season === 0.0) :: constraints
  let variables =
    [ regrowDayVariable ]
    :: variables
    |> Seq.concat
    |> Array.ofSeq

  constraints, variables

let private bridgeToNextSeason settings objectiveValue season nextSeason days bridgeCropsVars fertilizerData constraintsAndVariables =
  (constraintsAndVariables, fertilizerData) ||> Array.map2 (fun (constraints, variables) (fertilizer, fertCost) ->
    let constraints =
      (string nextSeason <== float (days - 1u))
      :: (BridgeDays @ string nextSeason === 1.0)
      :: (BridgeHarvests @ string nextSeason === 1.0)
      :: constraints

    let variables =
      if Array.isEmpty bridgeCropsVars then variables else
      [|
        (season, DayUsedForBridgeCrop), [|
          string season, 1.0
          BridgeDays @ string nextSeason, -1.0
        |]
        (nextSeason, DayUsedForBridgeCrop), [|
          string nextSeason, 1.0
          BridgeDays @ string nextSeason, -1.0
        |]
      |]
      :: bridgeCropVariables settings objectiveValue nextSeason (fertilizer, fertCost) bridgeCropsVars
      :: variables

    constraints, variables)

let private bridgeRegrowToNextSeason season days nextSeason regrowValues =
  regrowValues |> Array.append [|
    BridgeDays @ string nextSeason, 1.0
    BridgeHarvests @ string nextSeason, 1.0
    string season, float (days - 1u)
  |]

let private createModels startSeason endSeason constraintsAndVariables fertilizerData =
  (constraintsAndVariables, fertilizerData) ||> Array.map2 (fun (constraints, variables) (fert, fertCost) ->
    let variables = Array.concat variables
    let request = {
      Start = startSeason
      Stop = endSeason
      Variables = variables |> Array.map fst
      Fertilizer = fert
      FertilizerCost = fertCost
    }

    // Need to convert lists into arrays in order to be sent to web worker.
    // Similarly, need to convert variable keys into indicies.
    let model =
      Model.createAllInteger
        Maximize
        Objective
        (Array.ofList constraints)
        (variables |> Array.mapi (fun i (_, var) -> i, var))

    request, model)

let private fertilizerAndCropData data settings mode (seasons: _ array) =
  let fertilizerFilter, objectiveValue, objectiveValueWithFertilizer, regrowCropData =
    match mode with
    | MaximizeGold ->
      removeStrictlyWorseFertilizers,
      (fun crop data (fertilizer: Fertilizer option) fertCost ->
        data fertilizer - Query.replacedFertilizerPerHarvest settings crop * float fertCost),
      Query.Optimizer.nonRegrowCropProfitPerHarvest data settings,
      Query.Optimizer.regrowCropProfitData data settings
    | MaximizeXP ->
      groupFertilizersBySpeed,
      (fun _ data fertilizer _ -> data fertilizer),
      Query.Optimizer.nonRegrowCropXpPerHarvest data settings >> Option.map konst,
      Query.Optimizer.regrowCropXpData data settings >> Option.map konst

  let fertilizerData =
    Query.Selected.fertilizersOpt data settings
    |> Seq.choose (fun fertilizer ->
      Query.fertilizerCostOpt data settings (Fertilizer.Opt.name fertilizer)
      |> Option.map (fun cost -> fertilizer, cost))
    |> Array.ofSeq
    |> fertilizerFilter

  let regrowCrops, crops =
    Query.Selected.inSeasonCrops data settings
    |> Array.ofSeq
    |> Array.partition Crop.regrows

  let cropData = crops |> Array.choose (fun crop ->
    objectiveValueWithFertilizer crop |> Option.map (fun objectiveValue -> crop, objectiveValue))

  let cropData =
    if settings.Game.Location = Farm
    then seasons |> Array.map (fun season -> cropData |> Array.filter (fst >> Crop.growsInSeason season))
    else [| cropData |]

  let regrowCropData = regrowCrops |> Array.choose (function
    | FarmCrop crop ->
      regrowCropData crop |> Option.map (fun data -> crop, fertilizerData |> Array.map (fst >> data))
    | ForageCrop _ -> None)

  let regrowCropData =
    if settings.Game.Location = Farm then
      seasons |> Array.mapi (fun i season ->
        regrowCropData |> Array.filter (fun (crop, _) ->
          crop |> FarmCrop.growsInSeason season
          && (i = seasons.Length - 1 || crop |> FarmCrop.growsInSeason seasons[i + 1] |> not)))
    else
      [| regrowCropData |]

  fertilizerData, cropData, regrowCropData, objectiveValue


let private createRequests data settings mode =
  let seasons, days =
    if settings.Game.Location = Farm then
      Date.seasonSpan settings.Game.StartDate settings.Game.EndDate,
      Date.daySpan settings.Game.StartDate settings.Game.EndDate
    else
      [| settings.Game.StartDate.Season |],
      [| Date.totalDays settings.Game.StartDate settings.Game.EndDate |]

  let fertilizerData, cropData, regrowCropData, objectiveValue =
    fertilizerAndCropData data settings mode seasons

  let rec nextSeasonModels prevDays startSeason endSeason regrowCropVars regrowValues constraintsAndVariables models =
    let totalDays = prevDays + days[startSeason]
    let regularCropVars = cropData[startSeason]

    let constraintsAndVariables =
      (constraintsAndVariables, fertilizerData)
      ||> Array.mapi2 (fun fertIndex (constraints, variables) (fert, fertCost) ->
        let regularVars = regularCropVariables settings objectiveValue startSeason (fert, fertCost) regularCropVars
        let constraints, regrowVars = regrowVariablesAndConstraints settings startSeason prevDays totalDays fertIndex fert regrowValues constraints regrowCropVars
        let variables = regularVars :: regrowVars :: variables
        constraints, variables)

    let models = createModels startSeason endSeason constraintsAndVariables fertilizerData :: models

    if startSeason = 0 then models else

    let nextSeason = startSeason - 1

    let bridgeCropsVars = regularCropVars |> Array.filter (fst >> Crop.growsInSeason seasons[nextSeason])
    let regrowCropsVars = regrowCropVars |> Array.filter (fst >> FarmCrop.growsInSeason seasons[nextSeason])

    if bridgeCropsVars.Length + regrowCropsVars.Length = 0 then models else

    let constraintsAndVariables = bridgeToNextSeason settings objectiveValue startSeason nextSeason days[nextSeason] bridgeCropsVars fertilizerData constraintsAndVariables
    let regrowValues = bridgeRegrowToNextSeason startSeason days[startSeason] nextSeason regrowValues

    nextSeasonModels totalDays nextSeason endSeason regrowCropsVars regrowValues constraintsAndVariables models

  Array.init seasons.Length (fun endSeason ->
    let startingConstraints = [
      string endSeason <== float (days[endSeason] - 1u)
      EndingCrop <== 1.0
    ]

    let constraintsAndVariables = fertilizerData |> Array.map (fun (fert, fertCost) ->
      startingConstraints, [ unreplacedFertilizerVariables settings endSeason (fert, fertCost) cropData[endSeason] ])

    nextSeasonModels 0u endSeason endSeason regrowCropData[endSeason] [| EndingCrop, 1.0 |] constraintsAndVariables [])
  |> Seq.concat
  |> Array.concat

let private weightedIntervalSchedule (solvedRequests: (FertilizerSpanRequest * int Solution) array) =
  let bestDownTo = Array.create (solvedRequests.Length + 1) ([], 0.0)
  solvedRequests |> Array.sortInPlaceBy (fun (request, _) -> request.Start)

  for i = solvedRequests.Length - 1 downto 0 do
    let request, solution = solvedRequests[i]

    // solvedRequests[j..] |> Array.find (fun (prev, _) -> request.Stop < prev.Start)
    let mutable j = i + 1
    while j < solvedRequests.Length && request.Stop >= (fst solvedRequests[j]).Start do
      j <- j + 1

    let prev, prevValue = bestDownTo[j]
    let value = solution.result - float request.FertilizerCost + prevValue
    let next = (request, solution) :: prev

    bestDownTo[i] <- maxBy snd bestDownTo[i + 1] (next, value)

  bestDownTo[0]

let private sortAndPartitionVariables vars request (solution: int Solution) =
  solution.variables
  |> Array.choose (fun (i, n) ->
    let season, variable = request.Variables[i]
    let cropAndOrder =
      match variable with
      | DayUsedForRegrowCrop
      | DayUsedForBridgeCrop -> None
      | PlantCrop crop -> Some (crop, 0)
      | PlantBridgeCrop crop -> Some (crop, 1)
      | PlantCropUnreplacedFertilizer crop -> Some (crop, 2)
      | PlantRegrowCropFixedHarvests (crop, _) -> Some (FarmCrop crop, 3)
      | PlantRegrowCropFirstHarvests (crop, _) -> Some (FarmCrop crop, 3)
      | PlantRegrowCropRegrowHarvests crop -> Some (FarmCrop crop, 4)

    cropAndOrder |> Option.map (fun (crop, order) ->
      let growthTime = Game.growthTime vars request.Fertilizer crop
      (season, order, growthTime), (variable, nat n)))

  |> Array.sortBy fst
  |> Array.partition (snd >> fst >> function
    | PlantRegrowCropFixedHarvests _
    | PlantRegrowCropFirstHarvests _
    | PlantRegrowCropRegrowHarvests _ -> true
    | _ -> false)

let private toDateSpans (vars: GameVariables) (solvedRequests: (FertilizerSpanRequest * int Solution) list, totalValue) =
  let seasons = Date.seasonSpan vars.StartDate vars.EndDate
  let solution = solvedRequests |> Seq.map (fun (request, solution) ->
    let regrowVariables, variables = sortAndPartitionVariables vars request solution
    let regrowCrop =
      match regrowVariables with
      | [| (season, _, _), (PlantRegrowCropFixedHarvests (crop, harvests), n) |]
      | [| (season, _, _), (PlantRegrowCropFirstHarvests (crop, harvests), n) |] ->
        assert (n = 1u)
        Some (season - request.Start, crop, harvests)

      | [|
          (season, _, _), (PlantRegrowCropFirstHarvests (crop, harvests), n)
          (season2, _, _), (PlantRegrowCropRegrowHarvests crop2, harvests2)
        |] ->
        assert (n = 1u)
        assert (season = season2 && crop.Seed = crop2.Seed)
        Some (season - request.Start, crop, harvests + harvests2)

      | regrowVariables ->
        assert (Array.isEmpty regrowVariables)
        None

    let cropHarvests = variables |> Array.choose (snd >> function
      | PlantCrop crop, harvests -> Some (crop, harvests, false)
      | PlantBridgeCrop crop, n ->
        assert (n = 1u)
        Some (crop, 1u, true)
      | PlantCropUnreplacedFertilizer crop, n ->
        assert (n = 1u)
        Some (crop, 1u, false)
      | _ -> None)

    let startDate, endDate =
      if vars.Location = Farm then
        {
          Season = seasons[request.Start]
          Day = if request.Start = 0 then vars.StartDate.Day else Date.firstDay
        }, {
          Season = seasons[request.Stop]
          Day = if request.Stop = seasons.Length - 1 then vars.EndDate.Day else Date.lastDay
        }
      else
        vars.StartDate, vars.EndDate

    {
      StartDate = startDate
      EndDate = endDate
      Fertilizer = request.Fertilizer
      CropHarvests = cropHarvests
      RegrowCrop = regrowCrop
    })

  totalValue, Array.ofSeq solution


open Fable.Core
open Browser.Url
open Browser.Worker

let [<Global>] private import: {| meta: {| url: string |} |} = jsNative

let createWorker () =
  let worker = Worker.Create (unbox<string> (URL.Create ("Worker.js", import.meta.url)), unbox {| ``type`` = Browser.Types.WorkerType.Module |})
  let mutable inProgressRequest = None
  let mutable nextRequest = None

  let postWorker data settings mode =
    let requests, models = Array.unzip (createRequests data settings mode)
    inProgressRequest <- Some (settings, requests)
    worker.postMessage (models: Worker.Input)

  let queue data settings mode =
    if inProgressRequest.IsNone
    then postWorker data settings mode
    else nextRequest <- Some (data, settings, mode)

  let subscribe dispatch =
    worker.onmessage <- (fun e ->
      match e.data, inProgressRequest with
      | :? Worker.Output as solutions, Some (settings, requests) ->
        match nextRequest with
        | Some (data, settings, mode) ->
          // old request finished solving, ignore and send most recent request to worker
          nextRequest <- None
          postWorker data settings mode
        | None ->
          // finished solving most recent request, dispatch solution
          inProgressRequest <- None

          solutions
          |> Array.zip requests
          |> Array.filter (fun (request, solution) ->
            // The nature of the problem should prevent unbounded solutions.
            // Also, only spans with multiple seasons should be able to be infeasible.
            assert (solution.status = Optimal || (solution.status = Infeasible && request.Start <> request.Stop))
            solution.status = Optimal)
          |> weightedIntervalSchedule
          |> toDateSpans settings.Game
          |> dispatch

      | _ -> assert false)

  queue, subscribe
