// All possible constraints:
// Season.name season <== days[season]
// UnreplacedFertilizer <== 1.0
// RegrowChoice <== 1.0
// RegrowDays <== daysAfterFirstSeason - growthTime
// RegrowHarvests >== harvestsForMinReplantCost
// CrossDays @ Season.name end <== 1.0
// CrossHarvests @ Season.name end == 1.0

// All possible variables:
// crop:
//   cropId @ fertId @ Season.name season
//   [ Profit, profit - replacementCost
//     Season.name season, growthTime ]

// crop with unreplaced lost fertilizer:
//   UnreplacedFertilizer @ cropId @ fertId @ Season.name season
//   [ Profit, profit
//     UnreplacedFertilizer, 1.0
//     Season.name season, growthTime ]

// regrow crop with fixed harvests
//   FixedRegrow @ cropId @ fertId @ Season.name start @ Season.name stop @ harvests
//   [ Profit, profit * harvests - replantCost
//     Season.name season, daysUsedInFirstSeason
//     RegrowChoice, 1.0 ]

// regrow crop with harvests lowerbound
//   Regrow @ cropId @ fertId @ Season.name start @ Season.name stop
//   [ Profit, profit
//     RegrowDays, regrowTime
//     RegrowHarvests, 1.0 ]
//   // 1 regrow harvest and (profit - minReplantCost) is added after

// regrowDaysInFirstSeason
//   RegrowDaysInFirstSeason
//   [ Season.name season, 1.0
//     RegrowDays, -1.0 ]

// cross season crop
//   CrossCrop @ cropId @ fertId @ Season.name end
//   [ Profit, profit
//     CrossDays @ Season.name end, growthTime
//     CrossHarvests @ Season.name end, 1.0 ]

// number of days in this season used for the cross crop to the next season
//   CrossDaysForNext @ Season.name start
//   [ Season.name start, 1.0
//     CrossDays @ Season.name end, -1.0 ]

// number of days in this season used for the cross crop from the previous season
//   CrossDaysForPrevious @ Season.name end
//   [ CrossDays @ Season.name end, -1.0
//     Season.name end, 1.0 ]

module StardewValleyStonks.WebApp.Solver

open StardewValleyStonks

type Variable =
  | PlantCrop of SeedId * FertilizerName option
  | PlantCropUnreplacedFertilizer of SeedId * FertilizerName option
  | PlantBridgeCrop of SeedId * FertilizerName option
  | DayUsedForBridgeCropIntoNextSeason
  | DayUsedForBridgeCropFromPrevSeason
  | PlantRegrowCropFixedHarvests of SeedId * FertilizerName option * harvests: nat
  | PlantRegrowCropFirstHarvest of SeedId * FertilizerName option
  | PlantRegrowCropRegrowHarvests of SeedId * FertilizerName option
  | DayUsedForRegrowCrop



open YALPS
open YALPS.Operators

module ValueNames =
  let [<Literal>] Profit = "Profit"
  let [<Literal>] RegrowDays = "RegrowDays"
  let [<Literal>] BridgeDays = "BridgeDays"
  let [<Literal>] BridgeHarvests = "BridgeHarvests"
open ValueNames


// type SubRangeEnd =
//   | EndWithUnreplacedFertilizer of crop: Item Id
//   | EndWithRegrowCrop of daysInFirst: nat * crop: Item Id * harvests: nat

// type SubRangeSolution =
//   { Start: Season
//     End: Season
//     Fertilizer: (Fertilizer Id * (Item Id * nat) Index) option
//     CrossHarvests: (nat * Item Id * nat) Index
//     Crops: (Item Id * nat) Index Index
//     EndingCrop: SubRangeEnd option }

// module SubRangeSolution =
//   let ofRawSolution startSeason endSeason (solution: RawSolution) =
//     let vars = JS.Constructors.Object.entries solution |> unbox<(string * float) array>

//     for name, value in vars do
//       match name |> String.split "@" with
//       | [| nameof solution.feasible | nameof solution.bounded | nameof solution.isIntegral | nameof solution.result |] -> ()
//       | [| UnreplacedFertilizer; cropId; fertId; season |] -> ()
//       | [| FixedRegrow; cropId; fertId; start; stop; harvests |] -> assert (value = 1.0); ()
//       | [| RegrowCrop; cropId; fertId; start; stop |] -> ()
//       | [| RegrowDaysInFirstSeason |] -> ()
//       | [| CrossCrop; cropId; fertId; stop |] -> ()
//       | [| CrossDaysForNext; start |] -> ()
//       | [| CrossDaysForPrevious; stop |] -> ()
//       | [| cropId; fertId; season |] -> ()
//       | _ -> ()

//     ()

let inline private (@) str1 str2 = str1 + "@" + str2
let private fertilizerIdOption = Option.defaultOrMap "" Fertilizer.name

let endingCrop = "EndingCrop", 1.0


(*
A description of the problem:
Basically, this is an unbounded knapsack problem with some extra bells and whistles.

First consider a single season:
Given a set of fertilizers and crops, the goal is find some
(fertilizer, a set of crops, and a number of harvests for each crop)
that provide the maxiumum possible profit.

There are also the following caveats:
- Crops that become giant and forage crops may destroy their fertilizer after being fully grown.
  Fertilizer must be replaced between harvests (accounting for some cost),
  but the last harvest does not have to have its fertilizer replaced.
  So, the season may "end" with one of these harvests without replacement to save cost.

- The cost of replacing the fertilizer may make the crop less profitable,
  such that it becomes more profitable when planted without any fertilizer.
  Since fertilizer can be added always be added to ground with no fertilizer,
  but fertilizers cannot be swapped without destroying the previous one,
  then the season may "start" with some amount of harvests of these crops without fertilizer.

- Regrow crops must "end" the season, since they remain in the ground until the end of the season.
  As a consequence, there can only be one distinct regrow crop per season / dateSpan.

- The growth time and regrow time of a regrow crop may differ.
  The growth time for the first harvest can only happen once,
  and only once it happens can the regrow harvests happen.

On top of this, crops can grow in two consecutive seasons.
If one is planted with fertilizer at the end of the season,
then the fertilizer is not lost at the start of the season.
This crop could be a regular crop or one that regrows.
Additionally, this crop also gains a day of growth that would normally not be utilized
(from the last day of the previous season to the first day of the next).
Because of this, instead of solving a model for each pair of fertilizer and season,
a model/subproblem must be solved for each pair of fertilizer and consecutive season range / dateSpan.
E.g., for Spring-Fall: Spring, Summer, Fall, Spring-Summer, Summer-Fall, Spring-Fall
The "start" and "end" crops mentioned before must still occur
in the starting and ending season, respectively, for each season range.
Taking the sequence of non-overlapping solved subproblems that provide the maximum profit
yields the final optimal solution for the given game data and variables.

Discounting Winter because it has only one crop and no crops grow into/out of it,
then for a Spring start date and Fall end date there can be at most
7 fertilizers * 6 season ranges = 42 subproblems that need to be solved.

TODO:
- make an estimation for how long the solver will take,
  and start a p-cancellable web worker if necessary to not block the main thread ?

There is one case where the solver may not produce a correct answer.
This is if a crop can both destroy fertilizer and grow in two consecutive seasons.
Since no giant or forage crops can do so, this is fine for now.
Otherwise, this can happen if is chosen as the cross-season crop
where it assumed it is not the last crop, and the replacement cost is incurred.
However, it may be the case that it is the last crop in the season range,
e.g., it is planted on the last day of the previous season and the number of days
in the next season exactly covers the remaining growthTime of the crop.

Put together, the generated constraints and variables for a subproblem takes this form:
Constraints:
[ for Season in seasons
    Season <= days in season // days used in each season
  for Season in tail seasons
    CrossDays(Season) = 1
    // days deticated to the 1 harvest of a crop
    // needed to carry the fertilizer over from the previous season
    CrossHarvests(Season) = 1

  EndingCrop <= 1
  // at most one kind of caveat ending to the season range
  // I.e., unreplaced fertilizer or regrow crop

  RegrowDays <= 0 // days dedicated to regrowHarvests in the first season
  for regrowCrop in crops
    RegrowHarvests(regrowCrop) <= 0
    // no regrowHarvests unless the first harvest is chosen ]
Variables:
[ //"Start"
  for giant|forage in crops that can lose fertilizer and grow in first season
    { Profit = profit with no fertilizer
      StartSeason = growthTime }

  //"Mid"
  for regular|giant|forage in crops
    for Season in seasons where crop grows
    { Profit = profit - replacment cost
      Season = growthTime }

  //"End"
  for giant|forage in crops that can lose fertilizer and grow in last season
    { Profit = profit (no replacement cost incurred)
      EndSeason = growthTime }

  //"Cross"
  for regrowCrop in crops

  ]
*)

type SubRangeSolutionRequest = {
  Start: int
  Stop: int
  Model: Model<Season * Variable, string>
  ExtraProfit: float
}

let solutionRequests data settings (fertilizers: FertilizerName option array) (crops: SeedId array) =
  let crops = crops |> Array.map data.Crops.Find
  let varSeasons, days, crops, growsInSeason =
    let seasons, days = Date.seasonsAndDays settings.Game.StartDate settings.Game.EndDate
    if settings.Game.Location = Farm then
      let s = Seasons.ofSeq seasons
      seasons,
      days,
      crops |> Array.filter (Crop.growsInSeasons s),
      fun j crop -> crop |> Crop.growsInSeason seasons[j]
    else
      [| settings.Game.StartDate.Season |],
      [| Array.sum days |],
      crops,
      fun _ _ -> true

  let days = days |> Array.map (fun x -> x - 1u)

  let fertilizers =
    fertilizers |> Seq.choose (fun fertilizerName ->
      let fertilizer = fertilizerName |> Option.map data.Fertilizers.Find
      match Query.lowestFertilizerCostOpt data settings fertilizerName with
      | Some cost -> Some (fertilizer, cost)
      | None -> None)
    |> Array.ofSeq

  let byFertAndSeason () = Array.init fertilizers.Length (fun _ -> Array.init varSeasons.Length (fun _ -> ResizeArray ()))

  let cropVars = byFertAndSeason ()
  let crossVars = byFertAndSeason ()
  let unreplacedFertilizerVars = byFertAndSeason ()
  let noFertilizerVars = Array.init varSeasons.Length (fun _ -> ResizeArray ())
  let regrowFirstSeasonVars = Array.init varSeasons.Length (fun j ->
    (varSeasons[j], DayUsedForRegrowCrop),
    [| string j, 1.0
       RegrowDays, -1.0 |])

  let lossProb =
    if settings.Profit.PayForFertilizer && settings.Profit.ReplaceLostFertilizer then
      let giantProb = Game.giantCropProb settings.Game
      function
      | FarmCrop crop -> if crop.Giant then Fertilizer.lossProbability * giantProb else 0.0
      | ForageCrop _ -> Fertilizer.lossProbability
    else
      konst 0.0

  let netProfit = Query.nonRegrowData data settings

  let crops, regrowCrops = crops |> Array.partition (Crop.regrowTime >> Option.isNone)
  let regrowCrops = regrowCrops |> Array.map (function | FarmCrop crop -> crop | ForageCrop _ -> failwith "unreachable")

  for crop in crops do
    let seed = Crop.seed crop
    let lossProb = lossProb crop
    let profit = netProfit crop

    for i = 0 to fertilizers.Length - 1 do
      let fertilizer, fertCost = fertilizers[i]
      let fertName = Fertilizer.Opt.name fertilizer
      match profit fertilizer with
      | Some net when net > 0.0 ->
        let replacementCost = lossProb * float fertCost
        let growthTime = Game.growthTime settings.Game fertilizer crop
        let profit = Profit, net - replacementCost

        let mutable growsInPrevious = false
        for j = 0 to varSeasons.Length - 1 do
          let varSeason = varSeasons[j]
          if crop |> growsInSeason j then
            if growthTime <= days[j] then
              let time = string j, float growthTime

              let var = (varSeason, PlantCrop (seed, fertName)), [| profit; time |]
              (cropVars[i][j]).Add var

              if lossProb > 0.0 then
                if fertilizer = None then
                  noFertilizerVars[j].Add var
                elif replacementCost > 0.0 then
                  ((varSeason, PlantCropUnreplacedFertilizer (seed, fertName)),
                    [| Profit, net
                       endingCrop
                       time |] )
                  |> (unreplacedFertilizerVars[i][j]).Add

            //TODO? cross season giant/forage crop vars don't allow unreplaced fertilizer
            if growsInPrevious then
              ((varSeason, PlantBridgeCrop (seed, fertName)),
                [| BridgeHarvests @ string j, 1.0
                   BridgeDays @ string j, float growthTime
                   profit |] )
              |> (crossVars[i][j]).Add
            else
              growsInPrevious <- true
          else
            growsInPrevious <- false

      | _ -> ()

  for j = 0 to varSeasons.Length - 2 do
    let var =
      ((varSeasons[j], DayUsedForBridgeCropIntoNextSeason),
        [| string j, 1.0
           BridgeDays @ string (j + 1), -1.0 |] )
    for i = 0 to fertilizers.Length - 1 do
      if (crossVars[i][j + 1]).Count > 0 then
        (crossVars[i][j + 1]).Add var

  for j = 1 to varSeasons.Length - 1 do
    let var =
      ((varSeasons[j], DayUsedForBridgeCropFromPrevSeason),
        [| string j, 1.0
           BridgeDays @ string j, -1.0 |] )
    for i = 0 to fertilizers.Length - 1 do
      if (crossVars[i][j]).Count > 0 then
        (crossVars[i][j]).Add var

  let requests = ResizeArray ()

  let continuationModel constraints vars fertId start stop =
    let _, fertCost = fertilizers[fertId]
    let rec recur start constraints vars =
      let constraints = (string start <== float days[start]) :: constraints
      let vars = cropVars[fertId][start] :: vars

      { Start = start
        Stop = stop
        Model = Model.createAllInteger Maximize Profit constraints (noFertilizerVars[start] :: vars |> Seq.collect id)
        ExtraProfit = -float fertCost }
      |> requests.Add

      if start <> 0 then
        let cross = crossVars[fertId][start]
        if cross.Count > 0 then
          let constraints =
            (BridgeDays @ string start === 1.0)
            :: (BridgeHarvests @ string start === 1.0)
            :: constraints

          recur (start - 1) constraints (cross :: vars)

    recur start constraints vars

  for stop = varSeasons.Length - 1 downto 0 do
    let crops =
      let predicate =
        if stop = varSeasons.Length - 1
        then (fun crop -> crop |> FarmCrop |> growsInSeason stop)
        else (fun crop -> crop |> FarmCrop |> growsInSeason stop && crop |> FarmCrop |> growsInSeason (stop + 1) |> not)
      regrowCrops |> Array.filter predicate

    let fixedRegrowVars = Array.init fertilizers.Length (fun _ -> Array.init (stop + 1) (fun _ -> ResizeArray ()))
    let regrowVars = Array.init fertilizers.Length (fun _ -> Array.init (stop + 1) (fun _ -> ResizeArray ()))
    // let strs = Array.create 4 "" // [| cropName; fertName; startSeasonName; endSeasonName |]

    for crop in crops do
      let seed = crop.Seed
      let regrowData = Query.regrowSeedData data settings (FarmCrop crop)
      let regrowTime = Option.get crop.RegrowTime

      for i = 0 to fertilizers.Length - 1 do
        let fertilizer, _ = fertilizers[i]
        let fertName = Fertilizer.Opt.name fertilizer

        match regrowData fertilizer with
        | None -> ()
        | Some data ->
          let growthTime = Game.growthTime settings.Game fertilizer (FarmCrop crop)

          let rec recur start daysAfterFirstSeason harvestsAfterFirstSeason =
            let totalDays = daysAfterFirstSeason + days[start]
            let harvests =
              if totalDays < growthTime then
                0u
              else
                let maxHarvests = (totalDays - growthTime) / regrowTime + 1u

                for h in harvestsAfterFirstSeason..(min maxHarvests (data.HarvestsForMinCost - 1u)) do
                  match data.Cost h with
                  | None -> ()
                  | Some cost ->
                    let usedDays = growthTime + (h - 1u) * regrowTime
                    ((varSeasons[start], PlantRegrowCropFixedHarvests (seed, fertName, h)),
                      [| Profit, data.Profit * float h - cost
                         string start, float (if daysAfterFirstSeason > usedDays then 0u else usedDays - daysAfterFirstSeason)
                         endingCrop |] )
                    |> (fixedRegrowVars[i][start]).Add

                if maxHarvests >= data.HarvestsForMinCost then
                  let harvests = max data.HarvestsForMinCost harvestsAfterFirstSeason
                  let usedDays = growthTime + regrowTime * (harvests - 1u)
                  let harvestsName = string seed @ string i @ string start @ "Harvests"

                  let firstVar =
                    ((varSeasons[start], PlantRegrowCropFirstHarvest (seed, fertName)),
                      [| Profit, data.Profit * float harvests - data.MinCost
                         RegrowDays, -float(daysAfterFirstSeason - usedDays)
                         harvestsName, -float(maxHarvests - harvests)
                         endingCrop |] )

                  let var =
                    ((varSeasons[start], PlantRegrowCropRegrowHarvests (seed, fertName)),
                      [| Profit, data.Profit
                         harvestsName, 1.0
                         RegrowDays, float regrowTime |] )

                  (regrowVars[i][start]).Add ([| firstVar; var |], harvestsName <== 0.0)

                maxHarvests

            if start > 0 then
              let start = start - 1
              if crop |> FarmCrop |> growsInSeason start then
                recur start (totalDays + 1u) harvests

          recur stop 0u 0u


    for i = 0 to fertilizers.Length - 1 do
      let fixedRegrowVars = fixedRegrowVars[i]
      for j = 0 to stop - 1 do
        let fixedRegrowVars = fixedRegrowVars[j]
        if fixedRegrowVars.Count > 0 then
          continuationModel [ "EndingCrop" === 1.0 ] [ fixedRegrowVars ] i j stop

    for i = 0 to fertilizers.Length - 1 do
      let regrowVars = regrowVars[i]
      for j = 0 to stop - 1 do
        let regrowVars = regrowVars[j]
        if regrowVars.Count > 0 then
          continuationModel
            [ RegrowDays === 0.0; "EndingCrop" <== 1.0; yield! regrowVars |> Seq.map snd ]
            [ ResizeArray ( [| regrowFirstSeasonVars[j] |] ); yield! regrowVars |> Seq.map (fst >> ResizeArray) ]
            i
            j
            stop

    for i = 0 to fertilizers.Length - 1 do
      let vars = [ unreplacedFertilizerVars[i][stop] ]

      let fixedRegrowVars = fixedRegrowVars[i][stop]
      if fixedRegrowVars.Count > 0 then
        let vars = fixedRegrowVars :: vars
        continuationModel [ "EndingCrop" <== 1.0 ] vars i stop stop

      let regrowVars = regrowVars[i][stop]
      if regrowVars.Count > 0 then
        let vars =
          ResizeArray ( [| regrowFirstSeasonVars[stop] |] )
          :: ResizeArray (regrowVars |> Seq.collect fst)
          :: vars
        continuationModel [ RegrowDays <== 0.0; "EndingCrop" <== 1.0; yield! regrowVars |> Seq.map snd ] vars i stop stop

      if fixedRegrowVars.Count = 0 && regrowVars.Count = 0 then
        continuationModel [ "EndingCrop" <== 1.0 ] vars i stop stop

  unbox<SubRangeSolutionRequest array> requests


open Fable.Core.JsInterop

// https://pages.cs.wisc.edu/~shuchi/courses/787-F09/scribe-notes/lec3.pdf

let solveRanges (requests: SubRangeSolutionRequest array) =
  if requests.Length = 0 then
    [||], 0.0
  else
    let requests = requests |> Array.groupBy (fun r -> r.Stop)
    requests |> Array.sortInPlaceBy fst
    let seasonCount = fst requests[requests.Length - 1]

    let bestUpTo = Array.create (seasonCount + 2) ([], 0.0)

    for i = requests.Length - 1 downto 0 do
      let j, requests = requests[i]
      assert (j = i)

      for request in requests do
        let solution = Solver.solve request.Model

        if solution.status = Optimal then
          let prevSolution, prevValue = bestUpTo[request.Stop + 1]
          solution?result <- solution.result + request.ExtraProfit
          let newValue = prevValue + solution.result
          if newValue > snd bestUpTo[request.Start] then
            bestUpTo[request.Start] <- (solution :: prevSolution), newValue

    fst bestUpTo[0] |> Array.ofList, snd bestUpTo[0]


// type EndingCrop =
//   | RegularEnding
//   | UnreplacedGiant
//   | UnreplacedForage
//   | HoneyEnding of Crop * int
//   | RegrowEnding of RegrowCrop name * int

// type FertilizerSpan =
//   { Fertilizer: Fertilizer name option
//     Length: int
//     RegularCrops: (RegularCrop name * int) array array
//     GiantCrops: (GiantCrop name * int) array array
//     ForageCrop: (ForageCrop name * int) array
//     EndingCrop: EndingCrop }
