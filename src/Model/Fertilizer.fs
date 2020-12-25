namespace StardewValleyStonks

open FSharp.Data.Adaptive
open Adaptive
open Util

[<CustomEquality; CustomComparison>]
type Fertilizer =
  { Name: string
    Quality: int
    Distribution: QualityDistribution
    Speed: float
    Prices: Map<string, Price>
    BestPrice: int option aval
    BestSources: string alist
    Valid: bool aval }
  override this.Equals(x) =
    match x with
    | :? Fertilizer as fert -> this.Name = fert.Name
    | _ -> false
  interface System.IEquatable<Fertilizer> with
    member this.Equals(x) = this.Name = x.Name
  interface System.IComparable with
    member this.CompareTo(x) =
      match x with
      | :? Fertilizer as fert -> compare this.Name fert.Name
      | _ -> invalidArg "x" "x is not a fertilizer."
  interface System.IComparable<Fertilizer> with
    member this.CompareTo(x) = compare this.Name x.Name
  override this.GetHashCode() = hash this.Name

type FertilizerSort =
  | FertName
  | Quality
  | Speed
  | Price

module Fertilizer =
  let lossProb = 0.1
  let accountForCost = cval true
  let accountForReplacement = cval true

  let name fertilizer = fertilizer.Name
  let nameOption = defaultMap "No Fertilizer" name
  let quality fertilizer = fertilizer.Quality
  let qualityOption = defaultMap 0 quality
  let distribution fertilizer = fertilizer.Distribution
  let distributionOption = defaultMap Skills.farming.QualityDistribution distribution
  let speed fertilizer = fertilizer.Speed
  let speedOption = defaultMap 0.0 speed
  let prices fertilizer = fertilizer.Prices
  let bestPrice fertilizer = fertilizer.BestPrice
  let bestSources fertilizer = fertilizer.BestSources
  let valid fertilizer = fertilizer.Valid

  let validQuality: int -> _ = positive
  let validSpeed: float -> _ = positive

  let create name quality speed prices =
    let q = validQuality quality
    let bestPrice, bestSources = Price.bestPriceAndSources prices
    { Name = name
      Quality = q
      Distribution =
        if q = 0
        then Skills.farming.QualityDistribution
        else Skills.farmingDistributionWith q
      Speed = validSpeed speed
      Prices = mapOfValues Price.source prices
      BestPrice = bestPrice
      BestSources = bestSources
      Valid = !%Option.isSome bestPrice .|| aNot accountForCost }

module Fertilizers =
  open Fertilizer

  let all, sources =
    let sourceSelected, usedSources = trackUsed (fun _ -> cval true) Sources.all
    let price = Price.createWith sourceSelected

    [ create
        "Basic Fertilizer"
        1
        0.0
        [ price 100 "Pierre" ]
      create
        "Quality Fertilizer"
        2
        0.0
        [ price 150 "Pierre" ]
      create
        "Deluxe Fertilizer"
        3
        0.0
        []
      create
        "Speed-Gro"
        0
        0.1
        [ price 100 "Pierre" ]
      create
        "Deluxe Speed-Gro"
        0
        0.25
        [ price 150 "Pierre"
          price 80 "Oasis" ]
      create
        "Hyper Speed-Gro"
        0
        0.33
        [] ]
    |> List.sortBy name,
    usedSources()

  let allOption = listWithNone all

  let aAll = AList.ofList all

  let fastestFert = aAll |> AList.filterA valid |> AList.map speed |> AList.tryMax
