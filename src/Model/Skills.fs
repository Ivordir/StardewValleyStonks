namespace StardewValleyStonks

open Fable.Core

type [<Erase>] Vendor = VendorName of string

module Vendor =
  let pierre = VendorName "Pierre"
  let joja = VendorName "Joja"
  let oasis = VendorName "Oasis"


type FertilizerName = string

type Fertilizer = {
  Name: FertilizerName
  Quality: nat
  Speed: float
}

module [<RequireQualifiedAccess>] Fertilizer =
  let [<Literal>] lossProbability = 0.1
  let [<Literal>] minSpeed = 0.0

  let name fertilizer = fertilizer.Name
  let quality fertilizer = fertilizer.Quality
  let speed fertilizer = fertilizer.Speed

  module Opt =
    let name = Option.map name
    let quality = Option.defaultOrMap 0u quality
    let speed = Option.defaultOrMap 0.0 speed



module Multiplier =
  let [<Literal>] bearsKnowledge = 3.0
  let [<Literal>] tiller = 1.1
  let [<Literal>] artisan = 1.4
  let [<Literal>] agriculturist = 0.1
  let [<Literal>] gatherer = 1.2
  let [<Literal>] irrigated = 0.25



type Quality =
  | Normal = 0
  | Silver = 1
  | Gold = 2
  | Iridium = 3

module [<RequireQualifiedAccess>] Quality =
  let [<Literal>] highest = 3
  let [<Literal>] count = 4

  let name (quality: Quality) = enumName quality

  let all = Array.init count enum<Quality>

type [<Erase>] 'a Qualities = ByQuality of 'a array

module [<RequireQualifiedAccess>] Qualities =
  #if FABLE_COMPILER
  let inline unwrap (qualities: 'a Qualities) = Fable.Core.JsInterop.(!!)qualities : 'a array
  #else
  let inline unwrap (ByQuality arr) = arr
  #endif

  let inline create value = Array.create Quality.count value |> ByQuality

  let inline init initializer = Array.init Quality.count (enum<Quality> >> initializer) |> ByQuality

  let inline wrap arr = ByQuality arr

  let zero = create 0.0
  let multipliers = ByQuality [| 1.0; 1.25; 1.5; 2.0 |]

  let inline item (quality: Quality) qualities = (unwrap qualities)[int quality]
  let inline private itemi index qualities = (unwrap qualities)[index]

  let inline updateQuality (quality: Quality) value qualities =
    unwrap qualities |> Array.updateAt (int quality) value |> ByQuality

  let inline map mapping qualities = unwrap qualities |> Array.map mapping |> ByQuality
  let inline map2 mapping a b = Array.map2 mapping (unwrap a) (unwrap b) |> ByQuality

  let inline toArray qualities = unwrap qualities |> Array.copy

  let inline sum qualities = unwrap qualities |> Array.sum

  let mult (scalar: float) qualities =
    let arr = toArray qualities
    for i = 0 to Quality.highest do
      arr[i] <- scalar * arr[i]
    wrap arr

  let dot a b =
    let mutable sum = 0.0
    for i = 0 to Quality.highest do
      sum <- sum + itemi i a * itemi i b
    sum


  // Models the probabilities that result from a chain of if-else statements.
  // E.g. given that p(x) is the raw probability of the check on x succeeding:
  // if p(x1) then
  //   x1
  // elif p(x2) then
  //   x2
  // elif p(x3) then
  //   x3
  // else // p(x4) = 1
  //   x4
  //
  // The actual probability of x_n, P(x_n), depends on all previous branches not happening:
  // P(x_n) = p(x_n) * (1 - p(x_(n-1))) * (1 - p(x_(n-2))) * ... * (1 - p(x1)):
  //
  // P(x1) = p(x1)
  //
  // P(x2) = p(x2) * (1 - p(x1))
  //
  // P(x3) = p(x3) * (1 - p(x2)) * (1 - p(x1))
  //
  // P(x4) = 1 * (1 - p(x3)) * (1 - p(x2)) * (1 - p(x1))
  //
  // Assumes each quality only appears once in `probabilities`.
  let ifElseDistribution (probabilities: (Quality * float) array) =
    let dist = Array.zeroCreate Quality.count
    let mutable runningProb = 1.0
    for quality, rawProb in probabilities do
      let prob = runningProb * min 1.0 rawProb
      dist[int quality] <- prob
      runningProb <- runningProb - prob // = runningProb * (1.0 - rawProb)
    assert (abs (Array.sum dist - 1.0) < 1e-10)
    wrap dist

type 'a Qualities with
  member inline this.Item quality = Qualities.item quality this


type Skill = {
  Level: nat
  Buff: nat
}

module [<RequireQualifiedAccess>] Skill =
  let [<Literal>] maxLevel = 10u

  let zero = {
    Level = 0u
    Buff = 0u
  }

  let buffedLevel skill = skill.Level + skill.Buff


type Profession =
  | Tiller
  | Artisan
  | Agriculturist
  | Gatherer
  | Botanist


type Skills = {
  Farming: Skill
  Foraging: Skill
  Professions: Profession Set
  IgnoreSkillLevelRequirements: bool
  IgnoreProfessionConflicts: bool
}

module Skills =
  let zero = {
    Farming = Skill.zero
    Foraging = Skill.zero
    Professions = Set.empty
    IgnoreProfessionConflicts = false
    IgnoreSkillLevelRequirements = false
  }

  let professionUnlocked profession skills =
    match profession with
    | _ when skills.IgnoreSkillLevelRequirements -> true
    | Tiller -> skills.Farming.Level >= 5u
    | Artisan | Agriculturist -> skills.Farming.Level >= 10u
    | Gatherer -> skills.Foraging.Level >= 5u
    | Botanist -> skills.Foraging.Level >= 10u

  let professionActive profession skills =
    skills.Professions.Contains profession && professionUnlocked profession skills

  open type Quality

  let private farmCropQualitiesCalc fertQuality skills =
    let buffLevel = Skill.buffedLevel skills.Farming
    let gold = 0.01 + 0.2 * (float buffLevel / 10.0 + float fertQuality * float (buffLevel + 2u) / 12.0)
    let probabilities =
      if fertQuality >= 3u then [|
        Iridium, gold / 2.0
        Gold, gold
        Silver, 1.0
      |] else [|
        Gold, gold
        Silver, min (2.0 * gold) 0.75
        Normal, 1.0
      |]
    Qualities.ifElseDistribution probabilities

  let farmCropQualitiesWith fert skills = farmCropQualitiesCalc (Fertilizer.Opt.quality fert) skills

  let inline farmCropQualities skills = farmCropQualitiesWith None skills

  let forageCropQualities skills =
    if skills |> professionActive Botanist then
      Qualities.wrap [| 0.0; 0.0; 0.0; 1.0 |]
    else
      let buffLevel = Skill.buffedLevel skills.Foraging |> float
      Qualities.ifElseDistribution [|
        Gold, buffLevel / 30.0
        Silver, buffLevel / 15.0
        Normal, 1.0
      |]

  let forageCropHarvestAmounts skills =
    let qualities = forageCropQualities skills
    if skills |> professionActive Gatherer
    then qualities |> Qualities.mult Multiplier.gatherer
    else qualities
