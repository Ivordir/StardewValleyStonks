namespace StardewValleyStonks

open Fable.Core

type [<Erase>] Vendor = VendorName of string

module Vendor =
  let pierre = VendorName "Pierre"
  let joja = VendorName "Joja"
  let oasis = VendorName "Oasis"
  let travelingCart = VendorName "Traveling Cart"


type [<Erase>] FertilizerName = FertName of string

type Fertilizer = {
  Name: FertilizerName
  Quality: nat
  Speed: float
}

module Fertilizer =
  let [<Literal>] lossProbability = 0.1
  let [<Literal>] minSpeed = 0.0

  let name fertilizer = fertilizer.Name
  #if FABLE_COMPILER
  let nameStr fertilizer = !!fertilizer.Name: string
  #else
  let nameStr fertilizer =
    let (FertName name) = fertilizer.Name
    name
  #endif
  let quality fertilizer = fertilizer.Quality
  let speed fertilizer = fertilizer.Speed

  module Opt =
    let name = Option.map name
    let quality = Option.defaultOrMap 0u quality
    let speed = Option.defaultOrMap 0.0 speed




type Quality =
  | Normal = 0
  | Silver = 1
  | Gold = 2
  | Iridium = 3

module Quality =
  let [<Literal>] highest = 3
  let [<Literal>] count = 4

  let name (quality: Quality) = enumName quality

  let all = Array.init count enum<Quality>


type [<Erase>] Qualities = ByQuality of float array

module Qualities =
  #if FABLE_COMPILER
  let inline unwrap (qualities: Qualities) = !!qualities : float array
  #else
  let inline unwrap (ByQuality arr) = arr
  #endif

  let inline create value = Array.create Quality.count value |> ByQuality
  let normalSingleton normal =
    let qualities = Array.zeroCreate Quality.count
    qualities[0] <- normal
    ByQuality qualities
  let inline init initializer = Array.init Quality.count (enum<Quality> >> initializer) |> ByQuality
  let inline initi initializer = Array.init Quality.count initializer |> ByQuality

  let inline wrap arr = ByQuality arr
  let inline ofArray arr = Array.copy arr |> ByQuality

  let zero = create 0.0
  let one = create 1.0
  let multipliers = ByQuality [| 1.0; 1.25; 1.5; 2.0 |]

  let inline item (quality: Quality) qualities = unwrap qualities |> Array.item (int quality)
  let inline itemi index qualities = unwrap qualities |> Array.item index

  let inline updateAt index value qualities =
    unwrap qualities |> Array.updateAt index value |> ByQuality
  let inline updateQuality (quality: Quality) value amounts =
    updateAt (int quality) value amounts

  let inline map mapping qualities = unwrap qualities |> Array.map mapping |> ByQuality
  let inline map2 mapping a b = Array.map2 mapping (unwrap a) (unwrap b) |> ByQuality

  let inline sum qualities = unwrap qualities |> Array.sum

  let dot a b =
    let mutable sum = 0.0
    for i = 0 to Quality.highest do
      sum <- sum + itemi i a * itemi i b
    sum

  let inline toArray qualities = unwrap qualities |> Array.copy
  let inline toSeq qualities = unwrap qualities |> Array.toSeq

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

type Qualities with
  member inline this.Item quality = Qualities.item quality this
  member inline this.Item index = Qualities.itemi index this


type Skill<'professions> = {
  Level: nat
  Buff: nat
  Professions: 'professions
}

type Farming = Skill<{| Tiller: bool; Agriculturist: bool; Artisan: bool |}>

type Foraging = Skill<{| Gatherer: bool; Botanist: bool |}>

module Skill =
  let level skill = skill.Level
  let buff skill = skill.Buff
  let professions skill = skill.Professions
  let buffedLevel skill = skill.Level + skill.Buff


type Skills = {
  Farming: Farming
  Foraging: Foraging
  IgnoreSkillLevelRequirements: bool
  IgnoreProfessionConflicts: bool
}

module Skills =
  let [<Literal>] tillerUnlockLevel = 5u
  let [<Literal>] artisanUnlockLevel = 10u
  let [<Literal>] agriculturistUnlockLevel = 10u

  let [<Literal>] gathererUnlockLevel = 5u
  let [<Literal>] botanistUnlockLevel = 10u

  let farmingLevelMet level skills = skills.IgnoreSkillLevelRequirements || skills.Farming.Level >= level

  let tillerLevelMet skills = farmingLevelMet tillerUnlockLevel skills
  let artisanLevelMet skills = farmingLevelMet artisanUnlockLevel skills
  let agriculturistLevelMet skills = farmingLevelMet agriculturistUnlockLevel skills

  let tillerActive skills = skills.Farming.Professions.Tiller && tillerLevelMet skills
  let artisanActive skills = skills.Farming.Professions.Artisan && artisanLevelMet skills
  let agriculturistActive skills = skills.Farming.Professions.Agriculturist && agriculturistLevelMet skills


  let foragingLevelMet level skills = skills.IgnoreSkillLevelRequirements || skills.Foraging.Level >= level

  let gathererLevelMet skills = foragingLevelMet gathererUnlockLevel skills
  let botanistLevelMet skills = foragingLevelMet botanistUnlockLevel skills

  let gathererActive skills = skills.Foraging.Professions.Gatherer && gathererLevelMet skills
  let botanistActive skills = skills.Foraging.Professions.Botanist && botanistLevelMet skills


  open type Quality

  let private farmingQualitiesCalc fertQuality skills =
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

  let farmingQualitiesWith fert skills = farmingQualitiesCalc (Fertilizer.Opt.quality fert) skills
  let farmingQualitiesFrom fert skills = farmingQualitiesCalc fert.Quality skills
  let inline farmingQualities skills = farmingQualitiesWith None skills

  let foragingQualities skills =
    if botanistActive skills then
      Qualities.wrap [| 0.0; 0.0; 0.0; 1.0 |]
    else
      let buffLevel = Skill.buffedLevel skills.Foraging |> float
      Qualities.ifElseDistribution [|
        Gold, buffLevel / 30.0
        Silver, buffLevel / 15.0
        Normal, 1.0
      |]

  let foragingAmounts skills =
    let qualities = foragingQualities skills
    if gathererActive skills
    then qualities |> Qualities.map ((*) 1.2)
    else qualities
