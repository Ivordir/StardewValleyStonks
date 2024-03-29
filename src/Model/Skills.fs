namespace StardewValleyStonks

type Vendor = string

module Vendor =
  let [<Literal>] pierre = "Pierre"
  let [<Literal>] joja = "Joja"
  let [<Literal>] oasis = "Oasis"


type FertilizerName = string

type Fertilizer = {
  Name: FertilizerName
  Quality: nat
  Speed: float
}

[<RequireQualifiedAccess>]
module Fertilizer =
  let [<Literal>] destroyProbability = 0.1
  let [<Literal>] minSpeed = 0.0

  module Opt =
    let name = Option.map _.Name
    let quality = Option.defaultOrMap 0u _.Quality
    let speed = Option.defaultOrMap 0.0 _.Speed


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

[<RequireQualifiedAccess>]
module Quality =
  let [<Literal>] highest = 3
  let [<Literal>] count = 4

  let name (quality: Quality) = Enum.name quality


type 'a Qualities = ByQuality of 'a array

[<RequireQualifiedAccess>]
module Qualities =
  let inline create value = Array.create Quality.count value |> ByQuality

  let inline init initializer = Array.init Quality.count (enum<Quality> >> initializer) |> ByQuality

  let inline wrap arr = ByQuality arr

  let zero = create 0.0
  let multipliers = ByQuality [| 1.0; 1.25; 1.5; 2.0 |]

  let inline item (quality: Quality) (ByQuality qualities) = qualities[int quality]
  let inline private itemi index (ByQuality qualities) = qualities[index]

  let inline updateQuality (quality: Quality) value (ByQuality qualities) =
    qualities |> Array.updateAt (int quality) value |> ByQuality

  let inline addNormal value qualities =
    let value = value + (qualities |> item Quality.Normal)
    qualities |> updateQuality Quality.Normal value

  let inline map mapping (ByQuality qualities) = qualities |> Array.map mapping |> ByQuality
  let inline map2 mapping (ByQuality a) (ByQuality b) = Array.map2 mapping a b |> ByQuality

  let indexed (ByQuality qualities) = qualities |> Array.mapi (fun i x -> enum<Quality> i, x)
  let inline toArray (ByQuality qualities) = qualities |> Array.copy

  let inline sum (ByQuality qualities) = qualities |> Array.sum

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

type 'a Qualities with
  member inline this.Item quality = Qualities.item quality this


type Skill = {
  Level: nat
  Buff: nat
}

[<RequireQualifiedAccess>]
module Skill =
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

module Profession =
  let farmingProfessions = [| Tiller; Artisan; Agriculturist |]
  let foragingProfessions = [| Gatherer; Botanist |]


type Skills = {
  Farming: Skill
  Foraging: Skill
  Professions: Profession Set
  IgnoreSkillLevelRequirements: bool
  IgnoreProfessionConflicts: bool
}

[<RequireQualifiedAccess>]
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
    professionUnlocked profession skills && skills.Professions.Contains profession

  open type Quality

  (*
  Calculates the probabilities that result from a chain of if-else statements.
  E.g. given that p(x) is the raw probability of the check on x succeeding:
  if p(x1) then
    x1
  elif p(x2) then
    x2
  elif p(x3) then
    x3
  else // p(x4) = 1
    x4

  The actual probability of x_n, P(x_n), depends on all previous branches not happening:
  P(x_n) = p(x_n) * (1 - p(x_(n-1))) * (1 - p(x_(n-2))) * ... * (1 - p(x1)):

  P(x1) = p(x1)

  P(x2) = p(x2) * (1 - p(x1))

  P(x3) = p(x3) * (1 - p(x2)) * (1 - p(x1))

  P(x4) = 1 * (1 - p(x3)) * (1 - p(x2)) * (1 - p(x1))

  Assumes each quality only appears once in `probabilities`.
  *)
  let private ifElseDistribution (probabilities: (Quality * float) array) =
    let dist = Array.zeroCreate Quality.count
    let mutable currentProb = 1.0
    for quality, rawProb in probabilities do
      let prob = currentProb * min 1.0 rawProb
      dist[int quality] <- prob
      currentProb <- currentProb - prob // = currentProb * (1.0 - rawProb)
    assert (abs (Array.sum dist - 1.0) < 1e-10)
    Qualities.wrap dist

  let farmCropQualitiesFrom fertQuality skills =
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
    ifElseDistribution probabilities

  let farmCropProbsWith fertilizer skills = farmCropQualitiesFrom (Fertilizer.Opt.quality fertilizer) skills

  let inline farmCropProbs skills = farmCropProbsWith None skills

  let forageCropProbs skills =
    if skills |> professionActive Botanist then
      Qualities.wrap [| 0.0; 0.0; 0.0; 1.0 |]
    else
      let buffLevel = Skill.buffedLevel skills.Foraging |> float
      ifElseDistribution [|
        Gold, buffLevel / 30.0
        Silver, buffLevel / 15.0
        Normal, 1.0
      |]

  let forageCropHarvestQuantities skills =
    let probs = forageCropProbs skills
    if skills |> professionActive Gatherer
    then probs |> Qualities.mult Multiplier.gatherer
    else probs
