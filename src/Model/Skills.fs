namespace StardewValleyStonks

[<Fable.Core.Erase>]
type NameOf<'a> = Name of string
and 'a name = NameOf<'a>

// Common functions specific to StardewValleyStonks.
[<AutoOpen>]
module Common =
  let inline (!&) s = Name s
  let inline (!@) (Name s) = s

  let inline intMulti multiplier (value: int) = multiplier * float value |> int



type Quality =
  | Normal = 0
  | Silver = 1
  | Gold = 2
  | Iridium = 3

module Quality =
  let multiplier = function
    | Quality.Normal -> 1.0
    | Quality.Silver -> 1.25
    | Quality.Gold -> 1.5
    | Quality.Iridium -> 2.0
    | _ -> invalidArg "quality" "That was not defined quality."

  let multiply = multiplier >> intMulti

  let color = function
    | Quality.Normal -> "white"
    | Quality.Iridium -> "purple"
    | quality -> System.Enum.GetName(typeof<Quality>, quality) |> lower

  let all =
    [ Quality.Normal
      Quality.Silver
      Quality.Gold
      Quality.Iridium ]



[<Fable.Core.Erase>]
type Profession = ProfessionName of string

[<Fable.Core.StringEnum>]
type Skills =
  | [<CompiledName("Farming")>] Farming
  | [<CompiledName("Foraging")>] Foraging

type Skill =
  { Which: Skills
    Level: int
    Buff: int
    Lvl5Profession: Profession
    Lvl10ProfessionA: Profession
    Lvl10ProfessionB: Profession option
    SelectedProfessions: Profession Set }

module Skill =
  let which skill = skill.Which
  let name = which >> string
  let level skill = skill.Level
  let buff skill = skill.Buff
  let buffedLevel skill = skill.Level + skill.Buff

  let professionUnlocked skill profession =
    if profession = skill.Lvl5Profession
    then skill.Level >= 5
    else skill.Level = 10

  let professionSelected skill profession =
    skill.SelectedProfessions.Contains profession

  let professionActive skill profession =
    professionSelected skill profession
    && professionUnlocked skill profession

  // Models the probabilities that result from a chain of if-else statements.
  // E.g. given that p(x) is the raw probability of the check on x succeeding:
  // if p(A) then
  //   A
  // elif p(B) then
  //   B
  // elif p(C) then
  //   C
  // else // p(D) = 1
  //   D
  //
  // The actual probability of A, !P(A), is equal to p(A).
  // But for any other branches, their probability is multiplied by the probability of all the previous branches not happening:
  //
  // !P(A) = p(A)
  //
  // !P(B) = p(B) * (1 - p(A))
  //
  // !P(C) = p(C) * (1 - p(B)) * (1 - p(A))
  //
  // !P(D) = 1 * (1 - p(C)) * (1 - p(B)) * (1 - p(A))
  //
  let private ifElseDistribution probabilities =
    let dist = Array.zeroCreate 4
    let mutable runningProb = 1.0
    for quality: Quality, rawProb in probabilities do
      let prob = min 1.0 rawProb
      dist.[int quality] <- runningProb * prob
      runningProb <- runningProb * (1.0 - prob)
    dist

  let private farmingDistributionCalc buffLevel fertQuality =
    let gold = 0.01 + 0.2 * (float buffLevel / 10.0 + float fertQuality * float (buffLevel + 2) / 12.0)
    if fertQuality >= 3 then
      ifElseDistribution
        [ Quality.Iridium, gold / 2.0
          Quality.Gold, gold
          Quality.Silver, 1.0 ]
    else
      ifElseDistribution
        [ Quality.Gold, gold
          Quality.Silver, min (2.0 * gold) 0.75
          Quality.Normal, 1.0 ]

  let farmingDistribution = buffedLevel >> farmingDistributionCalc

  let private botanistDistribution = ifElseDistribution [ Quality.Iridium, 1.0 ]
  let private foragingDistributionCalc buffLevel =
    ifElseDistribution
      [ Quality.Gold, float buffLevel / 30.0
        Quality.Silver, float buffLevel / 15.0
        Quality.Normal, 1.0 ]

  let foragingDistribution professionActive foraging =
    if professionActive foraging foraging.Lvl10ProfessionA
    then botanistDistribution
    else buffedLevel foraging |> foragingDistributionCalc

  let foragingAmounts professionActive foraging =
    foragingDistribution professionActive foraging
    |>  if professionActive foraging foraging.Lvl5Profession
        then Array.map ((*) 1.2)
        else id

  let qualityDistribution professionActive skill =
    match skill.Which with
    | Farming -> farmingDistribution skill 0
    | Foraging -> foragingDistribution professionActive skill

  let validLevel = clamp 0 10
  let validBuff = positive

  let setLevel level skill =
    { skill with Level = validLevel level }

  let setBuff buff skill =
    { skill with Buff = validBuff buff }

  let toggleSingleProfession profession skill =
    { skill with SelectedProfessions = skill.SelectedProfessions |> Set.addOrRemove profession }

  let toggleProfession profession skill =
    let selection = skill.SelectedProfessions
    let newSelection =
      if profession = skill.Lvl5Profession then
        if selection.Contains profession then
          // lvl5 will be deselected, deselect lvl10 professions as well
          selection
          |> Set.remove profession
          |> Set.remove skill.Lvl10ProfessionA
          |> Option.foldBack Set.remove skill.Lvl10ProfessionB
        else
          selection |> Set.add profession
      elif not <| selection.Contains profession then
        // lvl10 will be selected, deselect other lvl10 professions if necessary and select lvl5 profession
        if profession = skill.Lvl10ProfessionA then
          selection
          |> Set.add skill.Lvl5Profession
          |> Set.add profession
          |> Option.foldBack Set.remove skill.Lvl10ProfessionB
        else
          selection
          |> Set.add skill.Lvl5Profession
          |> Set.remove skill.Lvl10ProfessionA
          |> Set.add profession
      else
        selection |> Set.remove profession

    { skill with SelectedProfessions = newSelection }

  let create which lvl5 lvl10A lvl10B =
    { Which = which
      Level = 0
      Buff = 0
      Lvl5Profession = ProfessionName lvl5
      Lvl10ProfessionA = ProfessionName lvl10A
      Lvl10ProfessionB = Option.map ProfessionName lvl10B
      SelectedProfessions = Set.empty }


[<Fable.Core.Erase>]
type SkillUnlock = SkillUnlock of skill: Skills * level: int

module SkillUnlock =
  let inline skill (SkillUnlock(skill, _)) = skill
  let inline level (SkillUnlock(_, level)) = level
  let inline unlocked skills (SkillUnlock(skill, level)) = skills skill |> Skill.level >= level
