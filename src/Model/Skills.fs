namespace StardewValleyStonks

open FSharp.Data.Adaptive

open Adaptive
open Util

type Quality =
  | Iridium
  | Gold
  | Silver
  | Normal

type QualityDistribution = Map<Quality, float aval>

module Quality =
  let multiplier = function
    | Normal -> 1.0
    | Silver -> 1.25
    | Gold -> 1.5
    | Iridium -> 2.0

  let color = function
    | Normal -> "white"
    | Iridium -> "purple"
    | x -> string x |> lower

  let multiply = multiplier >> apply

  let all =
    [ Normal
      Silver
      Gold
      Iridium ]



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



module Settings =
  let skillLevelPolicy = cval Warn
  let ignoreSkillUnlocks = skillLevelPolicy .<> !@Enforce

  let validLuckBuff: int -> _ = positive

  let specialCharm = cval false
  let luckBuff = cval 0



type Profession =
  { Name: string
    Selected: bool cval
    UnlockLevel: int
    Unlocked: bool aval
    Active: bool aval }

type Skill =
  { Name: string
    Level: int cval
    Buff: int cval
    BuffedLevel: int aval
    QualityDistribution: QualityDistribution
    Lvl5Profession: Profession
    Lvl10ProfessionA: Profession
    Lvl10ProfessionB: Profession option }

module Profession =
  let ignoreRelationships = cval false

  let name (profession: Profession) = profession.Name
  let selected profession = profession.Selected
  let unlockLevel profession = profession.UnlockLevel
  let unlocked profession = profession.Unlocked
  let active profession = profession.Active

  let validLevel = clamp 0 10

  let private setSelected = selected >> (<~)
  let private trySetSelected = optionDo setSelected
  let private forceToggle = selected >> (fun b -> b <~ not b.Value)

  let toggle skill profession () =
    transact (fun () ->
      profession |> forceToggle
      if not ignoreRelationships.Value then
        if profession = skill.Lvl5Profession then
          if not profession.Selected.Value then
            // lvl5 deselected, deselect lvl10 professions
            setSelected skill.Lvl10ProfessionA false
            trySetSelected skill.Lvl10ProfessionB false
        else if profession.Selected.Value then
          // lvl10 selected, deselect other lvl10 professions if necessary, select lvl5 profession
          setSelected skill.Lvl5Profession true
          if profession = skill.Lvl10ProfessionA then
            trySetSelected skill.Lvl10ProfessionB false
          else
            setSelected skill.Lvl10ProfessionA false)

  let private createAtLevel unlockLevel name skillLevel =
    let unlock = validLevel unlockLevel
    let selected = cval false
    let unlocked = !@unlock .<= skillLevel
    { Name = name
      Selected = selected
      UnlockLevel = unlock
      Unlocked = unlocked
      Active = selected .&& (Settings.ignoreSkillUnlocks .|| unlocked) }

  let createLvl5 = createAtLevel 5
  let createLvl10 = createAtLevel 10



module Skill =
  let name skill = skill.Name
  let level skill = skill.Level
  let buff skill = skill.Buff

  let validLevel = clamp 0 10
  let validBuff: int -> _ = positive

  let setLevel = level >> setValue
  let setBuff = buff >> setValue

module Skills =
  let private distribution iridium gold silver =
    let rec helper dist remainProb = function
      | [] -> dist
      | (q, p)::tail ->
          let prob = (aMin !@1.0 p) .* remainProb
          helper ((q, prob)::dist) (remainProb .- prob) tail
    helper [] !@1.0 [ Iridium, iridium; Gold, gold; Silver, silver; Normal, !@1.0 ]
    |> Map.ofList

  open Profession

  let farming, farmingDistributionWith =
    let level = cval 0
    let buff = cval 0
    let buffedLevel = level .+ buff

    let gold fertQuality = AVal.map (fun buffedLevel ->
      0.01 + 0.2 * (float buffedLevel / 10.0 + float fertQuality * float (buffedLevel + 2) / 12.0))

    let silver = AVal.map (fun gold ->
      min (2.0 * gold) 0.75)

    let dist fertQuality =
      let g = gold fertQuality buffedLevel
      if fertQuality >= 3
      then distribution (g ./ !@2.0) g !@1.0
      else distribution !@0.0 g (silver g)

    let tiller = createLvl5 "Tiller" level
    let artisan = createLvl10 "Artisan" level
    let agri = createLvl10 "Agriculturist" level
    
    { Name = "Farming"
      Level = level
      Buff = buff
      BuffedLevel = buffedLevel
      QualityDistribution = dist 0
      Lvl5Profession = tiller
      Lvl10ProfessionA = artisan
      Lvl10ProfessionB = Some agri },
    dist

  let foraging, foragingAmounts =
    let level = cval 0
    let buff = cval 0
    let buffedLevel = level .+ buff

    let gatherer = createLvl5 "Gatherer" level
    let botanist = createLvl10 "Botanist" level

    let gathererAmount amount =
      adaptive {
        let! active = gatherer.Active
        if active
        then return! amount .* !@1.2
        else return! amount }

    let iridium = botanist.Active |> aCond 1.0 0.0
    let b = aFloat buffedLevel
    let foragingDistribution = distribution iridium (b ./ !@30.0) (b ./ !@15.0)
    let amounts = foragingDistribution |> mapUsingValues gathererAmount
    
    { Name = "Foraging"
      Level = level
      Buff = buff
      BuffedLevel = buffedLevel
      QualityDistribution = foragingDistribution
      Lvl5Profession = gatherer
      Lvl10ProfessionA = botanist
      Lvl10ProfessionB = None },
    amounts

  let all =
    [ farming
      foraging ]



type SkillUnlock =
  { Skill: Skill
    UnlockLevel: int
    Unlocked: bool aval }

module SkillUnlock =
  let skill skillUnlock = skillUnlock.Skill
  let unlockLevel skillUnlock = skillUnlock.UnlockLevel
  let unlocked skillUnlock = skillUnlock.Unlocked

  let create skill unlockLevel =
    let level = Skill.validLevel unlockLevel
    { Skill = skill
      UnlockLevel = level
      Unlocked =  Settings.ignoreSkillUnlocks .|| (!@level .<= skill.Level) }
