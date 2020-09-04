namespace StardewValleyStonks

open Types

type Profession =
  { Name: string
    Selected: bool }

type Level10Profession =
  | Pair of Left: NameOf<Profession> * Right: NameOf<Profession>
  | Single of NameOf<Profession>
  | NoLevel10

type ProfessionTree =
  { Level5Profession: NameOf<Profession>
    Level10Profession: Level10Profession }

type Skills =
  | Farming
  | Foraging

module Skills =
  let all =
    [ Farming
      Foraging ]

type Skill =
  { Name: Skills
    Level: int
    Buff: int
    Professions: Map<NameOf<Profession>, Profession>
    TreeOf: Map<NameOf<Profession>, ProfessionTree>
    ProfessionTrees: ProfessionTree list }

module Profession =
  let name (profession: Profession) = profession.Name

  let nameOf = toNameOf name

  let isLevel5Profession skill profession =
    skill.TreeOf.[profession].Level5Profession = profession

  let unlockLevel skill profession =
    if profession |> isLevel5Profession skill
    then 5
    else 10

  let isUnlocked skill profession = unlockLevel skill profession <= skill.Level

  let private forceToggle (profession: Profession) = { profession with Selected = not profession.Selected }

  let private mapSetSelected profession value (professions: Map<NameOf<Profession>, Profession>) =
    professions.Add(profession, { professions.[profession] with Selected = value } )

  let toggle skill ignoreRelationships profession =
    let professions = skill.Professions.Add(profession, forceToggle skill.Professions.[profession])
    if ignoreRelationships then
      { skill with Professions = professions }
    else
      { skill with
          Professions =
            let tree = skill.TreeOf.[profession]
            if professions.[profession].Selected then
              // deselect professions as necessary, select lvl5 profession if necessary
              Map.fold (fun (map: Map<_,_>) name _ ->
                if skill.TreeOf.[name] = tree then
                  match tree.Level10Profession with
                  | NoLevel10 -> map
                  | _ when name = tree.Level5Profession -> map
                  | Single _ -> map |> mapSetSelected tree.Level5Profession true
                  | Pair (l, r) ->
                      // Will trigger twice (there is a left and a right) (although nothing bad happens if it does trigger twice)
                      // Check if this profession was the one that was selected
                      if name = profession then
                        map
                        |> if name = l
                           then mapSetSelected r false
                           else mapSetSelected l false
                        |> mapSetSelected tree.Level5Profession true
                      else
                        // skip one of the triggers
                        map
                else
                  // profession is not in the same tree.
                  // professions in other trees are exclusive with professions in this tree, deselect them
                  map |> mapSetSelected name false)
                professions
                professions

            elif profession = tree.Level5Profession then
              // lvl5 deselected, deselect lvl10 professions
              match tree.Level10Profession with
              | NoLevel10 -> professions
              | Single p -> professions |> mapSetSelected p false
              | Pair (l, r) ->
                  professions
                  |> mapSetSelected l false
                  |> mapSetSelected r false

            else
              //lvl 10 deselected, no action needed
              professions }

  let create name =
    { Name = name
      Selected = false }

module Lvl10Profession =
  let single = Profession.nameOf >> Single

  let pair left right = Pair (Profession.nameOf left, Profession.nameOf right)

module ProfessionTree =
  let create lvl5 lvl10 =
    { Level5Profession = Profession.nameOf lvl5
      Level10Profession = lvl10 }

type CreateLvl10Profession =
  | CreatePair of Left: string * Right: string
  | CreateSingle of string
  | CreateNoLevel10

module Skill =
  let which skill = skill.Name

  let name = which >> string

  let buffedLevel skill = skill.Level + skill.Buff

  let private distribution gold silver =
    let rec helper (dist: Map<_,_>) remainProb (qualities: _ list) = function
      | [] -> dist
      | head::tail ->
          let prob = head * remainProb
          helper (dist |> Map.add qualities.Head prob) (remainProb - prob) qualities.Tail tail
    helper Map.empty 1.0 Quality.common [ gold; silver; 1.0 ]

  let private farmingDist fertQuality farming =
    let buffLevel = buffedLevel farming
    let gold = 0.01 + 0.2 * (float buffLevel / 10.0 + float fertQuality * float (buffLevel + 2) / 12.0)
    distribution
      gold
      (min (2.0 * gold) 0.75)

  let farmingDistributionWith = Fertilizer.quality >> farmingDist
  let farmingDistribution = farmingDist 0
  let farmingDistributionOption = Fertilizer.qualityOfOption >> farmingDist

  let foargeDistribution botanist forage =
    if botanist then
      Quality.single Iridium
    else
      let buffLevel = buffedLevel forage |> float
      distribution
        (buffLevel / 30.0)
        (buffLevel / 15.0)

  let validLevel = clamp 0 10
  let validBuff = positive

  let private addNameOf profession = Map.add (Profession.nameOf profession)
  let private addProfession profession = addNameOf profession profession

  let private createSkill name createTrees =
    let professions, treeMap, treeList =
      Seq.fold (fun (profs, tMap, trees) (level5Profession, level10Profession) ->
        let tree, createdProfessions =
          let lvl5 = Profession.create level5Profession
          match level10Profession with
          | CreateNoLevel10 ->
              ProfessionTree.create lvl5 NoLevel10,
              [ lvl5 ]
          | CreateSingle p ->
              let lvl10 = Profession.create p
              ProfessionTree.create lvl5 (Lvl10Profession.single lvl10),
              [ lvl5; lvl10 ]
          | CreatePair (l, r) ->
              let left = Profession.create l
              let right = Profession.create r
              ProfessionTree.create lvl5 (Lvl10Profession.pair left right),
              [ lvl5; left; right ]
        
        List.fold (fun ps profession -> ps |> addProfession profession) profs createdProfessions,
        List.fold (fun tm profession -> tm |> addNameOf profession tree) tMap createdProfessions,
        tree::trees)

        (Map.empty, Map.empty, [])
        createTrees

    { Name = name
      Level = 0
      Buff = 0
      Professions = professions
      TreeOf = treeMap
      ProfessionTrees = treeList }

  let create name tree = createSkill name [ tree ]
  let create2 name tree1 tree2 = createSkill name [ tree1; tree2 ]

type SkillUnlock =
  { Skill: Skills
    UnlockLevel: int }

module SkillUnlock =
  let create skill level =
    { Skill = skill
      UnlockLevel = level }

  let seedMaker = create Farming 9