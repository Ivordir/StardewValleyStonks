namespace StardewValleyStonks

open Types

type Profession =
  { Name: string
    Selected: bool
    UnlockLevel: int
    Requires: Set<NameOf<Profession>>
    ExclusiveWith: Set<NameOf<Profession>>
    Dependants: Set<NameOf<Profession>> }

type Skill =
  { Name: string
    Level: int
    Buff: int
    Professions: Map<NameOf<Profession>, Profession>
    ProfessionLayout: NameOf<Profession> list list }

module Profession =
  let initial =
    { Name = "initial"
      Selected = false
      UnlockLevel = 10
      Requires = Set.empty
      ExclusiveWith = Set.empty
      Dependants = Set.empty }

  let name (profession: Profession) = profession.Name

  let nameOf = toNameOf name

  let isUnlocked skill profession = skill.Professions.[profession].UnlockLevel <= skill.Level

  let private setSelected value set (professions: Map<NameOf<Profession>, Profession>) =
    professions
    |> Map.map (fun name profession ->
      if set |> Set.contains name then
        { profession with Selected = value }
      else
        profession)

  let toggleIgnoreRelationships profession = { profession with Selected = not profession.Selected }

  let toggle skill ignoreRelationships profession =
    let professions = skill.Professions.Add(profession, toggleIgnoreRelationships skill.Professions.[profession])
    if ignoreRelationships then
      { skill with Professions = professions }
    else
      let profession = professions.[profession]
      { skill with
          Professions =
            if profession.Selected then
              professions
              |> setSelected true profession.Requires
              |> setSelected false profession.ExclusiveWith
            else
              professions |> setSelected false profession.Dependants }

module Skill =
  let initial =
    { Name = "initial"
      Level = 0
      Buff = 0
      Professions = Map.empty
      ProfessionLayout = List.empty }

  let name skill = skill.Name

  let nameOf = toNameOf name

  let buffedLevel skill = skill.Level + skill.Buff