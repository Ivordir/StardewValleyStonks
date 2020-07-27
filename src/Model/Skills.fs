namespace StardewValleyStonks

open Types

type Profession =
  { Name: string
    Selected: bool
    UnlockLevel: int
    Requires: Set<Name<Profession>>
    ExclusiveWith: Set<Name<Profession>>
    Dependants: Set<Name<Profession>> }
  member this.Toggle = { this with Selected = not this.Selected }

module Profession =
  let initial =
    { Name = "initial"
      Selected = false
      UnlockLevel = 10
      Requires = Set.empty
      ExclusiveWith = Set.empty
      Dependants = Set.empty }
  
  let nameKey (profession: Profession) : Name<Profession> = Name profession.Name

type Skill =
  { Name: string
    Level: int
    Buff: int
    Professions: Map<Name<Profession>, Profession>
    ProfessionLayout: Name<Profession> list list }

module Skill =
  let initial =
    { Name = "initial"
      Level = 0
      Buff = 0
      Professions = Map.empty
      ProfessionLayout = List.empty }

  let buffedLevel skill = skill.Level + skill.Buff

  let professionIsUnlocked profession skill =
    skill.Professions.[profession].UnlockLevel <= skill.Level

  let private setSelected value set (professions: Map<Name<Profession>, Profession>) =
    professions
    |> Map.map
      (fun name profession ->
        if set |> Set.contains name then
          { profession with Selected = value }
        else
          profession)

  let toggleProfession profession ignoreConflicts skill =
    let professions = skill.Professions.Add(profession, skill.Professions.[profession].Toggle)
    if ignoreConflicts then
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