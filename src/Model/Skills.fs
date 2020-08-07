namespace StardewValleyStonks

open Types

type Profession =
  { Name: string
    Selected: bool
    UnlockLevel: int
    Requires: NameOf<Profession> option
    ExclusiveWith: NameOf<Profession> option
    Dependants: Set<NameOf<Profession>> }

type Skill =
  { Name: string
    Level: int
    Buff: int
    Professions: Map<NameOf<Profession>, Profession>
    ProfessionLayout: NameOf<Profession> list list }

module Profession =
  let name (profession: Profession) = profession.Name

  let nameOf = toNameOf name

  let isUnlocked skill profession = skill.Professions.[profession].UnlockLevel <= skill.Level

  let forceToggle profession = { profession with Selected = not profession.Selected }

  let trySetSelected profession value skill (professions: Map<NameOf<Profession>,_>) =
    match profession with
    | Some name -> professions.Add(name, { skill.Professions.[name] with Selected = value } )
    | None -> professions

  // Professions should only have direct relationships.
  // E.g. profession A can depend on profession B, but profession B cannot then also depend on profession C.
  // So this function is not recursive.
  let toggle skill ignoreRelationships profession =
    let professions = skill.Professions.Add(profession, forceToggle skill.Professions.[profession])
    if ignoreRelationships then
      { skill with Professions = professions }
    else
      let profession = professions.[profession]
      { skill with
          Professions =
            if profession.Selected then
              professions
              |> trySetSelected profession.Requires true skill
              |> trySetSelected profession.ExclusiveWith false skill
            else
              Set.fold (fun professions profession ->
                  professions.Add(profession, { professions.[profession] with Selected = false } ))
                professions
                profession.Dependants }

  let initial =
    { Name = "initial"
      Selected = false
      UnlockLevel = 10
      Requires = None
      ExclusiveWith = None
      Dependants = Set.empty }

module Skill =
  let name skill = skill.Name

  let nameOf = toNameOf name

  let buffedLevel skill = skill.Level + skill.Buff

  let initial =
    { Name = "initial"
      Level = 0
      Buff = 0
      Professions = Map.empty
      ProfessionLayout = List.empty }

  let all =
    [ { initial with
          Name = "Farming"
          Professions =
            [ { Profession.initial with
                  Name = "Tiller"
                  UnlockLevel = 5
                  Dependants = set [ Name "Artisan"; Name "Agriculturist" ] }
              { Profession.initial with
                  Name = "Artisan"
                  Requires = Some <| Name "Tiller"
                  ExclusiveWith = Some <| Name "Agriculturist" }
              { Profession.initial with
                  Name = "Agriculturist"
                  Requires = Some <| Name "Tiller"
                  ExclusiveWith = Some <| Name "Artisan" } ]
            |> listToMap Profession.nameOf
          ProfessionLayout =
            [ [ Name "Tiller" ]
              [ Name "Artisan"; Name "Agriculturist" ] ] }
      { initial with
          Name = "Foraging"
          Professions =
            [ { Profession.initial with
                  Name = "Gatherer"
                  UnlockLevel = 5
                  Dependants = set [ Name "Botanist" ] }
              { Profession.initial with
                  Name = "Botanist"
                  Requires = Some <| Name "Gatherer" } ]
            |> listToMap Profession.nameOf
          ProfessionLayout =
            [ [ Name "Gatherer" ]
              [ Name "Botanist" ] ] } ]