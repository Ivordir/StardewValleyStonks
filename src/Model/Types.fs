module StardewValleyStonks.Types

type Name<'t> =
  | Name of string

let ofName (Name name) = name

type Season =
  | Spring
  | Summer
  | Fall
  | Winter

let allSeasons =
  [ Spring
    Summer
    Fall
    Winter ]

let nextSeason = function
  | Spring -> Summer
  | Summer -> Fall
  | Fall -> Winter
  | Winter -> Spring

let seasonsFrom start finish =
  let rec helper list current finish =
    if current = finish then
      current::list
    else
      helper (current::list) (nextSeason current) finish
  helper [] start finish

let parseSeason str =
  match str with
  | "Spring" -> Spring
  | "Summer" -> Summer
  | "Fall" -> Fall
  | "Winter" -> Winter
  | _ -> invalidArg "str" (sprintf "'%s' is not the name of a Season." str)

type Date =
  { Season: Season
    Day: int }

let isBefore other date =
  date.Season < other.Season
  || date.Season = other.Season && date.Day < other.Day

type RequirementsShould =
  | Warn
  | Invalidate
  | Ignore
  static member List =
    [ Warn
      Invalidate
      Ignore ]

let parseRequirementsShould str =
  match str with
  | "Warn" -> Warn
  | "Invalidate" -> Invalidate
  | "Ignore" -> Ignore
  | _ -> invalidArg "str" (sprintf "'%s' does not correspond to a RequirementsShould." str)

type Status =
  | Valid
  | Warning
  | Invalid

type Comparison =
  | Better
  | Worse
  | Same
  | Incomparable

let listToMap keyFun list =
  list
  |> List.map (fun x -> keyFun x, x)
  |> Map.ofList