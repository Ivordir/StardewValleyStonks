module StardewValleyStonks

type Season =
  | Spring
  | Summer
  | Fall
  | Winter

module Season =
  let all =
    [ Spring
      Summer
      Fall
      Winter ]

  let next = function
    | Spring -> Summer
    | Summer -> Fall
    | Fall -> Winter
    | Winter -> Spring

  let from start finish =
    let rec helper list current finish =
      if current = finish then
        current::list
      else
        helper (current::list) (next current) finish
    helper [] start finish

  let parse str =
    match str with
    | "Spring" -> Spring
    | "Summer" -> Summer
    | "Fall" -> Fall
    | "Winter" -> Winter
    | _ -> invalidArg "str" (sprintf "'%s' is not the name of a Season." str)

type RequirementsShould =
  | Warn
  | Invalidate
  | Ignore

module RequirementsShould =
  let all =
    [ Warn
      Invalidate
      Ignore ]

  let parse str =
    match str with
    | "Warn" -> Warn
    | "Invalidate" -> Invalidate
    | "Ignore" -> Ignore
    | _ -> invalidArg "str" (sprintf "'%s' does not correspond to a RequirementsShould." str)

module Types =
  type Date =
    { Season: Season
      Day: int }

  let isBefore other date =
    date.Season < other.Season
    || date.Season = other.Season && date.Day < other.Day

  type NameOf<'t> = Name of string

  let ofName (Name name) = name

  let toNameOf (string: 't -> string) = (string >> NameOf<'t>.Name)

  type Comparison =
    | Better
    | Worse
    | Same
    | Incomparable

  let listToMap keyFun list =
    list
    |> List.map (fun x -> keyFun x, x)
    |> Map.ofList

type Status =
  | Valid
  | Warning
  | Invalid

module Status =
  let validPrecedence = function
    | Valid -> 2
    | Warning -> 1
    | Invalid -> 0
  
  let invalidPrecedence = function
    | Valid -> 0
    | Warning -> 1
    | Invalid -> 2

  let WVIPrecedence = function
    | Warning -> 2
    | Valid -> 1
    | Invalid -> 0

  let compare (precedence: Status -> int) a b = if precedence a > precedence b then a else b
  
  let compareValid = compare validPrecedence
  let compareInvalid = compare invalidPrecedence
  let compareWVI = compare WVIPrecedence