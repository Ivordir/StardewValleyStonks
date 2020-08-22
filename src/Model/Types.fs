module StardewValleyStonks

type Override = bool option

module Types =
  open Fable.Core
  // During runtime, this will erase into simply a data field of string, not a union case.
  // During compile time, this is useful for static type checking (i.e. are we correctly passing the name of a type 't and not a type 'u)
  [<Erase>]
  type NameOf<'t> = Name of string

  let inline ofName (Name name) = name

  let inline toNameOf (toString: 't -> string) = toString >> NameOf<'t>.Name

  let inline listToMapByKey keyProjection list =
    list
    |> List.map (fun x -> keyProjection x, x)
    |> Map.ofList

  let inline listToMapByValue valueProjection list =
    list
    |> List.map (fun x -> x, valueProjection x)
    |> Map.ofList

  let inline mapValues (map: Map<_,_>) =
    [ for KeyValue(_, value) in map do
        yield value ]

  let inline mergeWith f =
    Map.fold (fun (map: Map<_,_>) k v1 ->
      match map.TryFind k with
      | Some v2 -> map.Add(k, f k v1 v2)
      | None -> map.Add(k, v1))

  let inline merge a b = Map.fold (fun (map: Map<_,_>) k v -> map.Add(k, v)) a b

  let inline flip f a b = f b a

  let inline clamp low high = max low >> min high

  let positive = max 0
  let positivef = max 0.0

  let inline sortMode ascending = if ascending then List.sortBy else List.sortByDescending

  let listWithNone list = None::(List.map Some list)

  let inline optionMapDefault f value = function
    | Some x -> f x
    | None -> value

  let optionToString f = optionMapDefault f "None"
  
  let stringToOption f = function
    | "None" -> None
    | x -> Some <| f x

  let inline applyTo (value: int) multiplier = multiplier * float value |> int

  let inline tryAddToSet key value (map: Map<_,Set<_>>) =
    match map.TryFind key with
    | Some x -> map.Add(key, x.Add value)
    | None -> map.Add(key, Set.singleton value)

  let inline allExtremes empty singleton add extreme projection = function
    | [] -> empty
    | head::tail ->
        let mutable extremes = singleton head
        let mutable currentExtreme = projection head
        for x in tail do
          let thisExtreme = projection x
          if thisExtreme = currentExtreme then
            extremes <- extremes |> add x
          elif extreme thisExtreme currentExtreme then
            extremes <- singleton x
            currentExtreme <- thisExtreme
        extremes

  let inline allList extreme = allExtremes List.empty List.singleton (fun h tail -> h::tail) extreme
  let inline allSet extreme = allExtremes Set.empty Set.singleton Set.add extreme

  let inline allMinsList projection list = allList (<) projection list
  let inline allMaxsList projection list = allList (>) projection list
  let inline allMinsSet projection list = allSet (<) projection list
  let inline allMaxsSet projection list = allSet (>) projection list

open Types

// Not a [<StringEnum>] since the underlying case number is needed for date comparisons.
// I'd also rather not use regular enums even though they have a more explicit integral value, because they give a warning when pattern matching unless you include a wildcard, they require qualified access, etc.
type Season =
  | Spring
  | Summer
  | Fall
  | Winter
  static member (-) ((a:Season), b) = compare a b

module Season =
  let next = function
    | Spring -> Summer
    | Summer -> Fall
    | Fall -> Winter
    | Winter -> Spring

  // Needs a parse function since it is used in a <select/> (the event change value is of type string) and it is not a [<StringEnum>]
  // This is one case where using an actual Enum might be better (i.e. Enum.Parse<Season>)
  let parse str =
    match str with
    | "Spring" -> Spring
    | "Summer" -> Summer
    | "Fall" -> Fall
    | "Winter" -> Winter
    | _ -> invalidArg "str" (sprintf "'%s' is not a Season." str)

  let all =
    [ Spring
      Summer
      Fall
      Winter ]

type Date =
  { Season: Season
    Day: int }
  static member (+) (date, days) =
    if days > 28 - date.Day then
      { Season = Season.next date.Season
        Day = days - 28 + date.Day }
    else
      { Season = date.Season
        Day = date.Day + days }

module Date =
  let validDay = clamp 1 28

  let daysIn season startDate endDate =
    if season < startDate.Season || season > endDate.Season then
      0
    else
      if startDate.Season = endDate.Season then endDate.Day - startDate.Day + 1
      elif season = startDate.Season then 29 - startDate.Day
      elif season = endDate.Season then endDate.Day
      else 28

type Status =
  | Valid
  | Warning
  | Invalid

module Status =
  let ofBool value = if value then Valid else Invalid

  let ofOverride = optionMapDefault ofBool

  let ofBoolOverride = ofBool >> ofOverride

  let foldEarlyReturn precedenceFun initialValue highestPrecedence (statuses: Status list) =
    let rec helper status = function
      | [] -> status
      | head::tail ->
          if head = highestPrecedence
          then highestPrecedence // highestPrecedence found, "return" early
          else helper (precedenceFun head status) tail
    helper initialValue statuses

  let oneValid = foldEarlyReturn min Invalid Valid
  let allValid = foldEarlyReturn max Valid Invalid

  let private warningMax a b =
    match a, b with
    | (Warning, _) | (_, Warning) -> Warning
    | (Valid, _) | (_, Valid) -> Valid
    | _ -> Invalid

  let warningPrecedence = foldEarlyReturn warningMax Invalid Warning

  let foldEarlyReturnWith precedenceFun initialValue highestPrecedence toStatus (list: _ list): Status =
    let rec helper status = function
      | [] -> status
      | head::tail ->
          let headStatus = toStatus head
          if headStatus = highestPrecedence
          then highestPrecedence // highestPrecedence found, "return" early
          else helper (precedenceFun headStatus status) tail
    helper initialValue list

  let oneValidWith toStatus list = foldEarlyReturnWith min Invalid Valid toStatus list
  let allValidWith toStatus list = foldEarlyReturnWith max Valid Invalid toStatus list

type Comparison =
  | Better
  | Equal
  | Worse

module Comparison =
  let ofInt = function
    | 0 -> Equal
    | x when x > 0 -> Better
    | _ -> Worse

  let overall = function
    | [] -> None
    | first::rest ->
        let rec helper comparison list =
          match list with
          | [] -> Some comparison
          | head::tail ->
              match head with
              | Equal -> helper comparison tail
              | x -> if x = comparison then helper comparison tail else None
        helper first rest