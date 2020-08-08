module StardewValleyStonks

type Override = bool option

module Types =
  type NameOf<'t> = Name of string

  let ofName (Name name) = name

  let toNameOf (toString: 't -> string) = toString >> NameOf<'t>.Name

  let listToMap keyFun list =
    list
    |> List.map (fun x -> keyFun x, x)
    |> Map.ofList

  let mapValues (map: Map<_,'t>) =
    [ for KeyValue(_, value) in map do
        value ]

  let mergeWith f a b =
    Map.fold (fun (map: Map<_,_>) k v1 ->
      match map.TryFind k with
      | Some v2 -> map.Add(k, f k v1 v2)
      | None -> map.Add(k, v1))
      a
      b

  let merge a b =
    Map.fold (fun (map: Map<_,_>) k v1 ->
      map.Add(k, v1))
      a
      b

  let flip f a b = f b a

  let clamp low high = max low >> min high 

  let positive = max 0
  let positivef = max 0.0

  let optionToString f = function
    | Some x -> f x
    | None -> "None"
  
  let stringToOption f = function
    | "None" -> None
    | x -> Some <| f x

open Types

type Season =
  | Spring
  | Summer
  | Fall
  | Winter

module Season =
  let next = function
    | Spring -> Summer
    | Summer -> Fall
    | Fall -> Winter
    | Winter -> Spring

  let from (start: Season) finish =
    let mutable season = start
    seq {
      start
      while season <> finish do
        season <- next season
        season
    }

  let parse str =
    match str with
    | "Spring" -> Spring
    | "Summer" -> Summer
    | "Fall" -> Fall
    | "Winter" -> Winter
    | _ -> invalidArg "str" (sprintf "'%s' is not the name of a Season." str)

  let all =
    [ Spring
      Summer
      Fall
      Winter ]

type Date =
  { Season: Season
    Day: int }

module Date =
  let isBefore other date =
    date.Season < other.Season
    || date.Season = other.Season && date.Day < other.Day

  let validDay = clamp 1 28

type Status =
  | Valid
  | Warning
  | Invalid

// type MaxValue<'t> =
//   | Max of 't
//   | NotMax of 't

// type StatusBuilder =
//   member this.Bind(v, f) =
//     match v with
//     | Max v -> v
//     | NotMax v -> f v


// type StatusDataBuilder =
//   member this.Bind(v, f) =
//     match v with
//     | Max v -> f v
//     | NotMax v -> f v

module Status =
  let ofBool value = if value then Valid else Invalid

  let ofOverride defaultValue = function
    | Some x -> ofBool x
    | None -> defaultValue

  let ofOverrideBool = ofBool >> ofOverride

  let foldEarlyReturn precedenceFun initialValue highestPrecedence (statuses: Status list) =
    let rec helper status = function
      | [] -> status
      | head::tail ->
          if head = highestPrecedence then
            highestPrecedence //highestPrecedence found, "return" early
          else
            helper (precedenceFun head status) tail
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
          if headStatus = highestPrecedence then
            highestPrecedence //highestPrecedence found, "return" early
          else
            helper (precedenceFun headStatus status) tail
    helper initialValue list

  let oneValidWith toStatus list = foldEarlyReturnWith min Invalid Valid toStatus list
  let allValidWith toStatus list = foldEarlyReturnWith max Valid Invalid toStatus list

type Comparrison =
  | Better
  | Equal
  | Worse

module Comparrison =
  let ofInt = function
    | 0 -> Equal
    | x when x > 0 -> Better
    | _ -> Worse

  let overall comparrisons =
    let rec helper comparrison list =
      match list with
      | [] -> Some comparrison
      | head::tail ->
          match head with
          | Equal -> helper comparrison tail
          | x -> if x = comparrison then helper comparrison tail else None
    helper Equal comparrisons