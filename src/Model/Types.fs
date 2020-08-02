module StardewValleyStonks

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

type Comparison =
  | Better
  | Worse
  | Same
  | Incomparable

module Types =
  type NameOf<'t> = Name of string

  let ofName (Name name) = name

  let toNameOf (toString: 't -> string) = toString >> NameOf<'t>.Name

  let listToMap keyFun list =
    list
    |> List.map (fun x -> keyFun x, x)
    |> Map.ofList

  let mapToList map =
    [ for KeyValue(_, value) in map do
        value ]

  let mergeWith f a b =
    Map.fold (fun (map: Map<_,_>) k v1 ->
      match map.TryFind k with
      | Some v2 -> map.Add(k, f k v1 v2)
      | None -> map.Add(k, v1))
      a
      b

  let merge a b = mergeWith (invalidArg "'a' and/or 'b'" "The maps had one or more of the same key(s).") a b

type Status =
  | Valid
  | Warning
  | Invalid

module Status =
  let validPrecedence = function
    | Valid -> 0
    | Warning -> 1
    | Invalid -> 2

  let invalidPrecedence = function
    | Valid -> 2
    | Warning -> 1
    | Invalid -> 0

  let WVIPrecedence = function
    | Warning -> 0
    | Valid -> 1
    | Invalid -> 2

  let listOverall precedence initialStatus (statuses: Status list) =
    if statuses.IsEmpty then
      initialStatus
    else
      let rec helper status = function
        | [] -> status
        | head::tail ->
            match precedence head with
            | 0 -> head //max value found, "return" early
            | x when x < precedence status -> helper head tail
            | _ -> helper status tail
      helper statuses.Head statuses.Tail

  let listOverallWith precedence initialStatus toStatus (list: _ list): Status =
    if list.IsEmpty then
      initialStatus
    else
      let rec helper status = function
        | [] -> status
        | head::tail ->
            let headStatus = toStatus head
            match precedence headStatus with
            | 0 -> headStatus //max value found, "return" early
            | x when x < precedence status -> helper headStatus tail
            | _ -> helper status tail
      helper (toStatus list.Head) list.Tail


  let listOverallValid = listOverall validPrecedence Valid
  let listOverallInvalid = listOverall invalidPrecedence Valid
  let listOverallWVI = listOverall WVIPrecedence Invalid

  let listOverallValidWith toStatus list = listOverallWith validPrecedence Valid toStatus list
  let listOverallInvalidWith toStatus list = listOverallWith invalidPrecedence Valid toStatus list
  let listoOerallWVIWith toStatus list = listOverallWith WVIPrecedence Valid toStatus list

  let mapOverall precedence initialStatus toStatus (map: Map<_,_>): Status =
    if map.IsEmpty then
      initialStatus
    else
      let list = Map.toList map
      let rec helper status = function
        | [] -> status
        | (key, value)::tail ->
            let headStatus = toStatus key value
            match precedence headStatus with
            | 0 -> headStatus //max value found, "return" early
            | x when x < precedence status -> helper headStatus tail
            | _ -> helper status tail
      helper (toStatus (fst list.Head) (snd list.Head)) list.Tail

  let mapOverallValid toStatus map = mapOverall validPrecedence Invalid toStatus map 
  let mapOverallInvalid toStatus map = mapOverall invalidPrecedence Valid toStatus map 
  let mapOverallWVI toStatus map = mapOverall WVIPrecedence Invalid toStatus map 
