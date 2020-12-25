namespace StardewValleyStonks

type Override = bool option

module Override =
  let all =
    [ None
      Some true
      Some false ]



module Util =
  let inline flip f x y = f y x

  // Take the output of a function with 3 inputs as the first input of another function
  let inline (>>>|) f1 f2 x y z = f2 (f1 x y z)
  let inline (|<<<) f2 f1 x y z = f2 (f1 x y z)

  // Compose 2 inputs
  let inline (>>|) f1 f2 x y = f2 (f1 x y)
  let inline (|<<) f2 f1 x y = f2 (f1 x y)

  type Dict<'a, 'b> = System.Collections.Generic.Dictionary<'a, 'b>

  let prememoize inputs func =
    let cache = Dict<_,_>()
    for input in inputs do
      cache.Add(input, func input)
    (fun key -> cache.[key])

  let memoize func =
    let cache = Dict<_,_>()
    (fun x ->
      match cache.TryGetValue(x) with
      | (true, y) -> y
      | _ ->
          let y = func x
          cache.Add(x, y)
          y)

  let memoize2 func =
    let cache = Dict<_,_>()
    (fun x y ->
      match cache.TryGetValue((x,y)) with
      | (true, z) -> z
      | _ ->
          let z = func x y
          cache.Add((x, y), z)
          z)

  let memoize3 func =
    let cache = Dict<_,_>()
    (fun x y z ->
      match cache.TryGetValue((x,y,z)) with
      | (true, w) -> w
      | _ ->
          let w = func x y z
          cache.Add((x, y, z), w)
          w)

  let trackUsed newValue keys =
    let values = Dict<_,_>()
    let value key =
      match values.TryGetValue(key) with
      | (true, v) -> v
      | _ ->
          let v = newValue()
          values.Add(key, v)
          v
    let usedList () =
      keys |> List.choose (fun k ->
        match values.TryGetValue(k) with
        | (true, v) -> Some (k, v)
        | _ -> None)
    value, usedList

  let inline swap (x, y) = x, y
  let inline tuple2 x y = (x, y)
  let inline tuple3 x y z = (x, y, z)
  let inline fstOf3 (x, _,_) = x
  let inline sndOf3 (_, x, _) = x
  let inline lastOf3 (_,_, x) = x

  // flipped defaultArg
  let inline defaultValue value = function
    | Some x -> x
    | None -> value

  let inline defaultMap defaultValue mapping = function
    | Some x -> mapping x
    | None -> defaultValue

  let optionToStringWith : string -> _ = defaultMap
  let optionToString f = optionToStringWith "None" f
 
  let stringToOptionWith none parse text =
    if text = none
    then None
    else Some <| parse text
  let stringToOption parse = stringToOptionWith "None" parse

  let inline optionDo func = function
    | Some value -> func value
    | None -> ignore

  let inline lower (text: string) = text.ToLower()

  let inline clamp lowBound upBound = max lowBound >> min upBound

  let inline positive (x: ^T) = max (LanguagePrimitives.GenericZero< ^T >) x

  let inline listSortMode ascending = if ascending then List.sortBy else List.sortByDescending

  let listWithNone list = None::(List.map Some list)

  let listToStringWith toString = function
    | [] -> ""
    | head::tail ->
        List.fold (fun text value ->
          text + ", " + toString value)
          (toString head)
          tail
  let listToString list = listToStringWith string list

  let mapGet (map: Map<_,_>) k = map.[k]
  let mapOfKeys keyMapping = List.map (fun k -> k, keyMapping k) >> Map.ofList
  let mapOfValues valueMapping = List.map (fun v -> valueMapping v, v) >> Map.ofList
  let mapUsingValues valueMapping = Map.map (fun _ v -> valueMapping v)

  // let cyclicIsBetween start finish x =
  //   if start < finish
  //   then start <= x && x <= finish
  //   else x <= finish || start <= x
    
  // let cyclicOverlap min max start finish =
  //   if start < finish
  //   then min <= finish && start <= max
  //   else min <= finish || start <= max

  let apply multiplier (value: int) = multiplier * float value |> int



open FSharp.Data.Adaptive
open Util

module Adaptive =
  let (!@) = AVal.constant
  let (!~) = AVal.force
  let (!%) = AVal.map
  let (!>) (cv: _ cval) = cv :> aval<_>
  let (<~) (c: _ cval) v = c.Value <- v
  let (!&) prop = !%(fun x -> prop (x :> obj))

  let inline (.+) x y = AVal.map2 (+) x y
  let inline (.-) x y = AVal.map2 (-) x y
  let inline (.*) x y = AVal.map2 (*) x y
  let inline (./) x y = AVal.map2 (/) x y
  let inline private pow x y = x ** y
  let inline (.**) x y = AVal.map2 pow x y

  let inline (.=) x y = AVal.map2 (=) x y
  let inline (.<) x y = AVal.map2 (<) x y
  let inline (.>) x y = AVal.map2 (>) x y
  let inline (.<=) x y = AVal.map2 (<=) x y
  let inline (.>=) x y = AVal.map2 (>=) x y
  let inline (.<>) x y = AVal.map2 (<>) x y

  let inline aMax x y = AVal.map2 max x y
  let inline aMin x y = AVal.map2 min x y
  let inline aPositive x = !%positive x

  let (.&&) x y = AVal.map2 (&&) x y
  let (.||) x y = AVal.map2 (||) x y

  let aNot: bool aval -> _ = !%not

  let inline aInt x = !%int x
  let inline aFloat x = !%float x
  let inline aString x = !%string x

  let aCond a b = !%(function
    | true -> a
    | false -> b)

  let setValueDelay c v () = (fun () -> c <~ v) |> transact
  let setValue c v = (fun () -> c <~ v) |> transact
  let toggleDelay b () = (fun () -> printf "%b" <| not !~ b; b <~ not !~b) |> transact

  let setSortDelay (currentSort: _ cval) ascending sort () =
    transact (fun () ->
      if sort = currentSort.Value then
        ascending <~ not ascending.Value
      else
        currentSort <~ sort
        ascending <~ true)

  let aDefaultValue value = AVal.map (defaultValue value)
  let aDefaultMap value projection = AVal.map (defaultMap value projection)
  let aSelectedOverride: Override aval -> bool aval -> _ = AVal.map2 defaultArg
  let aApply: float aval -> int aval -> _ = AVal.map2 apply
  let aCondApply value multiplier active =
    adaptive {
      let! a = active
      if a
      then return! aApply multiplier value
      else return! value }


open Adaptive

type Season =
  | Spring = 1
  | Summer = 2
  | Fall = 3
  | Winter = 4

type DateInput =
  { Season: Season cval
    Day: int cval }

module DateInput =
  let validDay = clamp 1 28

  let create season day =
    { Season = cval season
      Day = cval <| validDay day }

  let startDate = create Season.Spring 1
  let endDate = create Season.Fall 28

module Season =
  let name (season: Season) = System.Enum.GetName(typeof<Season>, season)

  let add value season =
    let added = (int season + value) % 4
    if added > 0
    then enum<Season> added
    else enum<Season> (4 + added)

  let next = add 1


type Date =
  { Season: Season
    Day: int }
  static member (+) (date, days) =
    let d = date.Day + days - 1
    { Season = date.Season |> Season.add (d / 28)
      Day = d % 28 + 1 }

module Date =
  //let isBetween: Date -> _ = cyclicIsBetween

  let create season day =
    { Season = season
      Day = DateInput.validDay day }

  let ofDateInput (date: DateInput) = AVal.map2 create date.Season date.Day

  let startDate = ofDateInput DateInput.startDate
  let endDate = ofDateInput DateInput.endDate

  let startDateBeforeEndDate = startDate .< endDate

  let daysBetween start finish =
    let days = 28 * int (finish.Season - start.Season) + finish.Day - start.Day
    if start > finish
    then 112 + days //28 * 4 + days
    else days

  let totalDays = AVal.map2 daysBetween startDate endDate


module Seasons =
  let all =
    [ Season.Spring
      Season.Summer
      Season.Fall
      Season.Winter ]

  let private seasonIsInDateSpan season =
    adaptive {
      let a = DateInput.startDate.Season .<= !@season
      let b = !@season .<= DateInput.endDate.Season
      let! before = Date.startDateBeforeEndDate
      if before
      then return! a .&& b
      else return! a .|| b }

  let inDateSpan = all |> ASet.ofList |> ASet.filterA seasonIsInDateSpan

  let intersectsWithDateSpan = memoize <| (fun seasons ->
    set seasons,
    ASet.ofList seasons |> ASet.intersect inDateSpan |> ASet.isEmpty |> aNot)
