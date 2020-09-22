namespace StardewValleyStonks

type Override = bool option

// During runtime, this will erase into simply a data field of string, not a union case.
// During compile time, this is useful for static type checking (i.e. are we correctly passing the name of a type 't and not a type 'u)
[<Fable.Core.Erase>]
type NameOf<'t> = Name of string

module Types =
  // Take the output of a function with 3 inputs as the first input of another function
  let inline (>>>|) f1 f2 a b c = f2 (f1 a b c)
  let inline (|<<<) f2 f1 a b c = f2 (f1 a b c)

  // Compose 2 inputs
  let inline (>>|) f1 f2 a b = f2 (f1 a b)
  let inline (|<<) f2 f1 a b = f2 (f1 a b)

  let inline flip f a b = f b a

  let inline swap (a, b) = b, a

  let inline conditional func condition = if condition then func else id

  let inline whenever condition func = if condition then func else id

  let inline defaultProjection defaultValue projection = function
    | Some x -> projection x
    | None -> defaultValue

  let optionToStringWith = defaultProjection
  let optionToString f = optionToStringWith "None" f
 
  let stringToOptionWith none parse text =
    if text = none
    then None
    else Some <| parse text

  let stringToOption parse = stringToOptionWith "None" parse

  let inline ofName (Name name) = name
  let inline nameValue<'t> (name: NameOf<'t>) = ofName name

  let inline ofNameOptionWith<'t> none = optionToStringWith none nameValue<'t>
  let inline ofNameOption<'t> = optionToString nameValue<'t>

  let inline toNameOf (toString: 't -> _ ) = toString >> NameOf<'t>.Name


  let lower (text: string) = text.ToLower()

  let inline toMapByKey keyProjection = Seq.map (fun x -> keyProjection x, x) >> Map.ofSeq

  let inline toMapByValue valueProjection = Seq.map (fun x -> x, valueProjection x) >> Map.ofSeq

  let inline mapValues (map: Map<_,_>) =
    seq {
      for KeyValue(_, value) in map do
        yield value }


  let inline clamp lowBound upBound = max lowBound >> min upBound

  let positive = max 0
  let positivef = max 0.0


  let inline sortMode ascending = if ascending then Seq.sortBy else Seq.sortByDescending

  let listWithNone list = None::(List.map Some list)

  let listToStringWith toString = function
    | [] -> ""
    | head::tail ->
        List.fold (fun text value ->
          text + ", " + toString value)
          (toString head)
          tail

  let inline listToString list = listToStringWith string list

  let inline apply multiplier (value: int) = multiplier * float value |> int

  let inline applyWhen condition multiplier = if condition then apply multiplier else id

  let inline tryAddToSet key value (map: Map<_,Set<_>>) =
    match map.TryFind key with
    | Some x -> map.Add(key, x.Add value)
    | None -> map.Add(key, Set.singleton value)


  let inline allExtremes empty singleton add moreExtreme projection items =
    if Seq.isEmpty items then
      empty
    else
      let head = Seq.head items
      let mutable extremes = singleton head
      let mutable currentExtreme = projection head
      for x in Seq.tail items do
        let thisExtreme = projection x
        if thisExtreme = currentExtreme then
          extremes <- extremes |> add x
        elif moreExtreme thisExtreme currentExtreme then
          extremes <- singleton x
          currentExtreme <- thisExtreme
      extremes

  let inline allExtremesList extreme = allExtremes List.empty List.singleton (fun head tail -> head::tail) extreme
  let inline allExtremesSet extreme = allExtremes Set.empty Set.singleton Set.add extreme

  let inline allMinsList projection list = allExtremesList (<) projection list
  let inline allMaxsList projection list = allExtremesList (>) projection list
  let inline allMinsSet projection list = allExtremesSet (<) projection list
  let inline allMaxsSet projection list = allExtremesSet (>) projection list

open Types

type Season =
  | Spring = 1
  | Summer = 2
  | Fall = 3
  | Winter = 4

module Seasons =
  let spring = Season.Spring
  let summer = Season.Summer
  let fall = Season.Fall
  let winter = Season.Winter

module Season =
  // Normal 'string' function returns the stringified int value and not the name.
  let name season = System.Enum.GetName(typeof<Season>, season)

  let add value (season: Season) =
    let added = (int season + value) % 4
    if added > 0
    then enum<Season> added
    else enum<Season> (4 + added)

  let next = add 1

  let distance (start: Season) finish =
    if start < finish
    then int finish - int start
    else 4 - int start + int finish

  // Easier to check for false case.
  let isNotBetween start finish (season: Season) =
    if start < finish then season < start || season > finish
    elif start > finish then season < start && season > finish
    else start <> season

  let isBetween = not |<<< isNotBetween

  let parse text = System.Enum.Parse(typeof<Season>, text) :?> Season

  open Seasons
  let all =
    [ spring
      summer
      fall
      winter ]

type Date =
  { Season: Season
    Day: int }
  static member (+) (date, days) =
    { Season = date.Season |> Season.add ((date.Day + days - 1) / 28)
      Day = (date.Day + days - 1) % 28 + 1 }


module Date =
  let validDay = clamp 1 28

  let daysIn season startDate endDate =
    if startDate = endDate then 28 // full year, infinite mode
    elif season |> Season.isNotBetween startDate.Season endDate.Season then 0
    elif startDate.Season = endDate.Season then endDate.Day - startDate.Day + 1
    elif season = startDate.Season then 29 - startDate.Day
    elif season = endDate.Season then endDate.Day
    else 28

  let isNotBetween startDate endDate (date: Date) =
    if startDate < endDate then date < startDate || date > endDate
    elif startDate > endDate then date < startDate && date > endDate
    else false

  let isBetween = not |<<< isNotBetween