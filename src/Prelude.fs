namespace StardewValleyStonks

type nat = uint

[<AutoOpen>]
module internal Combinators =
  let inline flip f x y = f y x

  let inline konst x _ = x

  let inline tuple2 a b = a, b
  let inline tuple3 a b c = a, b, c
  let inline curry f a b = f (a, b)
  let inline uncurry f (a, b) = f a b


module internal Option =
  let inline defaultOrMap defaultValue mapping = function
    | Some value -> mapping value
    | None -> defaultValue

  let merge mapping a b =
    match a, b with
    | Some a, Some b -> Some (mapping a b)
    | Some a, None -> Some a
    | None, Some b -> Some b
    | None, None -> None

  let inline min a b = merge min a b
  let inline max a b = merge max a b

  let ofResult = function
    | Ok x -> Some x
    | Error _ -> None



[<AutoOpen>]
module internal CollectionExtentions =
  [<RequireQualifiedAccess>]
  module Seq =
    let inline sortDirectionBy ascending projection seq =
      if ascending
      then Seq.sortBy projection seq
      else Seq.sortByDescending projection seq

    let inline sortDirectionWith ascending comparer seq =
      if ascending
      then Seq.sortWith comparer seq
      else Seq.sortWith (flip comparer) seq

    let tryReduce reduction (seq: _ seq) =
      use e = seq.GetEnumerator ()
      if not <| e.MoveNext () then None else
      let mutable acc = e.Current
      while e.MoveNext () do
        acc <- reduction acc e.Current
      Some acc

    let inline tryMin seq = tryReduce min seq
    let inline tryMax seq = tryReduce max seq

    let tryMinBy projection (seq: _ seq) =
      use e = seq.GetEnumerator ()
      if not <| e.MoveNext () then None else
      let mutable acc = e.Current
      let mutable minVal = projection acc
      while e.MoveNext () do
        let v = projection e.Current
        if v < minVal then
          acc <- e.Current
          minVal <- v
      Some acc

    let tryMaxBy projection (seq: _ seq) =
      use e = seq.GetEnumerator ()
      if not <| e.MoveNext () then None else
      let mutable acc = e.Current
      let mutable minVal = projection acc
      while e.MoveNext () do
        let v = projection e.Current
        if v > minVal then
          acc <- e.Current
          minVal <- v
      Some acc

  [<RequireQualifiedAccess>]
  module Array =
    let mapReduce reduction mapping (array: _ array) =
      if array.Length = 0 then invalidArg (nameof array) "The given array cannot be empty."
      let mutable current = mapping array[0]
      for i = 1 to array.Length - 1 do
        current <- mapping array[i] |> reduction current
      current

  [<RequireQualifiedAccess>]
  module Map =
    let ofValues (key: 'value -> 'key) values = values |> Seq.map (fun v -> key v, v) |> Map.ofSeq
    let ofKeys (value: 'key -> 'value) keys = keys |> Seq.map (fun k -> k, value k) |> Map.ofSeq


open Fable.Core
open Fable.Core.JsInterop

type private Dictionary<'a, 'b> = System.Collections.Generic.Dictionary<'a, 'b>

type [<Erase>] Table<'a, 'b> = private ReadOnlyDictionary of Dictionary<'a, 'b>

[<RequireQualifiedAccess>]
module Table =
  // using (!!) generates less cluttered js
  let inline private unwrap (table: Table<'a, 'b>) = !!table: Dictionary<'a, 'b>

  let inline empty () = Dictionary () |> ReadOnlyDictionary

  let inline wrap dict = ReadOnlyDictionary dict
  let ofSeq seq =
    let dict = Dictionary ()
    for k, v in seq do
      dict[k] <- v
    ReadOnlyDictionary dict
  let ofKeys value keys = keys |> Seq.map (fun k -> k, value k) |> ofSeq
  let ofValues key values = values |> Seq.map (fun v -> key v, v) |> ofSeq

  let inline toSeq (table: Table<'k, 'v>) = !!(unwrap table): ('k * 'v) seq
  let inline toArray table = toSeq table |> Array.ofSeq
  let inline toList table = toSeq table |> List.ofSeq

  let inline keys table = (unwrap table).Keys
  let inline values table = (unwrap table).Values
  let inline count table = (unwrap table).Count

  let inline find key table = (unwrap table)[key]
  let tryFind key table =
    // .TryGetValue is very cluttered js
    let dict = unwrap table
    if dict.ContainsKey key
    then Some dict[key]
    else None
  let inline containsKey key table = (unwrap table).ContainsKey key
  let exists predicate table = toSeq table |> Seq.exists (uncurry predicate)
  let forall predicate table = toSeq table |> Seq.forall (uncurry predicate)

  let fold folder state table = toSeq table |> Seq.fold (folder >> uncurry) state

  let iter action table = toSeq table |> Seq.iter (uncurry action)

type Table<'a, 'b> with
  member inline this.Count = Table.count this
  member inline this.Keys = Table.keys this
  member inline this.Values = Table.values this
  member inline this.Item key = Table.find key this
  member inline this.Find key = Table.find key this
  member inline this.TryFind key = Table.tryFind key this
  member inline this.ContainsKey key = Table.containsKey key this


type [<Erase>] 'a Block = private ReadOnlyArray of 'a array

[<RequireQualifiedAccess>]
module Block =
  let inline private unwrap (index: 'a Block) = !!index: 'a array

  let empty = ReadOnlyArray Array.empty

  let inline length index = unwrap index |> Array.length
  let inline isEmpty index = length index = 0

  let inline item i index = unwrap index |> Array.item i
  let inline tryItem i index = unwrap index |> Array.tryItem i

  let inline wrap array = ReadOnlyArray array
  let inline ofSeq seq = Array.ofSeq seq |> ReadOnlyArray
  let inline ofList list = Array.ofList list |> ReadOnlyArray
  let inline singleton item = Array.singleton item |> ReadOnlyArray
  let inline init count initializer = Array.init count initializer |> ReadOnlyArray
  let inline create count value = Array.create count value |> ReadOnlyArray
  let inline zeroCreate count = Array.zeroCreate count |> ReadOnlyArray
  let inline replicate count value = Array.replicate count value |> ReadOnlyArray

  let inline toArray index = unwrap index |> Array.copy
  let inline toSeq index = unwrap index |> Array.toSeq
  let inline toList index = unwrap index |> Array.toList

  let inline exists predicate index = unwrap index |> Array.exists predicate
  let inline forall predicate index = unwrap index |> Array.forall predicate

  let inline filter predicate index = unwrap index |> Array.filter predicate |> ReadOnlyArray

  let inline map' mapping index = unwrap index |> Array.map mapping
  let inline map mapping index = map' mapping index |> ReadOnlyArray
  let inline mapi' mapping index = unwrap index |> Array.mapi mapping
  let inline mapi mapping index = unwrap index |> Array.mapi mapping |> ReadOnlyArray
  let inline map2 mapping a b = Array.map2 mapping (unwrap a) (unwrap b) |> ReadOnlyArray


  let inline fold folder state index = unwrap index |> Array.fold folder state

  let inline mapFold' mapping state index = unwrap index |> Array.mapFold mapping state

  let inline reduce reduction index = unwrap index |> Array.reduce reduction

  let inline sum index = unwrap index |> Array.sum
  let inline sumBy projection index = unwrap index |> Array.sumBy projection

  let inline max index = unwrap index |> Array.max
  let inline maxBy projection index = unwrap index |> Array.maxBy projection
  let inline min index = unwrap index |> Array.min
  let inline minBy projection index = unwrap index |> Array.minBy projection

  let inline mapReduce reduction mapping index = unwrap index |> Array.mapReduce reduction mapping


  let inline find predicate index = unwrap index |> Array.find predicate
  let inline tryFind predicate index = unwrap index |> Array.tryFind predicate
  let inline findIndex predicate index = unwrap index |> Array.findIndex predicate
  let inline tryFindIndex predicate index = unwrap index |> Array.tryFindIndex predicate
  let inline pick chooser index = unwrap index |> Array.pick chooser
  let inline tryPick chooser index = unwrap index |> Array.tryPick chooser

  let inline append a b = Array.append (unwrap a) (unwrap b) |> ReadOnlyArray
  let inline allPairs a b = Array.allPairs (unwrap a) (unwrap b) |> ReadOnlyArray

  let inline iter action index = unwrap index |> Array.iter action
  let inline iteri action index = unwrap index |> Array.iteri action

  let inline sort index = unwrap index |> Array.sort |> ReadOnlyArray
  let inline sortBy projection index = unwrap index |> Array.sortBy projection |> ReadOnlyArray
  let inline sortDescending index = unwrap index |> Array.sortDescending |> ReadOnlyArray
  let inline sortByDescending projection index = unwrap index |> Array.sortByDescending projection |> ReadOnlyArray
  let inline sortWith comparer index = unwrap index |> Array.sortWith comparer |> ReadOnlyArray

type 'a Block with
  member inline this.Length = Block.length this
  member inline this.IsEmpty = Block.isEmpty this
  member inline this.Item i = Block.item i this




[<AutoOpen>]
module internal Util =
  let inline refEqual a b = System.Object.ReferenceEquals (a, b)

  let inline minBy projection a b = if projection a <= projection b then a else b
  let inline maxBy projection a b = if projection a >= projection b then a else b

  let inline compareBy projection a b = compare (projection a) (projection b)
  let inline compareByRev projection a b = compare (projection b) (projection a)

  let inline nat x = uint x

  let inline withMultiplier multiplier (value: nat) =
    multiplier * float value |> nat

  let inline onClosedInterval lowerbound upperbound value =
    lowerbound <= value && value <= upperbound

  let inline nonZero x = x > LanguagePrimitives.GenericZero

  let inline nonNegative x = x >= LanguagePrimitives.GenericZero

  let inline enumName (e: 'a) = System.Enum.GetName (typeof<'a>, e)

  let inline unitUnionCases<'a> =
    typeof<'a>
    |> Reflection.FSharpType.GetUnionCases
    |> Array.map (fun x -> Reflection.FSharpValue.MakeUnion (x, Array.empty) |> unbox<'a>)
    |> Block.wrap

  let sortWithLast last x y =
    match x = last, y = last with
    | true, true -> 0
    | true, false -> 1
    | false, true -> -1
    | false, false -> compare x y

  let sortWithLastBy last projection x y = sortWithLast last (projection x) (projection y)

  let sortWithLastRev last x y =
    match x = last, y = last with
    | true, true -> 0
    | true, false -> 1
    | false, true -> -1
    | false, false -> compare y x

  let sortWithLastByRev last projection x y = sortWithLastRev last (projection x) (projection y)

  let sortByMany comparers seq =
    let comparers = Array.ofSeq comparers
    seq
    |> Seq.sortWith (fun x y ->
      let mutable comparison = 0
      let mutable i = 0
      while comparison = 0 && i < comparers.Length do
        comparison <- comparers[i] x y
        i <- i + 1
      comparison)
    |> Array.ofSeq
      // comparers
      // |> Array.tryPick (fun comparer ->
      //   match comparer x y with
      //   | 0 -> None
      //   | x -> Some x)
      // |> Option.defaultValue 0)
    // sorted

  // let private levenshtein: JsFunc = import "distance" "fastest-levenshtein"

  // let fuzzyMatch tolerance (str: string) (search: string) =
  //   let str = str.ToLower()
  //   let search = search.Trim().ToLower()
  //   let distance: int = unbox <| levenshtein.Invoke(str, search)
  //   // let lengthDiff = abs (str.Length - search.Length)
  //   distance <= int (float search.Length * tolerance)
