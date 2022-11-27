namespace StardewValleyStonks

type nat = uint

[<AutoOpen>]
module Prelude =
  let inline nat x = uint x

  let inline withMultiplier multiplier (value: nat) =
    multiplier * float value |> nat

  let inline (!!) x = Fable.Core.JsInterop.(!!)x

  let inline resizeToArray (r: 'a ResizeArray) =
    #if FABLE_COMPILER
    !!r : 'a array
    #else
    r.ToArray()
    #endif

  let inline internal enumName (e: 'a) = System.Enum.GetName (typeof<'a>, e)


[<AutoOpen>]
module Combinators =
  let inline flip f x y = f y x
  let inline konst x _ = x
  let inline tuple2 a b = a, b
  let inline curry f a b = f (a, b)
  let inline uncurry f (a, b) = f a b


[<RequireQualifiedAccess>]
module Option =
  let defaultOrMap defaultValue mapping = function
    | Some value -> mapping value
    | None -> defaultValue

  let reduce reduction a b =
    match a, b with
    | Some a, Some b -> Some (reduction a b)
    | Some a, None -> Some a
    | None, Some b -> Some b
    | None, None -> None

  let inline min a b = reduce min a b
  let inline max a b = reduce max a b

  let ofResult = function
    | Ok x -> Some x
    | Error _ -> None


[<RequireQualifiedAccess>]
module Result =
  let get = function
    | Ok x -> x
    | Error _ -> invalidArg "result" "The result value was Error."


[<AutoOpen>]
module CollectionExtentions =
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
    let inline sum' array = Array.sum array

    #if FABLE_COMPILER
    // This gives faster and cleaner JS compared to `Array.sum` or `array?reduce(fun x y -> x + y)`
    let sum (array: float array) =
      let mutable sum = 0.0
      for x in array do
        sum <- sum + x
      sum

    let sumBy projection (array: _ array) =
      let mutable sum = 0.0
      for x in array do
        sum <- sum + projection x
      sum
    #endif

    let mapReduce reduction mapping (array: _ array) =
      if array.Length = 0 then invalidArg (nameof array) "The given array cannot be empty."
      let mutable current = mapping array[0]
      for x in array do
        current <- reduction current (mapping x)
      current

  [<RequireQualifiedAccess>]
  module Map =
    let ofValues (key: 'value -> 'key) values = values |> Seq.map (fun v -> key v, v) |> Map.ofSeq
    let ofKeys (value: 'key -> 'value) keys = keys |> Seq.map (fun k -> k, value k) |> Map.ofSeq


open Fable.Core

type Dictionary<'a, 'b> = System.Collections.Generic.Dictionary<'a, 'b>

/// An immutable wrapper around ESM Map.
/// Keys should be primitive (string, number, etc.),
/// and values should not be Option / null.
type [<Erase>] Table<'a, 'b> = ReadOnlyDictionary of Dictionary<'a, 'b>

[<RequireQualifiedAccess>]
module Table =
  #if FABLE_COMPILER
  let inline wrap (map: JS.Map<'a, 'b>) = !!map: Table<'a, 'b>
  #else
  let inline wrap dict = ReadOnlyDictionary dict
  #endif

  #if FABLE_COMPILER
  let inline unwrap (table: Table<'a, 'b>) = !!table: Dictionary<'a, 'b>
  #else
  let inline unwrap (ReadOnlyDictionary dict) = dict
  #endif

  let inline empty () =
    #if FABLE_COMPILER
    JS.Constructors.Map.Create ()
    #else
    Dictionary ()
    #endif
    |> wrap

  let inline ofSeq seq =
    #if FABLE_COMPILER
    JS.Constructors.Map.Create seq
    #else
    Dictionary (seq |> Seq.map System.Collections.Generic.KeyValuePair.Create)
    #endif
    |> wrap

  let ofKeys value keys = keys |> Seq.map (fun k -> k, value k) |> ofSeq
  let ofValues key values = values |> Seq.map (fun v -> key v, v) |> ofSeq

  let inline toSeq (table: Table<'k, 'v>) =
    #if FABLE_COMPILER
    !!table: ('k * 'v) seq
    #else
    table |> unwrap |> Seq.map (fun (KeyValue kv) -> kv)
    #endif

  let inline toArray table = toSeq table |> Array.ofSeq
  let inline toList table = toSeq table |> List.ofSeq

  let inline keys table = (unwrap table).Keys
  let inline values table = (unwrap table).Values
  let inline count table = (unwrap table).Count

  #if FABLE_COMPILER
  open Fable.Core.JsInterop
  let find (key: 'a) (table: Table<'a, 'b>) =
    let value: 'b = table?get key
    if isNullOrUndefined value
    then failwith $"The key {key} is not present in the map."
    else value
  #else
  let inline find key table = (unwrap table)[key]
  #endif

  #if FABLE_COMPILER
  let inline tryFind (key: 'a) (table: Table<'a, 'b>) = (table?get key): 'b option
  #else
  let tryFind key table =
    match (unwrap table).TryGetValue key with
    | true, value -> Some value
    | _ -> None
  #endif

  let inline containsKey key table = (unwrap table).ContainsKey key
  let exists predicate table = toSeq table |> Seq.exists (uncurry predicate)
  let forall predicate table = toSeq table |> Seq.forall (uncurry predicate)

type Table<'a, 'b> with
  member inline this.Count = Table.count this
  member inline this.Keys = Table.keys this
  member inline this.Values = Table.values this
  member inline this.Item key = this |> Table.find key
  member inline this.Find key = this |> Table.find key
  member inline this.TryFind key = this |> Table.tryFind key
  member inline this.ContainsKey key = this |> Table.containsKey key
