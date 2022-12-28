namespace StardewValleyStonks

type nat = uint

module [<AutoOpen>] Prelude =
  let inline nat x = uint x

  let inline withMultiplier multiplier (value: nat) =
    multiplier * float value |> nat

  let inline internal enumName (e: 'a) = System.Enum.GetName (typeof<'a>, e)


module [<RequireQualifiedAccess>] Option =
  let defaultOrMap defaultValue mapping = function
    | Some value -> mapping value
    | None -> defaultValue


module [<RequireQualifiedAccess>] Array =
  let inline internal sum' array = Array.sum array

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


module [<RequireQualifiedAccess>] Map =
  let ofValues (key: 'value -> 'key) values = values |> Seq.map (fun v -> key v, v) |> Map.ofSeq
  let ofKeys (value: 'key -> 'value) keys = keys |> Seq.map (fun k -> k, value k) |> Map.ofSeq


open Fable.Core

type Dictionary<'a, 'b> = System.Collections.Generic.Dictionary<'a, 'b>

/// An immutable wrapper around ESM Map.
/// Keys should be primitive (string, number, etc.),
/// and values should not be Option / null.
type [<Erase>] Table<'a, 'b> = ReadOnlyDictionary of Dictionary<'a, 'b>

module [<RequireQualifiedAccess>] Table =
  #if FABLE_COMPILER
  let inline wrap (map: JS.Map<'a, 'b>) = JsInterop.(!!)map: Table<'a, 'b>
  #else
  let inline wrap dict = ReadOnlyDictionary dict
  #endif

  #if FABLE_COMPILER
  let inline unwrap (table: Table<'a, 'b>) = JsInterop.(!!)table: Dictionary<'a, 'b>
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
    JsInterop.(!!)table: ('k * 'v) seq
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
  let exists predicate table = toSeq table |> Seq.exists (fun (k, v) -> predicate k v)
  let forall predicate table = toSeq table |> Seq.forall (fun (k, v) -> predicate k v)

type Table<'a, 'b> with
  member inline this.Count = Table.count this
  member inline this.Keys = Table.keys this
  member inline this.Values = Table.values this
  member inline this.Item key = this |> Table.find key
  member inline this.Find key = this |> Table.find key
  member inline this.TryFind key = this |> Table.tryFind key
  member inline this.ContainsKey key = this |> Table.containsKey key
