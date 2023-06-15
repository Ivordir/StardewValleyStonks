namespace StardewValleyStonks

type nat = uint

[<AutoOpen>]
module Prelude =
  let inline nat value = uint value
  let inline withMultiplier multiplier (value: nat) = nat (multiplier * float value)


[<RequireQualifiedAccess>]
module Enum =
  let rec bitSetOrder a b =
    if int a = 0 || int b = 0 then
      compare a b
    else
      let c = compare (b &&& 1) (a &&& 1)
      if c = 0
      then bitSetOrder (a >>> 1) (b >>> 1)
      else c

  let inline name (e: 'a) = System.Enum.GetName (typeof<'a>, e)
  let inline values<'a> = System.Enum.GetValues typeof<'a> |> unbox<'a array>


[<RequireQualifiedAccess>]
module Option =
  let defaultOrMap defaultValue mapping = function
    | Some value -> mapping value
    | None -> defaultValue


type Dictionary<'a, 'b> = System.Collections.Generic.Dictionary<'a, 'b>

type Table<'a, 'b> = private ReadOnlyDictionary of Dictionary<'a, 'b>

[<RequireQualifiedAccess>]
module Table =
  let inline empty () = Dictionary () |> ReadOnlyDictionary

  #if FABLE_COMPILER
  let addAll (dict: Dictionary<_,_>) seq =
    for key, value in seq do
      dict[key] <- value
    dict
  #endif

  let inline ofSeq seq =
    #if FABLE_COMPILER
    addAll (Dictionary ()) seq
    #else
    Dictionary (seq |> Seq.map System.Collections.Generic.KeyValuePair.Create)
    #endif
    |> ReadOnlyDictionary

  let inline ofKeys value keys = keys |> Seq.map (fun k -> k, value k) |> ofSeq
  let inline ofValues key values = values |> Seq.map (fun v -> key v, v) |> ofSeq

  let inline toSeq (ReadOnlyDictionary table) =
    #if FABLE_COMPILER
    unbox<('k * 'v) seq> table
    #else
    table |> Seq.map (fun (KeyValue kv) -> kv)
    #endif

  let inline keys (ReadOnlyDictionary table) = table.Keys
  let inline values (ReadOnlyDictionary table) = table.Values
  let inline count (ReadOnlyDictionary table) = table.Count

  let inline containsKey key (ReadOnlyDictionary table) = table.ContainsKey key
  let inline find key (ReadOnlyDictionary table) = table[key]
  let tryFind key table =
    if table |> containsKey key
    then Some (table |> find key)
    else None

type Table<'a, 'b> with
  member inline this.Count = Table.count this
  member inline this.Keys = Table.keys this
  member inline this.Values = Table.values this
  member inline this.Item key = this |> Table.find key
  member inline this.Find key = this |> Table.find key
  member inline this.TryFind key = this |> Table.tryFind key
  member inline this.ContainsKey key = this |> Table.containsKey key
