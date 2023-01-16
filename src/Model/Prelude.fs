namespace StardewValleyStonks

type nat = uint

module [<AutoOpen>] Prelude =
  let inline nat value = uint value

  let inline withMultiplier multiplier (value: nat) =
    multiplier * float value |> nat

  let inline internal enumName (e: 'a) = System.Enum.GetName (typeof<'a>, e)


module [<RequireQualifiedAccess>] Option =
  let defaultOrMap defaultValue mapping = function
    | Some value -> mapping value
    | None -> defaultValue


open Fable.Core

type Dictionary<'a, 'b> = System.Collections.Generic.Dictionary<'a, 'b>

type [<Erase>] Table<'a, 'b> = ReadOnlyDictionary of Dictionary<'a, 'b>

module [<RequireQualifiedAccess>] Table =
  #if FABLE_COMPILER
  let inline unwrap (table: Table<'a, 'b>) = unbox<Dictionary<'a, 'b>> table
  #else
  let inline unwrap (ReadOnlyDictionary dict) = dict
  #endif

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

  let inline toSeq (table: Table<'k, 'v>) =
    #if FABLE_COMPILER
    unbox<('k * 'v) seq> table
    #else
    table |> unwrap |> Seq.map (fun (KeyValue kv) -> kv)
    #endif

  let inline keys table = (unwrap table).Keys
  let inline values table = (unwrap table).Values
  let inline count table = (unwrap table).Count

  let inline containsKey key table = (unwrap table).ContainsKey key
  let inline find key table = (unwrap table)[key]
  let tryFind key table =
    if table |> containsKey key
    then Some (table |> find key)
    else None

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
