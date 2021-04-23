namespace StardewValleyStonks

// Generally useful functions.
[<AutoOpen>]
module internal Util =
  // Take the output of a function with 3 inputs as the first input of another function
  let inline (>>>|) f1 f2 a b c = f2 (f1 a b c)
  let inline (|<<<) f2 f1 a b c = f2 (f1 a b c)

  // Compose 2 inputs
  let inline (>>|) f1 f2 a b = f2 (f1 a b)
  let inline (|<<) f2 f1 a b = f2 (f1 a b)

  let inline flip f a b = f b a
  let inline tuple2 a b = a, b
  let inline tuple3 a b c = a, b, c

  let inline lower (text: string) = text.ToLower()

  let inline clamp lowBound upBound = max lowBound >> min upBound
  let inline positive (x: ^a) = max LanguagePrimitives.GenericZero< ^a> x

  let inline refEqual a b = System.Object.ReferenceEquals(a, b)
  let inline (==) a b = refEqual a b
  let inline (!=) a b = not <| refEqual a b

  // Option.defaultValue
  let inline defaultValue v = function
    | Some x -> x
    | None -> v

  // Option.map >> defaultValue
  let inline defaultMap defaultVal mapping = function
    | Some x -> mapping x
    | None -> defaultVal

  module Option =
    let iter2 action a b =
      match a, b with
      | Some x, Some y -> action x y
      | _ -> ()

    // Default (ascending) sort on options gives this order (None is lowest): None, None, Some -1, Some 0, Some 1, Some 1000
    // The following compare functions give a sort order where None is highest: Some -1, Some 0, Some 1, Some 1000, None, None
    let noneHighestCompare a b =
      match a, b with
      | Some _, None -> -1
      | None, Some _ -> 1
      | x, y -> compare x y

    let noneHighestCompareDescending a b =
      match a, b with
      | Some x, Some y -> compare y x
      | x, y -> compare x y

    let noneHighestCompareMode ascending =
      if ascending
      then noneHighestCompare
      else noneHighestCompareDescending

    let noneHighestCompareBy projection a b = noneHighestCompare (projection a) (projection b)
    let noneHighestCompareDescendingBy projection a b = noneHighestCompareDescending (projection a) (projection b)

    let noneHighestCompareByMode ascending =
      if ascending
      then noneHighestCompareBy
      else noneHighestCompareDescendingBy

    let combine combiner o1 o2 =
      match o1, o2 with
      | Some x, Some y -> combiner x y |> Some
      | (Some _ as x), None -> x
      | None, (Some _ as y) -> y
      | None, None -> None

  let inline private optionBind binder = function
    | Some x -> binder x
    | None -> None

  // Adapted from FSharpX: https://github.com/fsprojects/FSharpx.Extras/blob/master/src/FSharpx.Extras/ComputationExpressions/Option.fs
  type OptionBuilder() =
    member _.Return x = Some x
    member _.ReturnFrom (x: _ option) = x
    member _.Bind (o, f) = Option.bind f o
    member _.Zero () = None
    member _.Combine (o, f) = Option.bind f o
    member _.Delay (f: unit -> _) = f
    member _.Run f = f()

    member this.TryWith (o, h) =
      try this.ReturnFrom o
      with e -> h e

    member this.TryFinally (o, compensation) =
      try this.ReturnFrom o
      finally compensation()

    member this.Using (res: #System.IDisposable, body) =
      this.TryFinally (body res, fun () -> if res |> box |> isNull |> not then res.Dispose())

    member this.While (guard, f) =
      if not <| guard() then
        Some ()
      else
        f() |> ignore
        this.While (guard, f)

    member this.For (sequence: _ seq, body) =
      this.Using (sequence.GetEnumerator(), fun enum ->
        this.While (enum.MoveNext, this.Delay (fun () -> body enum.Current)))
  
  let option = OptionBuilder()



// Helpful functions / extensions for collections.
[<AutoOpen>]
module internal CollectionExtentions =
  module List =
    let inline cons h t = h::t


  module Seq =
    let inline sortByMode ascending =
      if ascending
      then Seq.sortBy
      else Seq.sortByDescending

    let inline private tryReduce reduction seq =
      if Seq.isEmpty seq
      then None
      else Some <| reduction seq
    let tryMin seq = tryReduce Seq.min seq
    let tryMax seq = tryReduce Seq.max seq
    let tryMinBy projection = tryReduce (Seq.minBy projection)
    let tryMaxBy projection = tryReduce (Seq.maxBy projection)


  module Set =
    let inline tryAddOrRemove select item =
      if select
      then Set.add item
      else Set.remove item

    let inline addOrRemove item set =
      if set |> Set.contains item
      then set.Remove item
      else set.Add item


  module Map =
    let inline ofKeys keyMapping = Seq.map (fun k -> k, keyMapping k) >> Map.ofSeq
    let inline ofValues valueMapping = Seq.map (fun v -> valueMapping v, v) >> Map.ofSeq
    let inline mapByKey keyMapping = Map.map (fun k _ -> keyMapping k)
    let inline mapByValue valueMapping = Map.map (fun _ v -> valueMapping v)
    let inline keys map = Map.toSeq map |> Seq.map fst
    let inline values map = Map.toSeq map |> Seq.map snd
    let inline update key update map = map |> Map.add key (update map.[key])

  type Map<'a, 'b when 'a: comparison> with
    member this.Find key = this.[key]
    member this.Keys = Map.keys this
    member this.Values = Map.values this
    member this.Pairs = Map.toSeq this


  open System.Collections.Generic

  type Dictionary<'a, 'b> with
    member this.Find key = this.[key]
    member this.TryFind key =
      match this.TryGetValue key with
      | true, x -> Some x
      | _ -> None

  and Cache<'a, 'b> = Dictionary<'a, 'b>

  let memoize func =
    let cache = Cache()
    fun x ->
      match cache.TryFind x with
      | Some y -> y
      | None ->
          let y = func x
          cache.Add(x, y)
          y

  let memoize2 func =
    let cache = Cache()
    fun x y ->
      match cache.TryFind (x, y) with
      | Some z -> z
      | None ->
          let z = func x y
          cache.Add((x, y), z)
          z

  let memoize3 func =
    let cache = Cache()
    fun x y z ->
      match cache.TryFind (x, y, z) with
      | Some w -> w
      | None ->
          let w = func x y z
          cache.Add((x, y, z), w)
          w
