module internal StardewValleyStonks.WebApp.Worker

open Fable.Core
open YALPS

type Input = Model<int, string> array
type Output = int Solution array

let [<Global>] private postMessage: Output -> unit = jsNative

let [<Global>] mutable private onmessage: Browser.Types.MessageEvent -> unit = jsNative

onmessage <- fun message ->
  match message.data with
  | :? Input as data -> postMessage (data |> Array.map Solver.solve)
  | _ -> assert false
