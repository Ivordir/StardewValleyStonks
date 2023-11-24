module internal StardewValleyStonks.WebApp.Worker

open Fable.Core
open YALPS

type Input = Model<int, string> array
type Output = int Solution array

let [<Global>] private postMessage: Output -> unit = jsNative

let [<Global>] mutable private onmessage: Browser.Types.MessageEvent -> unit = jsNative

onmessage <- fun message ->
  match message.data with
  | :? Input as data ->
    #if DEBUG
    console.time "solve"
    #endif
    let solutions = data |> Array.map Solver.solve
    #if DEBUG
    console.timeEnd "solve"
    #endif
    postMessage solutions
  | _ -> assert false
