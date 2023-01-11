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
    postMessage (data |> Array.map (fun model ->
      let solution = Solver.solve model
      // The nature of the problem should prevent infeasible or unbounded solutions.
      assert (solution.status = Optimal)
      solution))
  | _ -> assert false
