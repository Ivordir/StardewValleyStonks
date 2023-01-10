module internal StardewValleyStonks.WebApp.Worker

open Fable.Core

open YALPS

let [<Global>] private postMessage: obj -> unit = jsNative

let [<Global>] mutable private onmessage: Browser.Types.MessageEvent -> unit = jsNative

onmessage <- fun message ->
  match message.data with
  | :? (Model<int, string> array) as data ->
    postMessage (data |> Array.map (fun model ->
      let solution = Solver.solve model
      // The nature of the problem should prevent infeasible or unbounded solutions.
      assert (solution.status = Optimal)
      solution))
  | _ -> assert false
