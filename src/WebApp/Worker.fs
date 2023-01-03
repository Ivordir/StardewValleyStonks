module internal StardewValleyStonks.WebApp.Worker

open Fable.Core

open YALPS

let [<Global>] private postMessage: obj -> unit = jsNative

let [<Global>] mutable private onmessage: Browser.Types.MessageEvent -> unit = jsNative

onmessage <- fun message ->
  match message.data with
  | :? ((int * Model<int, string>) array) as data ->
    postMessage (data |> Array.choose (fun (i, model) ->
      let solution = Solver.solve model
      // the nature of the problem should prevent unbounded solutions
      // the profit on a regrow or bridge crop may return None,
      // preventing a fertilizer from being carried over and making the model infeasible
      assert (solution.status = Optimal || solution.status = Infeasible)
      if solution.status = Optimal
      then Some (i, solution)
      else None))
  | _ -> ()
