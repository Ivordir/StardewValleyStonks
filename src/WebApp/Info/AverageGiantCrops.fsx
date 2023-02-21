let tile field =
  let height = Array2D.length1 field
  let width = Array2D.length2 field
  let tiled = Array2D.zeroCreate (height + 2) (width + 2)
  tiled[..(height - 1), ..(width - 1)] <- field
  tiled[height.., ..(width - 1)] <- field[0..1, *]
  tiled[..(height - 1), width..] <- field[*, 0..1]
  tiled[height.., width..] <- field[0..1, 0..1]
  tiled

let calculate infinite field =
  let height = Array2D.length1 field
  let width = Array2D.length2 field
  let field = if infinite then tile field else field

  let tiles = Seq.allPairs (Array.init height id) (Array.init width id)

  let numGiant = tiles |> Seq.sumBy (fun (y, x) ->
    if field[y..(y + 2), x..(x + 2)] |> Seq.cast |> Seq.sum = 9
    then 9
    else 0)

  let numCrops = tiles |> Seq.sumBy (fun (y, x) -> field[y, x])

  float numGiant / float numCrops

let plainField height width = Array2D.create height width 1

let sprinklerField sprinklerRange numYSprinklers numXSprinklers =
  let height = numYSprinklers * sprinklerRange
  let width = numXSprinklers * sprinklerRange
  let field = plainField height width
  let startingOffset = sprinklerRange / 2

  Seq.allPairs
    [| startingOffset..sprinklerRange..height |]
    [| startingOffset..sprinklerRange..width |]
  |> Seq.iter (fun (y, x) -> field[y, x] <- 0)

  field

let iridiumSprinklerField = sprinklerField 5
let pressuredIridiumSprinklerField = sprinklerField 7

let juminoField = plainField 17 17
juminoField[7..8, 7..9] <- Array2D.zeroCreate 2 3

let iridiumSprinklerJuminoField =
  let field = iridiumSprinklerField 3 3
  field[5..9, 5..9] <- Array2D.zeroCreate 5 5
  field

let printWith infinite field =
  // printfn "%A" field
  printfn "%A" (calculate infinite field)

let print = printWith false
let printInfinite = printWith true

print (plainField 5 5)
print (plainField 10 10)
print (plainField 15 15)
print (plainField 20 20)
print (plainField 25 25)
print juminoField
printfn ""

print (iridiumSprinklerField 2 2)
print (iridiumSprinklerField 3 3)
print (iridiumSprinklerField 4 4)
printInfinite (iridiumSprinklerField 1 1)
print iridiumSprinklerJuminoField
printfn ""

print (pressuredIridiumSprinklerField 2 2)
print (pressuredIridiumSprinklerField 3 3)
print (pressuredIridiumSprinklerField 4 4)
printInfinite (pressuredIridiumSprinklerField 1 1)
printfn ""
