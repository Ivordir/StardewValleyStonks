let r = System.Random ()

let testExpectedTravelingCartPrice =
  let randomPrice seedPrice = max (100 * r.Next(1, 11)) (seedPrice * r.Next(3, 6))
  let trials = 100_000
  let simulate seedPrice =
    let mutable avg = 0.0
    for i = 1 to trials do
      avg <- avg + float (randomPrice seedPrice) / float trials
    avg
  let confirm seedPrice =
    let actual = simulate seedPrice
    let expected = expectedTravelingCartPrice seedPrice
    let diff = abs (actual - expected)
    assert (diff <= 0.001 * actual)
