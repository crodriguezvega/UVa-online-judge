namespace UVaOnlineJudge

module Problem_100 =
  // Inputs
  let i = 900
  let j = 1000

  let rec cycleLength input =
    match input with
    | 1 -> 1
    | n when n % 2 = 0 -> 1 + cycleLength (n / 2)
    | n -> 1 + cycleLength (3 * n + 1)
  let maximumCycleLength x y =
    [x..y] |> List.map cycleLength
           |> List.max

  printfn "%d %d %d" i j (maximumCycleLength i j)
