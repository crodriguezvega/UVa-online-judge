namespace UVaOnlineJudge

module Problem_10012 =
  // Inputs
  let radius = [2.0; 1.0; 4.0]

  let calculateMinimumDistance (r1, r2) = sqrt (pown (r1 + r2) 2 - pown (r1 - r2) 2)

  let length = radius.Length
  let sumMinimumDistance = radius |> List.take (length - 1)
                                  |> List.zip radius.Tail
                                  |> List.map calculateMinimumDistance
                                  |> List.sum
  
  let firstRadius = radius.Head
  let lastRadius = radius |> List.skip (length - 1) |> List.head
  printfn "%.3f" (firstRadius + sumMinimumDistance + lastRadius)