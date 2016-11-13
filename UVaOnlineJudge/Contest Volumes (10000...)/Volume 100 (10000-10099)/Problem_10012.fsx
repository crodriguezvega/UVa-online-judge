namespace UVaOnlineJudge

module Problem_10012 =
  // Inputs
  let radiuses = [2.0; 1.0; 4.0]

  let calculateMinimumDistance (r1, r2) = sqrt (pown (r1 + r2) 2 - pown (r1 - r2) 2)

  let length = radiuses.Length
  let sumMinimumDistance = radiuses |> List.take (length - 1)
                                    |> List.zip radiuses.Tail
                                    |> List.map calculateMinimumDistance
                                    |> List.sum
  
  let firstRadius = radiuses.Head
  let lastRadius = radiuses |> List.skip (length - 1) |> List.head
  printfn "%.3f" (firstRadius + sumMinimumDistance + lastRadius)