module Problem_927 =
  // Input
  let i = 5
  let c = [1UL; 1UL; 1UL; 1UL; 1UL; 1UL]
  let d = 1UL
  let k = 1000000UL

  let a n i coefficients = List.fold2 (fun acc coefficient order -> acc + coefficient * (pown n order)) 0UL coefficients [0..i]

  ((2UL * k) / d)
    |> (fun x -> (-1.0 + sqrt (1.0 + 4.0 * (float x))) / 2.0)
    |> (fun x -> a ((uint64 x) + 1UL) i c)
    |> printfn "%u"