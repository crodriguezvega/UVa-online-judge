module Problem_10041 =
  // Inputs
  let streetNumbers = [ 2; 0; 8; 9; 1; 4 ]

  let (|Zero|Even|Odd|) n =
    if n = 0 then Zero
    else if n % 2 = 0 then Even
    else Odd

  let findMedian numbers =
    let length = List.length numbers
    match length with
    | Zero -> 0
    | Even ->
      let position2 = length / 2
      let position1 = position2 - 1
      let element2 = numbers.[position2]
      let element1 = numbers.[position1]
      (element1 + element2) / 2
    | Odd ->
      let position = (length - 1) / 2
      numbers.[position]

  let calculateSumOfDistances median numbers =
    List.fold (fun acc elem -> acc + abs (median - elem)) 0 numbers

  let totalDistance = List.sort streetNumbers |> findMedian
                                              |> calculateSumOfDistances <| streetNumbers

  printfn "%d" totalDistance
