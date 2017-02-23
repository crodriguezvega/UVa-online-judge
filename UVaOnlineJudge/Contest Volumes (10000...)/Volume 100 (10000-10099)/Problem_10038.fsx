module Problem_10038 =
  // Inputs
  let numbers = [5; 1; 4; 2; -1; 6]

  let max = List.max numbers
  let result = numbers |> List.toSeq
                       |> Seq.pairwise
                       |> Seq.map (fun (x, y) -> abs (x - y))
                       |> Seq.distinct
                       |> Seq.sum
                       |> fun x -> match x with
                                   | x when x = max * (max - 1) / 2 -> "Jolly"
                                   | _ -> "Not jolly"

  printfn "%s" result