module Problem_10137 =
  // Inputs
  let amounts = [15.0; 15.01; 3.0; 3.01]

  let amountsInCents = List.map (fun x -> x * 100.0) amounts
  let average = float (int (List.average amountsInCents))
  let total = amountsInCents |> List.filter (fun x -> x < average)
                             |> List.fold (fun acc elem -> acc + average - elem) 0.0

  printfn "$%.2f" (float total / 100.0)