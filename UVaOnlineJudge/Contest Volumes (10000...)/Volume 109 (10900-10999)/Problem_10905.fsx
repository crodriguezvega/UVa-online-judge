module Problem_10905 =
  // Input
  let input = [123; 124; 56; 90; 9]

  let numberOfDigits number = int (log10 (float number)) + 1;

  let toDigits number =
    let rec loop number =
      let quotient = number / 10
      let remainder = number % 10
      match quotient with
      | 0 -> [remainder]
      | _ -> remainder :: loop quotient
    loop number |> List.rev

  let rec compare lhs rhs =
    match lhs, rhs with
    | [], _ -> -1
    | _, [] -> 1
    | headLhs :: tailLhs, headRhs :: tailRhs ->
      if headLhs <> headRhs then headRhs - headLhs
      else compare tailLhs tailRhs

  let sortFunction lhs rhs =
    let numberDigitsLhs = numberOfDigits lhs
    let numberDigitsRhs = numberOfDigits rhs

    if numberDigitsLhs <> numberDigitsRhs then
      let digitsLhs = toDigits lhs
      let digitsRhs = toDigits rhs
      compare digitsLhs digitsRhs
    else
      rhs - lhs

  let number = input |> List.sortWith sortFunction
                     |> List.map (fun x -> sprintf "%d" x)
                     |> List.fold (fun acc x -> acc + x) ""
  printfn "%s" number