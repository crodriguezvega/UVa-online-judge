module Problem_10035 =
  // Inputs
  let i = 555UL
  let j = 555UL

  let countCarryOperations number1 number2 =
    let rec loop (n1: uint64) (n2: uint64) carry nrOfCarries =
      match n1, n2 with
      | 0UL, 0UL -> nrOfCarries
      | _, _ ->
        let sum = (n1 % 10UL) + (n2 % 10UL) + carry
        match sum with
        | _ when sum >= 10UL -> loop (n1 / 10UL) (n2 / 10UL) 1UL (nrOfCarries + 1)
        | _ -> loop (n1 / 10UL) (n2 / 10UL) 0UL nrOfCarries
    loop number1 number2 0UL 0

  printfn "%d carry operation(s)." (countCarryOperations i j)
