module Problem_562 =
  // Input
  let coins = [6; 7; 8; 21; 38; 100; 101]

  let split coins =
    let half = (List.sum coins) / 2
    let rec loop coins coinsForA coinsForB =
      match coins with
      | [] -> (coinsForA, coinsForB)
      | head :: tail ->
        match head with
        | _ when head + (List.sum coinsForA) > half -> loop tail coinsForA (head :: coinsForB)
        | _ -> loop tail (head :: coinsForA) coinsForB
    loop (List.rev (List.sort coins)) List.empty List.empty

  let coinsForA, coinsForB = split coins
  printfn "%d" (abs ((List.sum coinsForA) - (List.sum coinsForB)))