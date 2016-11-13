module Problem_10820 =
  // Input
  let N = 50000

  let rec sieve numbers =
    match numbers with
    | [] -> []
    | head :: tail -> head :: sieve [for x in tail do if x % head > 0 then yield x]

  let permutationsWithRepetition chooseFrom chooseOf = pown chooseFrom chooseOf

  let numberOfPrimesIncludingOne = 1 + (sieve [2..N] |> List.length)
  let permutationsOfPrimes = permutationsWithRepetition numberOfPrimesIncludingOne 2
  let permutationsOfMultiples = (numberOfPrimesIncludingOne - 1) * (N - numberOfPrimesIncludingOne)
  let result = permutationsOfPrimes + permutationsOfMultiples

  printfn "%d" result