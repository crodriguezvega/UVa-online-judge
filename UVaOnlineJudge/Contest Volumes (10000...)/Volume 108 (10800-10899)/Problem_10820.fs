namespace UVaOnlineJudge

module Problem_10820 =
  // Input
  let N = 5

  let rec sieve numbers =
    match numbers with
    | [] -> []
    | (head :: tail) -> head :: sieve [for x in tail do if x % head > 0 then yield x]

  let factorial number = [1..number] |> List.reduce (*)

  let combinationsWithRepetitions chooseFrom chooseOf =
    let a = [chooseFrom..chooseFrom + chooseOf - 1] |> List.reduce (*)
    let b = [1..chooseOf] |> List.reduce (*)
    a / b

  let numberOfPrimes = sieve [2..N] |> List.length
  let result = combinationsWithRepetitions numberOfPrimes 2

  printfn "%d" result