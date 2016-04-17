namespace UVaOnlineJudge

// http://math.stackexchange.com/questions/226868/number-of-trailing-zeros-in-a-factorial-in-base-b
// http://mathforum.org/library/drmath/view/55723.html

module Problem_10061 =
  // Input
  let number = 20
  let system = 8

  let rec sieve numbers =
    match numbers with
    | [] -> []
    | head :: tail -> head :: sieve [for x in tail do if x % head > 0 then yield x]

  let factorize number =
    let rec loop number primes =
      match primes with
      | [] -> []
      | head :: tail ->
        if number % head = 0 then head :: loop (number / head) (head :: tail)
        else loop number tail
    let primes = sieve [2 .. number]
    loop number primes |> Seq.ofList
                       |> Seq.countBy id
                       |> List.ofSeq

  let numberTrailingZeros number system =
    let rec loop number prime power =
      let n = number / (pown prime power)
      if n = 0 then 0
      else n + loop number prime (power + 1)
    let factors = factorize system
    factors |> List.map (fun (prime, power) -> (loop number prime 1) / power)
            |> List.min

  let numberOfDigits number system =
    let a = log10 (float system)
    let b = [2.0 .. float number] |> List.fold (fun acc x -> acc + log10 x) 0.0
    int (b / a + 1.0)

  let trailingZeros = numberTrailingZeros number system
  let digits = numberOfDigits number system

  printfn "%d %d" trailingZeros digits