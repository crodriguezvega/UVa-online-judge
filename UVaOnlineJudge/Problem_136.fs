namespace UVaOnlineJudge

module Problem_136 =
  // Input
  let input = 1500

  let rec isUgly n =
    match n with
    | 1 -> true
    | n when n % 2 = 0 -> isUgly (n / 2)
    | n when n % 3 = 0 -> isUgly (n / 3)
    | n when n % 5 = 0 -> isUgly (n / 5)
    | _ -> false
  let uglyNumber i =
    let rec loop iterator solution  =
      match iterator, solution with
      | iterator, solution when iterator = 1 && isUgly solution -> Some(solution)
      | iterator, solution when iterator > 1 && isUgly solution -> loop (iterator - 1) (solution + 1)
      | iterator, solution when not (isUgly solution) -> loop iterator (solution + 1)
      | _, _ -> None
    loop i 1
  let printUglyNumber result =
    match result with
    | Some(result) -> printfn "The 1500'th ugly number is %d." result
    | None -> printfn "Error."

  printUglyNumber (uglyNumber input)
