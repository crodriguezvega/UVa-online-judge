namespace UVaOnlineJudge

module Problem_10591 =
  // Input
  let number = 13

  let sumOfSquareOfDigits number =
    let rec loop n acc =
      match n with
      | 0 -> acc
      | _ -> loop (n / 10) (acc + (pown (n % 10) 2))
    loop number 0

  let isHappyNumber number =
    let rec loop n =
      match n with
      | 1 -> Some true
      | _ when n < 10 -> None
      | _ -> loop (sumOfSquareOfDigits n)
    if number < 10 then loop (pown number 2)
    else loop number

  match (isHappyNumber number) with
  | Some value -> printfn "%d is a happy number." number
  | None -> printfn "%d is an unhappy number." number