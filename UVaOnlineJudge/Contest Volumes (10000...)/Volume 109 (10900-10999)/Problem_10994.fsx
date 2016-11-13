namespace UVaOnlineJudge

module Problem_10994 =
  // Inputs
  let p = 30
  let q = 40

  let rec F n =
    match n with
    | 0 -> 0
    | _ when n % 10 > 0 -> n % 10
    | _ -> F (n / 10)

  let S p q = [p .. q] |> List.fold (fun acc elem -> acc + F elem) 0

  printfn "%d" (S p q)