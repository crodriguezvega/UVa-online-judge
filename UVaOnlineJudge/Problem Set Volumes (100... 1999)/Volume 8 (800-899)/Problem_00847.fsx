module Problem_847 =
  // Inputs
  let n = 34012226

  let rec game n =
    match n with
    | n when n > 18 -> game ((n + 17) / 18)
    | n when n <= 9 -> "Stan"
    | _ -> "Ollie"

  printf "%s wins." (game n)