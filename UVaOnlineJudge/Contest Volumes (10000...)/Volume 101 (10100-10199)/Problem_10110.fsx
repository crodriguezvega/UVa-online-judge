module Problem_10110 =
  // Inputs
  let n = uint32 6241

  let light (n: uint32) = n |> float
                            |> sqrt
                            |> fun x -> match x with
                                        | x when x % 1.0 > 0.0 -> "no"
                                        | _ -> "yes"

  printf "%s" (light n)