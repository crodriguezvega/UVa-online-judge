module Problem_1618 =
  // Input
  let key = [30; 40; 10; 20; 80; 50; 60; 70]

  let rec combinations n l = 
    match n, l with
    | 0, _ -> [[]]
    | _, [] -> []
    | k, (x::xs) -> List.map ((@) [x]) (combinations (k-1) xs) @ combinations k xs

  let satisfiesWeakCondition combination =
    match combination with
    | [Np; Nq; Nr; Ns] -> ((Nq > Ns && Ns > Np && Np > Nr) || (Nq < Ns && Ns < Np && Np < Nr))
    | _ -> failwith "Wrong length of subset"

  let rec isWeakKey key =
    combinations 4 key
    |> List.filter (fun x -> satisfiesWeakCondition x)
    |> List.isEmpty
    
  match (isWeakKey key) with
  | true -> printfn "NO"
  | false -> printfn "YES"