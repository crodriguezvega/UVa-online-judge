namespace UVaOnlineJudge

module Problem_11536 = 
  // Inputs
  let N = 20
  let M = 12
  let K = 4

  let generateSequence N M K =
    let rec loop index sequence =
      match index with
      | 0 -> List.rev sequence
      | _ ->
        let value = sequence |> List.toSeq
                             |> Seq.take 3
                             |> Seq.sum
                             |> (fun x -> x % M + 1)
        loop (index - 1) (value :: sequence)
    loop (N - 3) [3 .. -1 .. 1]

  let rec contains seqA seqB =
    match seqB with
    | [] -> if List.isEmpty seqA then true else false
    | head :: tail -> contains (List.filter (fun x -> x <> head) seqA) tail

  let lengthsOfSubsequences seqA seqB =
    [for i in 0 .. List.length seqB - 1 do
      for j in 1 .. List.length seqB - i do
        let subSequence = Array.sub (List.toArray seqB) i j |> Array.toList
        if contains seqA subSequence then yield subSequence.Length]

  generateSequence N M K |> lengthsOfSubsequences [1 .. K]
                         |> (fun x ->
                              match x with
                              | [] -> printfn "sequence nai"
                              | _ -> printfn "%d" (List.min x))