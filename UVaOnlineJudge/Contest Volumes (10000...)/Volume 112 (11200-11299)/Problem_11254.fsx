module Problem_11254 =
  let input = 1000

  let sumOfConsecutiveIntegers input =
    let rec loop length range (sequences: list<int * int * int>) =
      let tail = Seq.skip 1 range
      match length, range with
      | _, _ when Seq.isEmpty range -> sequences
      | _, _ when length > Seq.length range ->
        loop 2 tail sequences
      | _, _ ->
        let sequence = Seq.take length range
        let sequenceSum = Seq.sum sequence
        match sequenceSum with 
        | _ when sequenceSum = input ->
          loop 2 tail ((Seq.head sequence, Seq.last sequence, Seq.length sequence) :: sequences)
        | _ when sequenceSum > input ->
          loop 2 tail sequences
        | _ -> loop (length + 1) range sequences
    loop 2 (seq {1 .. int (ceil (float (input) / 2.0))}) [(input, input, 1)]

  sumOfConsecutiveIntegers input |> List.maxBy (fun (_, _, length) -> length)
                                 |> fun (start, stop, _) -> printfn "%d + ... + %d" start stop
