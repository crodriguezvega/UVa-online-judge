namespace UVaOnlineJudge

module Problem_11254 =
  let input = 35

  let sumOfConsecutiveIntegers input =
    let rec loop length range (sequences: list<int * int * int>) =
      match length, range with
      | _, _ when Seq.isEmpty range -> sequences
      | _, _ when length > Seq.length range ->
        let tail = (range |> Seq.toList |> List.tail |> List.toSeq)
        loop 2 tail sequences
      | _, _ ->
        let sequence = Seq.take length range
        match sequence with 
        | _ when Seq.sum sequence = input ->
          let tail = (range |> Seq.toList |> List.tail |> List.toSeq)
          loop 2 tail ((Seq.head sequence, Seq.last sequence, Seq.length sequence) :: sequences)
        | _ -> loop (length + 1) range sequences
    loop 2 (seq {1 .. int (ceil (float (input) / 2.0))}) [(input, input, 1)]

  sumOfConsecutiveIntegers input |> List.maxBy (fun (_, _, length) -> length)
                                 |> fun (start, stop, _) -> printfn "%d + ... + %d" start stop