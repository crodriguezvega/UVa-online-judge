namespace UVaOnlineJudge

// Solved with a variation of the longest common subsequence algorithm (https://en.wikipedia.org/wiki/Longest_common_subsequence_problem)
module Problem_00111 =
  // Input
  let order = [3; 1; 2; 4; 9; 5; 10; 6; 8; 7]
  let rank = [1; 2; 3; 4; 5; 6; 7; 8; 9; 10]

  // Length of common subsequence
  let rec lcs order rank =
    match rank with
    | [] -> 0
    | head :: tail ->
      match List.tryFind (fun x -> x = head) order with
      | None -> 0
      | Some value ->
        let subOrder = order |> Seq.ofList |> Seq.skipWhile (fun x -> x <> value) |> List.ofSeq
        1 + lcs subOrder tail

  // Lengths of common subsequences
  let rec lcss order rank =
    match rank with
    | [] -> []
    | head :: tail -> lcs order rank :: lcss order tail

  let maximum = lcss order rank |> List.max
  printfn "%d" maximum