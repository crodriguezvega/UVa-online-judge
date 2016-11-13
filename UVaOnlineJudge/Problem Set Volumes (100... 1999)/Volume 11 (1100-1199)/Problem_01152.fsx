module Problem_1152 =
  // Input
  let A = [-45; -41; -36; -36; 26; -32]
  let B = [22; -27; 53; 30; -38; -54]
  let C = [42; 56; -37; -75; -10; -6]
  let D = [-16; 30; 77; -46; 62; 45]

  let sumPairs list1 list2 =
    let rec loop list1 list2 =
      match list1 with
      | [] -> []
      | head :: tail -> (List.map (fun x -> x + head) list2) :: loop tail list2
    loop list1 list2 |> List.concat

  let rec countQuadruplets list1 list2 =
    match list1 with
    | [] -> 0
    | (value1, occurences1) :: tail ->
      let occurences2 = match List.tryFind (fun (value2, occurences2) -> value2 = -value1) list2 with
                        | None -> 0
                        | Some(value2, occurrences2) -> occurrences2
      occurences1 * occurences2 + countQuadruplets tail list2   

  let E = sumPairs A B |> Seq.ofList |> Seq.countBy id |> List.ofSeq
  let F = sumPairs C D |> Seq.ofList |> Seq.countBy id |> List.ofSeq

  let result = countQuadruplets E F
  printfn "%d" result