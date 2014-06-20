namespace UVaOnlineJudge

module Problem_568 =
  // Inputs
  let input = 9999

  let factorial number =
    let rec carryLoop result carry =
      match carry with
      | 0 -> result
      | _ -> carryLoop ((carry % 10) :: result) (carry / 10)
    let rec multiplicationLoop factorial n result carry =
      match factorial with
      | [] -> List.rev (carryLoop result carry)
      | head::tail ->
        let x = head * n + carry
        multiplicationLoop tail n ((x % 10) :: result) (x / 10)
    let rec factorialLoop n result =
      match n with
      | _ when n > number -> result
      | _ -> factorialLoop (n + 1) (multiplicationLoop result n [] 0)
    List.rev (factorialLoop 1 [1])

  let printSolution = printfn "%d -> %A" input
  (factorial input) |> List.filter (fun x -> x <> 0)
                    |> List.rev
                    |> List.head
                    |> printSolution

