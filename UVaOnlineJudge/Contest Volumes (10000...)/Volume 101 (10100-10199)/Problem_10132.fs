namespace UVaOnlineJudge

module Problem_10132 =
  // Input
  let fragments = ["011";
                   "01";
                   "0";
                   "10";
                   "0111";
                   "110"]
  
  let rec findMatches matches combinations =
    match matches with
    | [] -> []
    | head :: tail ->
      let isMatch = combinations |> List.exists (fun x -> x = head)
      if isMatch then head :: findMatches tail combinations
      else findMatches tail combinations

  let rec findCombinations fragment fragments =
    match fragments with
    | [] -> []
    | head :: tail -> (fragment + head) :: findCombinations fragment tail

  let findOriginalFile fragments =
    if (List.length fragments = 0) then None
    else
      let rec loop remainingFragments allFragments matches =
        match remainingFragments with
        | [] -> None
        | head :: tail ->
          let combinations = findCombinations head fragments
          let newMatches = findMatches matches combinations
          if (List.length newMatches = 1) then Some(newMatches.[0])
          else loop tail allFragments newMatches

      let firstFragment = fragments |> List.head
      let remainingFragments = fragments |> List.tail
      let matches = findCombinations firstFragment fragments
      loop remainingFragments fragments matches

  match findOriginalFile fragments with
  | None -> printfn ""
  | Some(bitPattern) -> printfn "%s" bitPattern