namespace UVaOnlineJudge

module Problem_11176 =
  // Input
  let numberOfGames = 10
  let winningProbability =  0.75

  type outcome = Win | Lose

  let initialStreak = List.init numberOfGames (fun index -> Lose)

  let generateStreaks initialStreak =
    let streakLength = List.length initialStreak
    let rec loop index streaks =
      match index with
      | _ when index < 0 -> streaks
      | _ ->
        let newStreaks = [for streak in streaks do
                           let arr = Array.ofList streak
                           arr.[index] <- Win
                           yield List.ofArray arr]
        loop (index - 1) (newStreaks @ streaks) 
    loop (streakLength - 1) [initialStreak]

  let calculateLengthWinningStreak streak =
    let rec loop lengthWinningStreak streak =
      match streak with
      | [] ->
        if lengthWinningStreak > 0 then [lengthWinningStreak]
        else []
      | head :: tail ->
        match head with
        | Win ->
          loop (lengthWinningStreak + 1) tail
        | Lose ->
          if lengthWinningStreak > 0 then lengthWinningStreak :: loop 0 tail
          else loop 0 tail      
    let lengths = loop 0 streak
    match lengths with
    | [] -> 0
    | _ -> List.max lengths

  let rec calculateProbability streak =
    match streak with
    | [] -> 1.0
    | head :: tail ->
      let probability = match head with
                        | Win -> winningProbability
                        | Lose -> 1.0 - winningProbability
      probability * calculateProbability tail

  let streaks = generateStreaks initialStreak
  let lengthsWinningStreak = streaks |> List.map calculateLengthWinningStreak
  let expectedLengthWinningStreak = streaks |> List.map calculateProbability
                                            |> List.zip lengthsWinningStreak
                                            |> List.map (fun (length, probability) -> float length * probability)
                                            |> List.reduce (+)
  printfn "%f" expectedLengthWinningStreak