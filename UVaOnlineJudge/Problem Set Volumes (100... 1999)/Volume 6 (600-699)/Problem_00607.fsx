module Problem_607 =
  // Inputs
  let L = 120
  let C = 10
  let topics = [80; 80; 10; 50; 30; 20; 40; 30; 120; 100]

  let rec calculateDissatisfactionIndex maxDuration lecture =
    match lecture with
    | [] -> 0
    | head :: tail ->
      let index = match (maxDuration - List.sum head) with
                  | t when t = 0 -> 0
                  | t when t >= 1 && t <= 10 -> -C
                  | t -> pown (t - 10) 2
      index + calculateDissatisfactionIndex maxDuration tail

  let rec calculateLectures maxDuration topics =
    match topics with
    | [] -> [[]]
    | _ ->
        let possibleLectures = [1..List.length topics] |> List.map (fun x -> List.take x topics)
        let candidateLectures = possibleLectures |> List.takeWhile (fun x -> List.sum x <= maxDuration)

        [for lecture in candidateLectures do
          let numberOfTopics = List.length lecture
          let tail = List.skip numberOfTopics topics
          let nextlectures = calculateLectures maxDuration tail
          yield! List.map (fun x -> [lecture] @ x) nextlectures]

  let candidateLectures = calculateLectures L topics
  let lecture = candidateLectures |> List.minBy (fun x -> calculateDissatisfactionIndex L x)
  
  printfn "Minimum number of lectures: %d" (lecture |> List.length)
  printfn "Total dissatisfaction index: %d" (calculateDissatisfactionIndex L lecture)                   