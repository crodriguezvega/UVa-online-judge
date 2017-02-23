module Problem_120 =
  // Inputs
  let pancakes = [ 5; 1; 2; 3; 4 ]

  let takeFrom pancakes count =
    pancakes |> Seq.ofList
             |> Seq.take count
             |> List.ofSeq

  let flip (pancakes: int list) index =
    let flippedIndex =
      match index with
      | i when i = pancakes.Length - 1 -> 0
      | _ -> index
    let flippedPancakes = pancakes |> Seq.ofList
                                   |> Seq.skip flippedIndex
                                   |> List.ofSeq
                                   |> List.rev
    match flippedIndex with
    | 0 ->
      (flippedIndex + 1), flippedPancakes
    | _ ->
      let rest = takeFrom pancakes flippedIndex
      (flippedIndex + 1), (rest @ flippedPancakes)

  let rec sort pancakes =
    let indexedPancakes = pancakes |> List.mapi (fun i x -> (i, x))
    let indexOfMax = indexedPancakes |> List.maxBy snd
                                     |> fst
    let indexOfMin = indexedPancakes |> List.minBy snd
                                     |> fst

    let length = pancakes.Length
    let lastIndex = length - 1
    match indexOfMax, indexOfMin  with
    | 0, i when i = lastIndex -> [ 0 ]
    | 0, _->
      let flipped = flip pancakes indexOfMin
      fst flipped :: (sort (snd flipped))
    | _, _ ->
      let flipped = flip pancakes indexOfMax
      fst flipped :: (sort (snd flipped))

  printfn "%A" (sort (List.rev pancakes))