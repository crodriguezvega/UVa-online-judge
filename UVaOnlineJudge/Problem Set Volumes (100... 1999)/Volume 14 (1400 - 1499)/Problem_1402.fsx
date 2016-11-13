module Problem_1402 =
  // Input
  let values = [3; 4; 5; 1; 6; 2]

  type sample = { value: int32; index: uint32 }

  let toSamples values =
    let rec loop values samples =
      match values with
      | [] -> samples |> List.rev
      | head :: tail ->
        let sampleOption = samples |> List.tryFindBack (fun x -> x.value = head)
        let index = match sampleOption with
                    | None -> 0u
                    | Some(sample) -> sample.index + 1u
        loop tail ({ value = head; index = index } :: samples)
    loop values []

  let findOrder samples =
    let rec loop sortedSamples samples skip = 
      match sortedSamples with
      | [] -> []
      | head :: tail ->
        let index = samples |> List.skip skip
                            |> List.filter (fun x -> x.value = head)
                            |> List.map (fun x -> x.index)
                            |> List.min

        let take = 1 + (samples |> List.skip skip
                                |> List.findIndex (fun x -> x.value = head && x.index = index))
        let position = take + skip

        let a = samples |> List.take skip
        let b = samples |> List.skip skip
                        |> List.take take
                        |> List.rev
        let c = samples |> List.skip (skip + take)

        position :: loop tail (a @ b @ c) (skip + 1)
    loop (samples |> List.map (fun x -> x.value) |> List.sort) samples 0

  let order = values |> toSamples |> findOrder
  printfn "%A" order