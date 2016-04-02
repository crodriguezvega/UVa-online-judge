namespace UVaOnlineJudge

module Problem_311 =
  // Input
  let order = [7; 5; 1; 0; 0; 0]

  let sizeParcel = 36;
  let rec findNumberOfParcels order =
    let rec loop packets size numberOfParcels =
      match packets with
      | [] -> numberOfParcels
      | head :: tail ->
        let nextSize = size - 1
        match size with
        | _ when size = 6 -> 
          loop tail nextSize (numberOfParcels + head)
        | _ when size = 5 ->
          let arr = tail |> List.rev |> Array.ofList
          arr.[0] <- arr.[0] - head * 11
          let lst = arr |> List.ofArray |> List.rev
          loop lst nextSize (numberOfParcels + head)
        | _ when size = 4 ->
          let arr = tail |> List.rev |> Array.ofList
          arr.[1] <- arr.[1] - head * 5
          let lst = arr |> List.ofArray |> List.rev
          loop lst nextSize (numberOfParcels + head)
        | _ when size = 3 ->
          let arr = tail |> List.rev |> Array.ofList

          if (head % 4 = 1) then
              arr.[1] <- arr.[1] - 5
              arr.[0] <- arr.[0] - 7
          else if (head % 4 = 2) then
              arr.[1] <- arr.[1] - 3
              arr.[0] <- arr.[0] - 6
          else if (head % 4 = 3) then
              arr.[1] <- arr.[1] - 1
              arr.[0] <- arr.[0] - 5

          let lst = arr |> List.ofArray |> List.rev
          loop lst nextSize (numberOfParcels + (head + 3) / 4)
        | _ when size = 2 ->
          let arr = tail |> List.rev |> Array.ofList
          if head > 0 then
            arr.[0] <- arr.[0] - (sizeParcel - (head % 9) * 4)  
            let lst = arr |> List.ofArray |> List.rev
            loop lst nextSize (numberOfParcels + (head + 8) / 9)
          else
            arr.[0] <- arr.[0] + head * 4
            let lst = arr |> List.ofArray |> List.rev
            loop lst nextSize numberOfParcels
        | _ when size = 1 ->
          if head > 0 then loop tail nextSize (numberOfParcels + (head + 35) / 36)
          else loop tail nextSize numberOfParcels
        | _ -> failwith "error"

    loop (List.rev order) 6 0

  printfn "%d" (findNumberOfParcels order)