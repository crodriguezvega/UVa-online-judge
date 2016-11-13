namespace UVaOnlineJudge

module Problem_311 =
  // Input
  let order = [7; 5; 1; 0; 0; 0]

  let sizeParcel = 36;
  let findNumberOfParcels order =
    let rec loop packets size numberOfParcels (packetsOfSize1, packetsOfSize2) =
      match packets with
      | [] -> numberOfParcels
      | head :: tail ->
        let newNumberOfParcels, (newPacketsOfSize1, newPacketsOfSize2) =
          match size with
          | 6 -> (numberOfParcels + head), (packetsOfSize1, packetsOfSize2)
          | 5 -> (numberOfParcels + head), (packetsOfSize1 + head * 11, packetsOfSize2)
          | 4 -> (numberOfParcels + head), (packetsOfSize1, packetsOfSize2 + head * 5)
          | 3 ->
            (numberOfParcels + (head + 3) / 4), match head % 4 with
                                                | 1 -> (packetsOfSize1 + 7, packetsOfSize2 + 5)
                                                | 2 -> (packetsOfSize1 + 6, packetsOfSize2 + 3)
                                                | 3 -> (packetsOfSize1 + 5, packetsOfSize2 + 1)
                                                | _ -> failwith "error"
          | 2 ->
            let newPacketsOfSize2 = (head - packetsOfSize2)
            if newPacketsOfSize2 > 0 then
              (numberOfParcels + (head + 8) / 9), (packetsOfSize1 + (sizeParcel - (newPacketsOfSize2 % 9) * 4), newPacketsOfSize2)
            else
              numberOfParcels, (packetsOfSize1 - newPacketsOfSize2 * 4, newPacketsOfSize2)
          | 1 ->
            let newPacketsOfSize1 = (head - packetsOfSize1)
            if newPacketsOfSize1 > 0 then
              (numberOfParcels + (newPacketsOfSize1 + 35) / 36), (newPacketsOfSize1, packetsOfSize2)
            else
              numberOfParcels, (newPacketsOfSize1, packetsOfSize2)
          | _ -> failwith "error"

        let nextSize = size - 1
        loop tail nextSize newNumberOfParcels (newPacketsOfSize1, newPacketsOfSize2)
    loop (List.rev order) 6 0 (0, 0)
  
  printfn "%d" (findNumberOfParcels order)