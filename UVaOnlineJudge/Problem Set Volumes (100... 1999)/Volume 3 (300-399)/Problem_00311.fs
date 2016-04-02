namespace UVaOnlineJudge

module Problem_311 =
  // Input
  let order = [7; 5; 1; 0; 0; 0]

  let sizeParcel = 36.0;
  let numberOfParcels = order |> List.mapi (fun i numberOfPackets -> numberOfPackets * pown (i + 1) 2)
                              |> List.reduce (+)
                              |> fun totalSize -> int (ceil (float totalSize) / sizeParcel)
  
  printfn "%d" numberOfParcels