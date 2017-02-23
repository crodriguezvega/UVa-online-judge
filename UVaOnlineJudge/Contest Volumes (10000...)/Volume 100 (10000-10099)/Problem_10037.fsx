module Problem_10037 =
  // Inputs
  let times = [ 1; 2; 5; 10 ]

  let slowestCalculation times = times |> Seq.ofList
                                       |> Seq.sortBy (~-)
                                       |> Seq.take 2
                                       |> List.ofSeq
  let fastestCalculation times = times |> Seq.ofList
                                       |> Seq.sort
                                       |> Seq.take 2
                                       |> List.ofSeq

  let rec calculateCrossings times totalTime crossings =
    match times with
    | [] -> (crossings, totalTime)
    | [ head; tail ] ->
      let slowest = times |> slowestCalculation
      let firstSlowest = slowest.[0]
      let secondSlowest = slowest.[1]
      let tt = totalTime + firstSlowest

      let cr = crossings @ (List.append List.empty [ (sprintf "%d %d" secondSlowest firstSlowest) ])
      calculateCrossings List.empty tt cr
    | _ ->
      let fastest = times |> fastestCalculation
      let firstFastest = fastest.[0]
      let secondFastest = fastest.[1]
      let slowest1 = times |> slowestCalculation
      let firstSlowest1 = slowest1.[0]
      let secondSlowest1 = slowest1.[1]

      if (secondSlowest1 + firstFastest < 2 * secondFastest) then        
        let tt1 = totalTime + firstFastest + firstSlowest1
        let cr1 = List.append List.empty [ (sprintf "%d %d" firstFastest firstSlowest1)
                                           (sprintf "%d" firstFastest) ]
        
        let times1 = List.filter (fun x -> x <> firstSlowest1) times
        let slowest2 = times1 |> slowestCalculation
        let firstSlowest2 = slowest2.[0]
        let secondSlowest2 = slowest2.[1]
        let tt2 = tt1 + 2 * firstFastest + firstSlowest2 + secondSlowest2
        let cr2 = crossings @ (List.append cr1 [ (sprintf "%d %d" firstFastest secondSlowest2);
                                                 (sprintf "%d" firstFastest);
                                                 (sprintf "%d %d" firstFastest firstSlowest2) ])

        let times2 = List.filter (fun x -> x <> firstFastest && x <> firstSlowest2 && x <> secondSlowest2) times
        calculateCrossings times2 tt2 cr2
      else
        let tt = totalTime + 2 * secondFastest + firstFastest + firstSlowest1
        let cr = crossings @ (List.append List.empty [ (sprintf "%d %d" firstFastest secondFastest)
                                                       (sprintf "%d" firstFastest)
                                                       (sprintf "%d %d" secondSlowest1 firstSlowest1)
                                                       (sprintf "%d" secondFastest) ])

        let times2 = List.filter (fun x -> x <> firstSlowest1 && x <> secondSlowest1) times
        calculateCrossings times2 tt cr

  let result = calculateCrossings times 0 List.empty
  printfn "%d" (snd result)
  List.iter (fun x -> printfn "%s" x) (fst result)