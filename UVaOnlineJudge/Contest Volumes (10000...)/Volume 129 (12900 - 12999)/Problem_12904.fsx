namespace UVaOnlineJudge

module Problem_12904 =
  // Input
  let input = [0; 40; 41; 80; 85; 120; 121; 150; 155]

  let median list =
    let length = List.length list
    let index = length / 2
    let median = list |> List.skip (index - 1)
                      |> List.head
    (index, median)

  let sortedInput = input |> List.sort
  let index2nd, median2nd = sortedInput |> median
  let _, median1st        = sortedInput |> List.take index2nd 
                                        |> median
  let _, median3rd        = sortedInput |> List.skip index2nd
                                        |> median

  printfn "%d %d %d" median1st median2nd median3rd