namespace UVaOnlineJudge

module Problem_11091 =
  // Input
  let numberOfNodes = 3
  let graph = [[1; 2]; [0; 2]; [0; 1]]

  type color =
    | None
    | Blue
    | Red

  let setColor = function
    | Blue -> Red
    | Red -> Blue
    | None -> Blue

  let isBicolorable graph =
    let rec loop graph (colors: color array) node =
      match graph with
      | [] -> true
      | hd :: tl when colors.[node] = None ->
        colors.[node] <- setColor colors.[node]
        List.iter (fun x -> colors.[x] <- setColor colors.[node]) hd
        loop tl colors (node + 1)
      | hd :: tl ->
        if List.exists (fun x -> colors.[x] = colors.[node]) hd then
          false
        else
          List.iter (fun x -> colors.[x] <- setColor colors.[node]) hd
          loop tl colors (node + 1)
    loop graph (Array.create (numberOfNodes) None) 0

  match (isBicolorable graph) with
  | true -> printfn "BICOLORABLE."
  | false -> printfn "NOT BICOLORABLE."