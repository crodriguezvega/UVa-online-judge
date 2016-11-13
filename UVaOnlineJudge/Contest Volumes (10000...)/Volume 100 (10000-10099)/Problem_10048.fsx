module Problem_10048 =
  // Inputs
  let crossings = [
                    ((1, 2), 50);
                    ((1, 3), 60);
                    ((2, 4), 120);
                    ((2, 5), 90);
                    ((3, 6), 50);
                    ((4, 6), 80);
                    ((4, 7), 70);
                    ((5, 7), 40);
                    ((6, 7), 140)
                  ]

  let routes = [
                 (1, 7);
                 (2, 6);
                 (6, 2)
               ]

  type Street (edges, decibels) = class
    member this.edges = fst edges :: snd edges :: []
    member this.decibels = decibels
  end

  let streets = crossings |> List.map (fun x -> Street (fst x, snd x))

  let rec toDecibels path =
    match path with
    | [] -> []
    | edge1 :: edge2 :: tail ->
      let street = streets |> List.find (fun x -> List.contains edge1 x.edges &&
                                                  List.contains edge2 x.edges) 
      street.decibels :: toDecibels (List.tail path)
    | _ :: tail -> toDecibels tail

  let generatePaths origin destination =
    let rec loop current visited =
      if current = destination then [current :: []]
      else if List.contains current visited then []
      else
        let next = streets |> List.where (fun x -> List.contains current x.edges)
                           |> List.map (fun x -> x.edges)
                           |> List.concat
                           |> List.filter (fun x -> x <> current)
        next |> List.map (fun x -> loop x (current :: visited))
             |> List.concat
             |> List.map (fun x -> List.append (current :: []) x)
    loop origin []
  
  routes |> List.map (fun x -> generatePaths (fst x) (snd x))
         |> List.map (fun x -> List.map toDecibels x)
         |> List.map (fun x -> List.map List.max x)
         |> List.iter (fun x -> match x with
                                | [] -> printfn "no path"
                                | _ -> printfn "%d" (List.min x))