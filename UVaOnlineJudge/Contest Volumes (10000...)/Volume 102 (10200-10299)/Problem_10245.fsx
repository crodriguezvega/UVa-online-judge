module Problem_10245 =
  type Coordinate2D (x, y) = class
    member this.x = x
    member this.y = y
    member this.Distance (point: Coordinate2D) = sqrt (float ((pown (x - point.x) 2) + (pown (y - point.y) 2)))
  end

  let printDistance (distance: float) =
    match distance with
    | _ when distance >= 10000.0 -> printfn "INFINITY"
    | _ -> printfn "%F" distance

  let rec distanceBetweenPoints (points: Coordinate2D list) =
    match points with
    | [] -> []
    | head :: tail -> 
      let distances = List.map (fun x -> head.Distance x) tail
      List.append distances (distanceBetweenPoints tail)

  [new Coordinate2D (0, 2);
   new Coordinate2D (6, 67);
   new Coordinate2D (43, 71);
   new Coordinate2D (39, 107);
   new Coordinate2D (189, 140)] |> distanceBetweenPoints
                                |> List.min
                                |> printDistance
