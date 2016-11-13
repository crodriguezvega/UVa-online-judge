module Problem_10034 =
  type Vertex (x, y) = class
    member this.X = x
    member this.Y = y
  end

  let distance (vertex1: Vertex, vertex2: Vertex) = sqrt (float ((pown (vertex1.X - vertex2.X) 2) + (pown (vertex1.Y - vertex2.Y) 2)))

  type Edge (vertex1: Vertex, vertex2: Vertex) = class
    member this.Vertex1 = vertex1
    member this.Vertex2 = vertex2
    member this.Distance = distance (this.Vertex1, this.Vertex2)
  end

  // Input 
  let vertices = [new Vertex (1.0, 1.0);
                  new Vertex (2.0, 2.0);
                  new Vertex (2.0, 4.0)]

  let minimumLength vertices =
    let rec minimumDistanceEdge vertices tree edges =
      match tree with
      | [] -> List.minBy (fun (x: Edge) -> x.Distance) edges
      | hd :: tl ->
        let vertex = List.minBy (fun x -> distance(x, hd)) vertices
        minimumDistanceEdge vertices tl (new Edge (hd, vertex) :: edges)
    let rec loop vertices tree distance =
      match vertices with
      | [] -> distance
      | _ ->
        let edge = minimumDistanceEdge vertices tree []
        loop (List.filter (fun x -> x <> edge.Vertex2) vertices) (edge.Vertex2 :: tree) (distance + edge.Distance)
    loop (List.tail vertices) [(List.head vertices)] 0.0

  printfn "%f" (minimumLength vertices)