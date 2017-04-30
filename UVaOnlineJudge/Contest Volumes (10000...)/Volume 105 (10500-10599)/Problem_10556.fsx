module Problem_10556 =
  let input1 = "0 0 10 10 20 20"
  let input2 = "0 0 11 11 22 22"

  type Point = { x: float; y: float }

  let toPoints coordinates =
    let arr = coordinates
                |> List.map string
                |> Array.ofList
    { x = float arr.[0]; y = float arr.[1]}

  let makePolygon (str: string) =
    str.Split(' ')
      |> List.ofArray
      |> List.chunkBySize 2
      |> List.map toPoints

  let translate polygon =
    let reference = { x = 0.0; y = 0.0 }
    let point = polygon |> List.head
    let translation = { x = reference.x - point.x; y = reference.y - point.y }
    let doTranslate point = { x = point.x + translation.x; y = point.y + translation.y }
    polygon |> List.map doTranslate
  
  let check func polygon1 polygon2 =
    let output = List.map2 func polygon1 polygon2 |> List.tail
    let head = output |> List.head
    output |> List.forall (fun x -> abs x - head < 0.00001)

  let isRotated polygon1 polygon2 =
    let angle p1 p2 = System.Math.Atan2(p1.y, p1.x) - System.Math.Atan2(p2.y, p2.x)
    check angle polygon1 polygon2

  let isScaled polygon1 polygon2 =
    let norm p = sqrt (pown p.y 2 + pown p.x 2) 
    let scale p1 p2 = norm p1 / norm p2
    check scale polygon1 polygon2

  let isSimilar polygon1 polygon2 =
    isRotated polygon1 polygon2 && isScaled polygon1 polygon2

  let polygons = [input1; input2]
                   |> List.map makePolygon
                   |> List.map translate
                   |> Array.ofList
  match isSimilar polygons.[0] polygons.[1] with
  | false -> printfn "dissimilar"
  | true -> printfn "similar"