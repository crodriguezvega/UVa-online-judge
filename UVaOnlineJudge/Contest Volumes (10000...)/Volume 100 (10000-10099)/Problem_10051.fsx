module Problem_10051 =
  type face =
    | Front of int
    | Back of int
    | Left of int
    | Right of int
    | Top of int
    | Bottom of int

  let color face =
    match face with
    | Front color | Back color | Left color | Right color | Top color | Bottom color -> color

  let face index =
    match index with
    | 0 -> Some(Front)
    | 1 -> Some(Back)
    | 2 -> Some(Left)
    | 3 -> Some(Right)
    | 4 -> Some(Top)
    | 5 -> Some(Bottom)
    | _ -> None

  let oppositeFace face (cube: face list) =
    match face with
    | Front _   -> cube.[1]
    | Back _    -> cube.[0]
    | Left _    -> cube.[3]
    | Right _   -> cube.[2]
    | Top _     -> cube.[5]
    | Bottom _  -> cube.[4]

  // Input
  let input = [ [Front 1; Back 5; Left 10; Right 3; Top 6; Bottom 5];
                [Front 2; Back 6; Left 7; Right 3; Top 6; Bottom 9];
                [Front 5; Back 7; Left 3; Right 2; Top 1; Bottom 9];
                [Front 1; Back 3; Left 3; Right 5; Top 8; Bottom 10];
                [Front 6; Back 6; Left 2; Right 2; Top 4; Bottom 4];
                [Front 1; Back 2; Left 3; Right 4; Top 5; Bottom 6];
                [Front 10; Back 9; Left 8; Right 7; Top 6; Bottom 5];
                [Front 6; Back 1; Left 2; Right 3; Top 4; Bottom 7];
                [Front 1; Back 2; Left 3; Right 3; Top 2; Bottom 1];
                [Front 3; Back 2; Left 1; Right 1; Top 2; Bottom 3] ]

  let rec findMatchingFaces index downFace cubes =
    match cubes with
    | [] -> []
    | head :: tail ->
      let matchingFaces = head |> List.filter (fun x -> color x = color downFace)
      (index, matchingFaces, cubes) :: findMatchingFaces (index + 1) downFace tail

  let rec findTowerForFace index upFace cube cubes =
    match cubes with
    | [] -> [(index, upFace)]
    | head :: tail ->
      let face = (index, upFace)
      let downFace = oppositeFace upFace cube

      let matches = findMatchingFaces (index + 1) downFace cubes
      face :: match matches with
              | [] -> []
              | _ -> let towers = [for m in matches ->
                                    match m with
                                    | (idx, fs, cs) ->
                                      match fs with
                                      | [] -> []
                                      | _ -> [for f in fs -> findTowerForFace idx f (List.head cs) (List.tail cs)]
                                             |> List.maxBy (fun x -> List.length x)
                                  ]
                     match towers with
                     | [] -> []
                     | _ -> towers |> List.maxBy (fun x -> List.length x)

  let rec findTowersForCube index faces cube cubes =
    match faces with
    | [] -> []
    | upFace :: tail -> // each element represents the color of one face of the cube
      let tower = findTowerForFace index upFace cube cubes
      tower :: findTowersForCube index tail cube cubes

  let rec findTowers index cubes =
    match cubes with
    | [] -> []
    | cube :: tail -> // each row represents a cube
      let i = index + 1
      let tower = findTowersForCube i cube cube tail |> List.maxBy (fun x -> List.length x)
      tower :: findTowers i tail

  findTowers 0 input |> List.maxBy (fun x -> List.length x)
                     |> List.iter (fun x -> printfn "%d %A" (fst x) (snd x))