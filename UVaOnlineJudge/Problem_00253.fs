namespace UVaOnlineJudge

module Problem_253 =
  // Input
  let input = "rbgggrrggbgr"

  let rotations = [[1; 2; 3; 4; 5; 6];
                   [1; 3; 5; 2; 4; 6];
                   [1; 5; 4; 3; 2; 6];
                   [1; 4; 2; 5; 3; 6];
                   [5; 1; 3; 4; 6; 2];
                   [6; 5; 3; 4; 2; 1];
                   [2; 6; 3; 4; 1; 5];
                   [4; 2; 1; 6; 5; 3];
                   [6; 2; 4; 3; 5; 6];
                   [3; 2; 6; 1; 5; 4]]

  let paintedCube = input.[0..5]
  let anotherCube = input.[6..11]

  let rec rotate (rotation: int list) (paintedCube: string) =
    match rotation with
    | [] -> []
    | head :: tail -> paintedCube.[head - 1] :: rotate tail paintedCube

  let rec equalPainted rotations paintedCube anotherCube =
    match rotations with
    | [] -> false
    | head :: tail ->
      let rotatedCube = System.String.Concat(Array.ofList(rotate head paintedCube))
      match rotatedCube with
      | rotatedCube when rotatedCube = anotherCube -> true
      | _ -> equalPainted tail paintedCube anotherCube

  let equal = equalPainted rotations paintedCube anotherCube
  let result = match equal with
               | false -> "FALSE"
               | true -> "TRUE"
  printfn "%s" result
