module Problem_10010 =
  // Inputs
  let grid = [| [| 'a'; 'b'; 'c'; 'D'; 'E'; 'F'; 'G'; 'h'; 'i'; 'g'; 'g' |]
                [| 'h'; 'E'; 'b'; 'k'; 'W'; 'a'; 'l'; 'D'; 'o'; 'r'; 'k' |]
                [| 'F'; 't'; 'y'; 'A'; 'w'; 'a'; 'l'; 'd'; 'O'; 'R'; 'm' |]
                [| 'F'; 't'; 's'; 'i'; 'm'; 'r'; 'L'; 'q'; 's'; 'r'; 'c' |]
                [| 'b'; 'y'; 'o'; 'A'; 'r'; 'B'; 'e'; 'D'; 'e'; 'y'; 'v' |]
                [| 'K'; 'l'; 'c'; 'b'; 'q'; 'w'; 'i'; 'k'; 'o'; 'm'; 'k' |]
                [| 's'; 't'; 'r'; 'E'; 'B'; 'G'; 'a'; 'd'; 'h'; 'r'; 'b' |]
                [| 'y'; 'U'; 'i'; 'q'; 'l'; 'x'; 'c'; 'n'; 'B'; 'j'; 'f' |] |]
  let word = "Waldorf"

  let directions = [ (-1, 0); (-1, 1); (0, 1); (1, 1); (1, 0); (1, -1); (0, -1); (-1, -1) ]

  let compare letter (grid: char[][]) (position: int * int) =
    let height = grid.Length
    let width = grid.[0].Length
    let row = fst position
    let column = snd position
    if (row >= 0 && row < height && column >= 0 && column < width) then
      System.Char.ToLower(letter) = System.Char.ToLower(grid.[row].[column])
    else false

  let findStartPositions (word: string) (grid: char[][]) =
    let firstLetter = word.[0]
    [ for row in 0 .. grid.Length - 1 do
        for column in 0 .. grid.[row].Length - 1 do
          if (compare firstLetter grid (row, column)) then yield (row, column) ]

  let rec isMatchInDirection word grid startPosition direction =
    match word with
    | [] -> true
    | head :: tail ->
      let position = (fst startPosition + fst direction, snd startPosition + snd direction)
      if compare head grid position then isMatchInDirection tail grid position direction
      else false

  let rec isMatchInSomeDirection word grid startPosition directions =
    match directions with
    | [] -> false
    | head :: tail ->
      if isMatchInDirection word grid startPosition head then true
      else isMatchInSomeDirection word grid startPosition tail

  let rec findMatches word grid startPositions directions =
    let rec loop startPositions matchingStartPositions = 
      match startPositions with
      | [] -> matchingStartPositions
      | head :: tail ->
        if isMatchInSomeDirection word grid head directions then loop tail (head :: matchingStartPositions)
        else loop tail matchingStartPositions
    loop startPositions []

  let startPositions = findStartPositions word grid
  let restOfWordToMatch = (Array.toList (word.ToCharArray())).Tail
  let matchingStartPositions = findMatches restOfWordToMatch grid startPositions directions

  let result = matchingStartPositions |> fun x -> match x with
                                                  | [] -> "No match"
                                                  | _ -> x |> List.sortBy (fun x -> fst x)
                                                           |> List.sortBy (fun x -> snd x)
                                                           |> List.head
                                                           |> fun x -> sprintf "%d %d" (fst x + 1) (snd x + 1)

  printfn "%s" result