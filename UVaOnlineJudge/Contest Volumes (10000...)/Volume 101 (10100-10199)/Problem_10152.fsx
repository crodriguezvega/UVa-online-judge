module Problem_10152 =
  // Inputs
  let originalOrdering = [
    "Yertle";
    "Duke of Earl";
    "Sir Lancelot";
    "Elizabeth Windsor";
    "Michael Eisner";
    "Richard M. Nixon";
    "Mr. Rogers";
    "Ford Perfect";
    "Mack"
  ]
  let desiredOrdering = [
    "Yertle";
    "Richard M. Nixon";
    "Sir Lancelot";
    "Duke of Earl";
    "Elizabeth Windsor";
    "Michael Eisner";
    "Mr. Rogers";
    "Ford Perfect";
    "Mack"
  ]

  let findTurtles original desired =
    let correction = List.length original - 1

    let rec loop index original sequence =
      match original with
      | [] -> List.rev sequence
      | head :: tail ->
        let distance = (List.findIndex (fun x -> x = head) desired) - (correction - index)
        if distance = (List.length sequence) then
          loop (index + 1) tail sequence
        else
          loop (index + 1) tail (head :: sequence)

    loop 0 (List.rev original) List.empty

  let sort (turtle1: string) (turtle2: string) =
    let indexTurtle1 = List.findIndex (fun x -> x = turtle1) desiredOrdering
    let indexTurtle2 = List.findIndex (fun x -> x = turtle2) desiredOrdering

    if indexTurtle1 < indexTurtle2 then 1
    else if indexTurtle1 > indexTurtle2 then -1
    else 0

  let turtles = findTurtles originalOrdering desiredOrdering
  let sequence = List.sortWith sort turtles

  List.iter (fun x -> printfn "%s" x) sequence