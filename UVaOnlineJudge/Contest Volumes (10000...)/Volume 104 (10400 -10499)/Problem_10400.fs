namespace UVaOnlineJudge

module Problem_10400 =
  // Input
  let numbers = [12; 2; 5; 1; 2] 
  let target = 4

  type operation =
    | Sum
    | Subtraction
    | Multiplication
    | Division

  let toOperator operation =
    match operation with
    | Sum -> (+)
    | Subtraction -> (-)
    | Multiplication -> (*)
    | Division -> (/)

  let toString operation =
      match operation with
      | Sum -> "+"
      | Subtraction -> "-"
      | Multiplication -> "*"
      | Division -> "/"

  let nextOperations current =
    let increment operation =
      match operation with
      | Sum -> Subtraction
      | Subtraction -> Multiplication
      | Multiplication -> Division
      | Division -> Sum

    match List.tryFindIndex (fun x -> x < Division) current with
    | None -> None
    | Some(index) ->
      let first, second = List.splitAt (index + 1) current
      let next = first |> List.map (fun op -> increment op)
      Some(List.append next second)

  let isOperationValid value =
    if (value % 1.0 = 0.0) && (abs value) <= 32000.0 then true
    else false

  let rec applyOperators operators operands =
    match operators, operands with
    | operation :: tailOperators, operand1 :: operand2 :: tailOperands ->
      let result = operation (float operand1) (float operand2)
      if isOperationValid result then applyOperators tailOperators (int result :: tailOperands)
      else None
    | _, result :: tail -> Some(result)
    | _, _ -> None

  let rec findExpression currentOperations numbers target =
    let operators = List.map toOperator currentOperations
    let result = applyOperators operators numbers
    match result with
    | None ->
      let nextOperations = nextOperations currentOperations
      match nextOperations with
      | None -> None
      | Some(operations) -> findExpression operations numbers target
    | Some(value) ->
      if value = target then Some(currentOperations)
      else
        let nextOperations = nextOperations currentOperations
        match nextOperations with
        | None -> None
        | Some(operations) -> findExpression operations numbers target

  let rec formatExpression operations numbers target =
    match operations, numbers with
    | [] , [] -> [sprintf "=%d" target]
    | [], number :: tailNumbers -> sprintf "%d" number :: formatExpression [] tailNumbers target
    | operation :: tailOperations, number :: tailNumbers ->
      sprintf "%d%s" number operation :: formatExpression tailOperations tailNumbers target
    | _, _ -> []

  let length = List.length numbers
  let initialOperations = List.init (length - 1) (fun _ -> Sum)
  let expression = findExpression initialOperations numbers target
  match expression with
  | None -> printfn "NO EXPRESSION"
  | Some(operations) ->
    let operationStrings = List.map toString operations
    let expressionString = List.fold (+) "" (formatExpression operationStrings numbers target)
    printfn "%s" expressionString