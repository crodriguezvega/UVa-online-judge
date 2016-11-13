module Problem_10763 =
  // Input
  let exchange = [(1, 2);
                  (2, 1);
                  (3, 4);
                  (4, 3);
                  (100, 200);
                  (200, 100);
                  (57, 2);
                  (2, 57);
                  (1, 2);
                  (2, 1)]

  let isProgramValid exchange =
    let rec loop exchange origin destination =
      match exchange with
      | [] -> (origin, destination)
      | (o, d) :: tail -> loop tail (o :: origin) (d :: destination) 
    let origin, destination = loop exchange List.empty List.empty
    if (origin |> List.sort) = (destination |> List.sort) then "YES"
    else "NO"

  let result = isProgramValid exchange
  printfn "%s" result