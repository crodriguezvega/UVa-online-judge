module Problem_1640 =
  // Inputs
  let a = 503
  let b = 1004

  let countDigits a b =
    let rec count number (counters: int array) =
      match number with
      | 0 -> counters
      | _ ->
        counters.[number % 10] <- counters.[number % 10] + 1
        count (number / 10) counters
    let rec loop numbers (counters: int array) =
      match numbers with
      | [] -> counters
      | head :: tail -> loop tail (count head counters)
    loop [a .. b] (Array.zeroCreate 10)

  (countDigits a b) |> Array.iter (fun x -> printf "%d " x)