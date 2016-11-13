// http://en.wikipedia.org/wiki/Nth_root_algorithm
module Problem_113 =
  // Inputs
  let n = 7I
  let p = 4357186184021382204544I

  let root n p =
    let power a b = // Power operator for bigint
      let rec loop accumulator counter =
        if counter <= 1I then
          accumulator
        else
          loop (accumulator * a) (counter - 1I)
      loop a b
    let rec loop x =
      if (power x n) = p then x
      else
        let a = 1.0 / (float n)
        let b = (n - 1I) * x
        let c = (float p) / (float (power x (n - 1I)))
        loop (bigint (round (a * ((float b) + c))))
    loop n // Initial guess

  printfn "%A" (root n p)
