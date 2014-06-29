namespace UVaOnlineJudge

module Problem_100 =
  // Inputs
  let i = 900
  let j = 1000

  let cycleLength input =
    let rec loop n counter =
      if n = 1 then counter
      elif n % 2 = 0 then loop (n / 2) (counter + 1)
      else loop (3 * n + 1) (counter + 1)
    loop input 1
  let maximumCycleLength x y =
    let mutable maxLength = 0
    for i = x to y do
      let length = cycleLength i
      if length > maxLength then maxLength <- length
    maxLength

  printfn "%d %d %d" i j (maximumCycleLength i j)
