namespace UVaOnlineJudge

module Problem_1619 =
  // Input
  let dayValues = [|3; 1; 6; 4; 5; 2|]

  type EmotionalPeriod (value, l, r) = class
    member this.value = value
    member this.l = l
    member this.r = r
  end

  let calculateEmotionalPeriods (dayValues: int []) =
    [for i in 0 .. dayValues.Length - 1 do
      for j in 2 .. dayValues.Length - i do
        let subArray = Array.sub dayValues i j
        yield new EmotionalPeriod ((Array.sum subArray) * (Array.min subArray), (i + 1), (i + j))]

  calculateEmotionalPeriods dayValues |> List.maxBy (fun x -> x.value)
                                      |> (fun x -> printfn "%d\n%d %d" x.value x.l x.r)
