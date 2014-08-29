namespace UVaOnlineJudge

module Problem_10026 =
  type Job (index: uint16, duration: uint16, fine: uint16) = class
    member this.Index = index
    member this.Duration = duration
    member this.Fine = fine
    member this.FineDurationRatio = (float this.Fine) / (float this.Duration)
  end

  let sortFunction (left: Job) (right: Job) =
    if (left.FineDurationRatio > right.FineDurationRatio) then -1
    else if (left.FineDurationRatio < right.FineDurationRatio) then 1
    else
      if (left.Index < right.Index) then -1
      else if (left.Index > right.Index) then 1
      else 0

  [new Job (1us, 3us, 2us);
   new Job (2us, 3us, 5us);
   new Job (3us, 9999us, 9998us);
   new Job (4us, 2us, 3us);
   new Job (5us, 10000us, 10000us)] |> List.sortWith sortFunction
                                    |> List.iter (fun x -> printf "%d " x.Index)