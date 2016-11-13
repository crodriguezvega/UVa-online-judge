namespace UVaOnlineJudge

module Problem_350 =
  // Inputs
  let Z = 1079
  let I = 2136
  let M =  9999
  let seed = 1237

  let nextRndNumber L = (Z * L + I) % M
  let lengthPseudoRandomSequence seed =
    let rec loop L sequence =
      match List.tryFindIndex (fun x -> x = L) sequence with
      | Some position -> position + 1
      | None -> loop (nextRndNumber L) (L :: sequence)
    loop seed List.empty

  printfn "%d" (lengthPseudoRandomSequence seed)
