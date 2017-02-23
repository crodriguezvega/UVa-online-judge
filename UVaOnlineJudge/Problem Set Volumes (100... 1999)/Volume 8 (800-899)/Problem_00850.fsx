module Problem_850 =
  // Inputs
  let plainText = "the quick brown fox jumps over the lazy dog"
  let encrypted = [ "vtz ud xnm xugm itr pyy jttk gmv xt otgm xt xnm puk ti xnm fprxq";
                    "xnm ceuob lrtzv ita hegfd tsmr xnm ypwq ktj";
                    "frtjrpgguvj otvxmdxd prm iev prmvx xnmq" ]

  let rec joinMaps (maps: Map<char, char> list) map =
    match maps with
    | [] -> map
    | head :: tail ->
      joinMaps tail (Map(Seq.concat [(Map.toSeq head); (Map.toSeq map)]))

  let rec matchWord plainLetters encryptedLetters maps runningMap =
    match plainLetters, encryptedLetters with
    | [], [] -> Some runningMap
    | plainHeadLetter :: plainTailLetters, encryptedHeadLetter :: encryptedTailLetters ->
      let currentMap = joinMaps (runningMap :: maps) Map.empty
      if currentMap.ContainsKey encryptedHeadLetter
         && currentMap.[encryptedHeadLetter] <> plainHeadLetter then None
      else
        matchWord plainTailLetters encryptedTailLetters maps (runningMap.Add(encryptedHeadLetter, plainHeadLetter))
    | _, _ -> None

  let rec matchWords plainWords encryptedWords maps =
    match plainWords, encryptedWords with
    | [], [] -> Some (joinMaps maps Map.empty)
    | plainHeadWord :: plainTailWords, encryptedHeadWord :: encryptedTailWords ->
      if (String.length plainHeadWord) <> (String.length encryptedHeadWord) then
        None
      else
        let result = matchWord (List.ofSeq plainHeadWord) (List.ofSeq encryptedHeadWord) maps Map.empty
        match result with
        | Some map ->
          matchWords plainTailWords encryptedTailWords (map :: maps)
        | None -> None
    | _, _ -> None

  let rec findDictionary (plainText: string) (encrypted: string list) =
    match encrypted with
    | [] -> None
    | head :: tail ->
      let encryptedWords = head.Split(' ') |> Array.toList
      let plainWords = plainText.Split(' ') |> Array.toList
      if encryptedWords.Length <> plainWords.Length then
        findDictionary plainText tail
      else
        let result = matchWords plainWords encryptedWords List.empty
        match result with
        | Some maps -> Some maps
        | None -> findDictionary plainText tail

  let rec decrypt (dictionary: Map<char, char>) encrypted =
    match encrypted with
    | [] -> []
    | head :: tail ->
      let decryptedChars = [ for letter in head do
                               if letter = ' ' then yield letter
                               else yield dictionary.[letter] ]
      System.String(Array.ofList decryptedChars) :: (decrypt dictionary tail)

  findDictionary plainText encrypted |> fun x -> match x with
                                                 | None -> printfn "No solution."
                                                 | Some dictionary ->
                                                   decrypt dictionary encrypted |> List.iter (fun x -> printfn "%s" x)