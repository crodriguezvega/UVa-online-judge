module Problem_843 =
  // Inputs
  let dictionary = [ "and"; "dick"; "jane"; "puff"; "spot"; "yertle" ]
  let encrypted = "bjvg xsb hxsn xsb qymm xsb rqat xsb pnetfn"

  let dictionaryByWordLength = dictionary |> Seq.sortBy (fun x -> -x.Length)
                                          |> Seq.groupBy (fun x -> x.Length)
  let encryptedByWordLength = encrypted.Split(' ') |> Array.toList
                                                   |> Seq.distinct
                                                   |> Seq.sortBy (fun x -> -x.Length)
                                                   |> Seq.groupBy (fun x -> x.Length)

  // BEGIN: http://stackoverflow.com/questions/4495597/combinations-and-permutations-in-f
  // All ordered picks {x_i1, x_i2, .. , x_ik} of k out of n elements {x_1,..,x_n}
  // where i1 < i2 < .. < ik
  let picks n L = 
    let rec aux nleft acc L = seq {
      match nleft,L with
      | 0,_ -> yield acc
      | _,[] -> ()
      | nleft,h::t -> yield! aux (nleft-1) (h::acc) t
                      yield! aux nleft acc t }
    aux n [] L

  // Distribute an element y over a list:
  // {x1,..,xn} --> {y,x1,..,xn}, {x1,y,x2,..,xn}, .. , {x1,..,xn,y}
  let distrib y L =
    let rec aux pre post = seq {
      match post with
      | [] -> yield (L @ [y])
      | h::t -> yield (pre @ y::post)
                yield! aux (pre @ [h]) t }
    aux [] L

  // All permutations of a single list = the head of a list distributed
  // over all permutations of its tail
  let rec getAllPerms = function
    | [] -> Seq.singleton []
    | h::t -> getAllPerms t |> Seq.collect (distrib h)
  // END: http://stackoverflow.com/questions/4495597/combinations-and-permutations-in-f

  let rec factorial n =
    match n with
    | 0 | 1 -> 1
    | _ -> n * factorial(n-1)

  let rec joinMaps (maps: Map<char, char> list) map =
    match maps with
    | [] -> map
    | head :: tail ->
      joinMaps tail (Map(Seq.concat [(Map.toSeq head); (Map.toSeq map)]))

  let rec matchWord dictionaryLetters encryptedLetters maps runningMap =
    match dictionaryLetters, encryptedLetters with
    | [], [] -> Some runningMap
    | dictionaryHeadLetter :: dictionaryTailLetters, encryptedHeadLetter :: encryptedTailLetters ->
      let currentMap = joinMaps (runningMap :: maps) Map.empty
      if currentMap.ContainsKey encryptedHeadLetter
         && currentMap.[encryptedHeadLetter] <> dictionaryHeadLetter then None
      else
        matchWord dictionaryTailLetters encryptedTailLetters maps (runningMap.Add(encryptedHeadLetter, dictionaryHeadLetter))
    | _, _ -> None

  let rec matchGroup dictionaryWords encryptedWords maps =
    match dictionaryWords, encryptedWords with
    | _, [] -> Some (joinMaps maps Map.empty)
    | dictionaryHeadWord :: dictionaryTailWords, encryptedHeadWord :: encryptedTailWords ->
      let result = matchWord (List.ofSeq dictionaryHeadWord) (List.ofSeq encryptedHeadWord) maps Map.empty
      match result with
      | Some map ->
        matchGroup dictionaryTailWords encryptedTailWords (map :: maps)
      | None -> None
    | _, _ -> None

  let rec decrypt dictionaryByWordLength encryptedByWordLength history decryptionMaps =
    match history with
    | history when List.length history > Seq.length encryptedByWordLength ->
      Some decryptionMaps
    | _ ->
      let permutationNr = List.head history
      let level = List.length history - 1
      let encryptedGroup = Seq.nth level encryptedByWordLength
      let encryptedWords = snd encryptedGroup
      let wordLength = fst encryptedGroup
      let nrOfEncryptedWords = Seq.length encryptedWords
      let dictionaryGroup = dictionaryByWordLength |> Seq.tryFind (fun x -> (fst x) = wordLength)
      match dictionaryGroup with
      | Some dictionaryGroup ->
        let dictionaryWords = snd dictionaryGroup
        let nrOfDictionaryWords = Seq.length dictionaryWords

        if permutationNr >= factorial nrOfDictionaryWords then
          let historyTail = List.tail history
          if List.isEmpty historyTail then None
          else
            decrypt dictionaryByWordLength
                    encryptedByWordLength
                    ((List.head historyTail + 1) :: (List.tail historyTail))
                    (List.tail decryptionMaps)
        else
          if nrOfEncryptedWords > nrOfDictionaryWords then None
          else
            let permutedDictionaryWords = picks nrOfDictionaryWords (List.ofSeq dictionaryWords) |> Seq.collect getAllPerms
                                                                                                 |> Seq.nth permutationNr
            let result = matchGroup permutedDictionaryWords (List.ofSeq encryptedWords) decryptionMaps
            match result with 
            | Some maps ->
              decrypt dictionaryByWordLength encryptedByWordLength (0 :: history) (maps :: decryptionMaps)
            | None ->
              if List.isEmpty decryptionMaps then None
              else
                decrypt dictionaryByWordLength encryptedByWordLength ((permutationNr + 1) :: (List.tail history)) decryptionMaps
      | None -> None

  match decrypt dictionaryByWordLength encryptedByWordLength (0 :: List.empty) (List.empty) with
  | Some decryptionMaps ->
    let decryptionMap = joinMaps decryptionMaps Map.empty
    encrypted |> Seq.iter (fun x -> if x = ' ' then printf " "
                                    else printf "%c" decryptionMap.[x])
  | None ->
    encrypted |> Seq.iter (fun x -> if x = ' ' then printf " "
                                    else printf "*")