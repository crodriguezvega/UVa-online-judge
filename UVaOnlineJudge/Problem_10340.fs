namespace UVaOnlineJudge

module Problem_10340 =
  // Input
  let s = "VERDI"
  let t = "vivaVittorioEmanueleReDiIatlia"

  let rec isSubsequence s t =
    match s with
    | [] -> Some true
    | head :: tail -> 
      match List.tryFindIndex (fun x -> x = head) t with
      | Some value ->
        let arr = Array.ofList t
        isSubsequence tail [for i in (value + 1) .. (arr.Length - 1) do yield arr.[i]]
      | None -> None

  match isSubsequence (Array.toList (s.ToCharArray())) (Array.toList (t.ToCharArray())) with
  | Some value -> printfn "Yes"
  | None -> printfn "No"
