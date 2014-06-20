namespace UVaOnlineJudge

module Problem_10785 =
  // Input
  let n = 50u

  let (|Even|Odd|) (n: uint32) =
    if n % 2u = 0u then
        Even
    else
        Odd

  let generateName length =
    let vowels = [for vowel in ['a'; 'u'; 'e'; 'o'; 'i'] do
                  for i in 1 .. 21 do yield vowel]
    let consonants = [for consonant in ['j'; 's'; 'b'; 'k'; 't'; 'c'; 'l'; 'd'; 'm'; 'v'; 'n'; 'w'; 'f'; 'x'; 'g'; 'p'; 'y'; 'h'; 'q'; 'z'; 'r'] do
                      for i in 1 .. 5 do yield consonant]

    let rec loop (index: uint32) vowels consonants str =
      match index, vowels, consonants with
      | _, _, _ when index = length -> Some (System.String (Array.ofList(List.rev str)))
      | Odd, head :: tail, _ -> loop (index + 1u) tail consonants (head :: str)
      | Even, _, head:: tail -> loop (index + 1u) vowels tail (head :: str)
      | _, _, _ -> None
    loop 1u vowels consonants List.empty

  match generateName n with
  | Some str -> printfn "%s" str
  | None -> printfn "Error."