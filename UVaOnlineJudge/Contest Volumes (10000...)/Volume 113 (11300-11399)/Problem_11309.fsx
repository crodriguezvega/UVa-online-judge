module Problem_11309 =
  // Input
  let time = "14:59"

  let (|Int|_|) str =
    match System.Int32.TryParse(str) with
    | (true, int) -> Some(int)
    | _ -> None
  
  let parse str =
    match str with
    | Int i -> i
    | _ -> failwith "Cannot parse string to int"

  let reverse (str: string) = System.String (Array.rev (str.ToCharArray()))

  let increment n =
    let hh = n / 100
    let mm = n % 100
    match hh, mm with
    | 23, 59 -> 0
    | _, 59 -> (hh + 1) * 100
    | _, _ -> (hh * 100) + (mm + 1)

  let isPalindromic n =
    let str = string n
    str = reverse str

  let nextPalindromicTime (time: string) =
    let rec loop n =
      match n with
      | n when isPalindromic n -> (string n).PadLeft(4, '0').Insert(2, ":")
      | _ -> loop (increment n)
    let current = parse (time.Remove(2, 1))
    loop (increment current)

  printfn "%s" (nextPalindromicTime time)