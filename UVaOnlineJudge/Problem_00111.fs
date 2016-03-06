namespace UVaOnlineJudge

// Longest common subsequence algorithm (https://en.wikipedia.org/wiki/Longest_common_subsequence_problem)
module Problem_00111 =
  // Input
  let order = [3; 1; 2; 4; 9; 5; 10; 6; 8; 7]
  let rank = [1; 2; 3; 4; 5; 6; 7; 8; 9; 10]

  let lcs order rank =
    let lengthOrder = List.length order
    let lengthRank = List.length rank
    let lengths = Array2D.zeroCreate<int> (lengthOrder + 1) (lengthRank + 1)
    for i in 1..lengthOrder do
      for j in 1..lengthRank do
        if order.[i - 1] = rank.[j - 1] then lengths.[i, j] <- lengths.[i - 1, j - 1] + 1
        else lengths.[i, j] <- max lengths.[i - 1, j] lengths.[i, j - 1]
    lengths.[lengthOrder, lengthRank]

  printfn "%d" (lcs order rank)
