module Problem_10142 =
  // Inputs
  let candidates = Map.empty
                      .Add(1, "John Doe")
                      .Add(2, "Jane Smith")
                      .Add(3, "Sirhan Sirhan")
  let votes = [| [| 1; 2; 3 |];
                 [| 2; 1; 3 |];
                 [| 2; 3; 1 |];
                 [| 1; 2; 3 |];
                 [| 3; 1; 2 |]; |]

  type pairCandidateVoter = { candidate: int; voter: int }
  type pairCandidateNrOfVotes = { candidate: int; nrVotes: int }

  let select (cv: int[][]) voters round = seq { for voter in voters do
                                                  yield { candidate = votes.[voter].[round];
                                                          voter = voter } }
  let count (cv: seq<pairCandidateVoter>) = cv |> Seq.groupBy (fun x -> x.candidate)
                                               |> Seq.map (fun x -> { candidate = fst x;
                                                                      nrVotes = (snd x) |> Seq.map (fun y -> y.voter)
                                                                                        |> Seq.length })
  let filter (cv: seq<pairCandidateNrOfVotes>) value = cv |> Seq.filter (fun x -> x.nrVotes = value)
                                                          |> Seq.map (fun x -> x.candidate)

  let election (votes: int[][]) =
    let nrOfBallots = votes.GetLength 0

    let rec loop voters round results winners =
      match round, winners with
      | r, w when r < 2 && Seq.isEmpty w ->
        let res1 = Seq.append results (select votes voters round)
        let res2 = count res1

        let max = Seq.maxBy (fun x -> x.nrVotes) res2
        let min = Seq.minBy (fun x -> x.nrVotes) res2

        if max.nrVotes > (nrOfBallots/ 2) || max.nrVotes = min .nrVotes then
          let winnerCandidates = filter res2 max.nrVotes
          loop voters (round + 1) res1 winnerCandidates
        else
          let eliminatedCandidates = filter res2 min.nrVotes
          let votersOfEliminatedCandidates = res1 |> Seq.filter (fun x -> Seq.exists (fun y -> y = x.candidate) eliminatedCandidates)
                                                  |> Seq.map (fun x -> x.voter)
          let remainingCandidates = res1 |> Seq.filter (fun x -> not (Seq.exists (fun y -> y = x.candidate) eliminatedCandidates))
          loop votersOfEliminatedCandidates (round + 1) remainingCandidates Seq.empty
      | _, _ -> winners
    loop [ 0 .. nrOfBallots - 1 ] 0 Seq.empty Seq.empty

  let winners = election votes
  Seq.iter (fun x -> printfn "%s" candidates.[x]) winners