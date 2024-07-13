open Domain

(* strictly more than half of voters consider a candidate deserves at least their majority rating *)
let majority_rating ~candidate_votes =
  let total_votes = Array.fold_left (+) 0 candidate_votes in
  if total_votes = 0 then None
  else
    let rec go count rating =
      let i = Rating.to_int rating in
      let count' = count + candidate_votes.(i) in
      if count' > total_votes / 2 then rating
      else go count' (Option.get @@ Rating.of_int_opt (i - 1))
      (* we checked there is at least one vote,
         so we always cross the median vote at some point,
         so Option.get is safe here *)
    in
    Some (go 0 (Option.get @@ Rating.of_int_opt 6))

(* finite sequence of repeated majority rating computations,
   where we remove the median vote every time until no votes are left *)
let majority_sequence ~candidate_votes =
  let candidate_votes = Array.copy candidate_votes in
  let majority_rating_removing_vote rating =
    let i = Rating.to_int rating in
    candidate_votes.(i) <- candidate_votes.(i) - 1;
    Option.bind (majority_rating ~candidate_votes) @@ fun r -> Some (r, Some r)
  in
  majority_rating ~candidate_votes
  |> Seq.unfold @@ fun rating -> Option.bind rating majority_rating_removing_vote

let majority_judgment ~votes =
  let rec find_winners indexed_majority_sequences =
    if Seq.is_empty @@ snd (List.hd indexed_majority_sequences) then begin
      (* had to reach the last vote and could not break the tie -> several winners *)
      List.map fst indexed_majority_sequences
    end else
      (* expose the next majority rating in the sequences *)
      let majority_ratings_and_sequences =
        indexed_majority_sequences
        |> List.map (fun (candidate, majority_seq) ->
            let (majority_rating, majority_seq') = Option.get @@ Seq.uncons majority_seq in
            (candidate, majority_rating, majority_seq')
           )
      in
      (* find the best one and remove all sequences who do not have it *)
      let best_majority_rating =
        majority_ratings_and_sequences
        |> List.sort (fun (_, rating, _) (_, rating', _) -> Rating.compare rating' rating)
        |> List.hd
        |> fun (_, rating, _) -> rating
      in
      let majority_sequences' =
        majority_ratings_and_sequences
        |> List.filter_map @@ fun (candidate, rating, majority_seq) ->
             if rating = best_majority_rating then Some (candidate, majority_seq) else None
      in
      (* loop with the remaining sequences *)
      find_winners majority_sequences'
  in
  let majority_ratings =
    CandidateMap.map (fun candidate_votes -> majority_rating ~candidate_votes) votes
  in
  let winners =
    votes
    |> CandidateMap.map (fun candidate_votes -> majority_sequence ~candidate_votes)
    |> CandidateMap.to_list
    |> find_winners
  in
  majority_ratings, winners
