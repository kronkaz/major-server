open Domain

(* ┌── Warning ─────────────────────────────────────────────────────────────────────────────────┐ *)
(* │ Functions in this module expect arrays to list rating counts in increasing order, i.e.,    │ *)
(* │ from the worst rating to the best rating. We also assume there are exactly 7 ratings.      │ *)
(* └────────────────────────────────────────────────────────────────────────────────────────────┘ *)

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
  (majority_rating ~candidate_votes, candidate_votes)
  |> Seq.unfold @@ fun (rating_opt, candidate_votes) -> let (let*) = Option.bind in
       let* rating = rating_opt in
       let i = Rating.to_int rating in
       (* the array is only used as an internal state for computing the ratings, but we still copy
          it at every step to avoid weird bugs due to the interaction between mutability and the
          laziness of sequences (on purpose, because we don't want to compute the whole sequence
          if it is not needed)
          anyway, the arrays are 7-cells long, and the more voters, the less perfect equalities,
          so it is not a big deal *)
       let candidate_votes = Array.copy candidate_votes in
       candidate_votes.(i) <- candidate_votes.(i) - 1;
       Some (rating, (majority_rating ~candidate_votes, candidate_votes))

let majority_judgment ~votes =
  let rec find_winners indexed_majority_sequences =
    if List.length indexed_majority_sequences = 1 ||
       Seq.is_empty (snd @@ List.hd indexed_majority_sequences)
    then
      (* only one winner left, or had to reach the last vote and could not break the tie *)
      List.map fst indexed_majority_sequences
    else begin
      (* expose the next majority rating in the sequences *)
      let majority_ratings_and_sequences =
        indexed_majority_sequences
        |> List.map (fun (candidate, majority_seq) ->
             let (majority_rating, majority_seq') = Option.get @@ Seq.uncons majority_seq in
             (candidate, majority_rating, majority_seq'))
      in
      (* find the best one and remove all sequences who do not have it *)
      let best_majority_rating =
        majority_ratings_and_sequences
        |> List.fold_left
             (fun best_rating (_, rating, _) -> Rating.max best_rating rating)
             (Option.get @@ Rating.of_int_opt 0)
      in
      let majority_sequences' =
        majority_ratings_and_sequences
        |> List.filter_map @@ fun (candidate, rating, majority_seq) ->
             if rating = best_majority_rating then Some (candidate, majority_seq) else None
      in
      (* loop with the remaining sequences *)
      find_winners majority_sequences'
    end
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
