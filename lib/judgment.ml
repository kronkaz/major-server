let sum = List.fold_left (+) 0

let majority_rating_of_counts candidate_counts ~total_votes =
  let half_of_total_votes = total_votes / 2 in
  let rec find_majority_rating acc_count rating =
    let new_acc_count = acc_count + candidate_counts.(rating) in
    if new_acc_count > half_of_total_votes then rating
    else find_majority_rating new_acc_count (rating + 1)
  in
  find_majority_rating 0 0

module Explanation = struct
  type t =
    | Unique of int * int
    | Tie_break of int * unit
  
  let to_string = function
    | Unique (winner, majority_rating) -> Printf.sprintf "Unique(%d,%d)" winner majority_rating
    | _ -> failwith ""
end

let break_tie results ~rank ~explanation =
  failwith ""

let decide_winner results =
  let best_majority_rating = Array.fold_left (fun best_mr (mr, _) -> max best_mr mr) 0 results in
  match Utils.array_find_indices (fun (mr, _) -> mr = best_majority_rating) results with
  | [winner] -> winner, Explanation.Unique (winner, best_majority_rating)
  | winners ->
    break_tie (List.map (fun i -> results.(i)) winners) ~rank:0
      ~explanation:(Explanation.Tie_break (best_majority_rating, ()))

(* compute a / b rounded to 0.01 *)
let ratio100 a b = let open Decimal in
  of_int a / of_int b
  |> round ~n:2
  |> to_float

let compute_results ~vote_counts ~nb_voters =
  let total_votes = Array.fold_left (+) 0 vote_counts.(0) in
  let results = vote_counts |> Array.map @@ fun candidate_counts ->
    let majority_rating = majority_rating_of_counts candidate_counts ~total_votes in
    let scores =
      Array.map (fun rating_count -> ratio100 rating_count total_votes) candidate_counts
    in
    majority_rating, scores
  in
  let participation = ratio100 total_votes nb_voters in
  let winner, explanation = decide_winner results in
  winner, explanation, participation, results
