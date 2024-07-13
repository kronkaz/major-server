let option_sequence l =
  let rec go xs = function
    | [] -> Some (List.rev xs)
    | opt :: opts ->
      match opt with
      | None -> None
      | Some x -> go (x :: xs) opts
  in
  go [] l

let option_liftM2 f a_opt b_opt = let (let*) = Option.bind in
  let* a = a_opt in
  let* b = b_opt in
  Some (f a b)

let array_find_indices p xs =
  Array.fold_left
    (fun (i, indices) x -> if p x then i + 1, i :: indices else i + 1, indices)
    (0, []) xs
  |> snd
  |> List.rev
