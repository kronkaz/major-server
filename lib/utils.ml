let option_traverse f l =
  let rec traverse ys = function
    | [] -> Some (List.rev ys)
    | x :: xs ->
      match f x with
      | None -> None
      | Some y -> traverse (y :: ys) xs
  in
  traverse [] l

let array_find_indices p xs =
  Array.fold_left
    (fun (i, indices) x -> if p x then i + 1, i :: indices else i + 1, indices)
    (0, []) xs
  |> snd
  |> List.rev