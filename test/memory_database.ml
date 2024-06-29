type db = {
  voter_data : (int, string) Hashtbl.t
}

let name_of_voter db voter_id = Hashtbl.find_opt db.voter_data voter_id

let create () =
  let voter_data = Hashtbl.of_seq @@ List.to_seq
    [(0, "Jean Dupont"); (1, "John Smith"); (2, "Giuseppe Ragazzo")]
  in
  { voter_data }
