module Json = Yojson.Basic

module type S = sig
  type db
  
  val create : unit -> db
  val name_of_voter : db -> int -> string option
  val election_summaries_of_voter : db -> int -> Json.t
  val election_exists : db -> int -> bool
  val can_vote : db -> voter_id:int -> election_id:int -> bool
  val get_election_by_id : db -> int -> Json.t
end

module Default : S = struct
  type candidate = {
    candidate_name : string;
    party_name : string;
    colour : string
  }

  type election = {
    id : int;
    election_name : string;
    is_running : bool;
    candidates : candidate list;
    voters : int list
  }

  let election_to_string election ~detailed = Json.to_string @@ `Assoc (List.concat [
    if detailed then
      []
    else [
      ("id", `Int election.id)
    ]; [
      ("name", `String election.election_name);
      ("is_running", `Bool election.is_running)
    ];
    if detailed then [
      ("candidates", `List (election.candidates |> List.map @@ fun candidate -> `Assoc [
        ("name", `String candidate.candidate_name);
        ("party", `String candidate.party_name);
        ("colour", `String candidate.colour);
      ]))
    ] else
      []
  ])

  type db = {
    voter_data : (int, string) Hashtbl.t;
    election_data : election list
  }

  let name_of_voter db voter_id = Hashtbl.find_opt db.voter_data voter_id

  let election_summaries_of_voter db voter_id =
    db.election_data
    |> List.filter_map (fun election ->
         if not (List.mem voter_id election.voters) then None
         else Some (`String (election_to_string ~detailed:false election)))
    |> fun l -> `List l

  let election_exists db election_id =
    List.exists (fun election -> election.id = election_id) db.election_data

  let can_vote db ~voter_id ~election_id =
    let election = List.find (fun election -> election.id = election_id) db.election_data in
    List.mem voter_id election.voters

  let get_election_by_id db election_id =
    List.find (fun election -> election.id = election_id) db.election_data
    |> election_to_string ~detailed:true
    |> fun s -> `String s

  let create () =
    let voter_data = Hashtbl.of_seq @@ List.to_seq
      [(0, "Jean Dupont"); (1, "John Smith"); (2, "Giuseppe Ragazzo")]
    in
    let election_data = [
      { id = 1;
        election_name = "Election 1";
        is_running = true;
        candidates = [
          { candidate_name = "Bob"; party_name = "The Blue"; colour = "#0000FF" };
          { candidate_name = "Ronald"; party_name = "The Red"; colour = "#FF0000" };
          { candidate_name = "Gabriel"; party_name = "The Green"; colour = "#00FF00" }
        ];
        voters = [0 ; 1] };
      { id = 2;
        election_name = "Election 2";
        is_running = false;
        candidates = [{ candidate_name = "Bob"; party_name = "The Blue"; colour = "#0000FF" }];
        voters = [2] };
    ] in
    { voter_data; election_data }
end