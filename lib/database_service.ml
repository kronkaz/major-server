module Json = Yojson.Basic

module type S = sig
  type db
  
  val create : unit -> db
  val name_of_voter : db -> int -> string option
  val election_summaries_of_voter : db -> int -> Json.t
  val election_exists : db -> int -> bool
  val can_vote : db -> voter_id:int -> election_id:int -> bool
  val get_election_by_id : db -> int -> Json.t
  val nb_voters : db -> election_id:int -> int
  val nb_candidates : db -> int -> int
  val election_is_running : db -> int -> bool
  val has_voted : db -> election_id:int -> voter_id:int -> bool
  val vote : db -> election_id:int -> voter_id:int -> ballot:int list -> unit
  val terminate_election : db -> int -> unit
  val vote_counts_of_election : db -> int -> int array array
end

module IntSet = Set.Make(Int)

module Default : S = struct
  type candidate = {
    candidate_name : string;
    party_name : string;
    colour : string
  }

  type election = {
    id : int;
    election_name : string;
    mutable is_running : bool;
    candidates : candidate list;
    voters : int list;
    mutable have_voted : IntSet.t;
    votes : int array array (* [| ratings_candidate1; ...; ratings_candidate_n |] *)
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

  let nb_voters db ~election_id =
    let election = List.find (fun election -> election.id = election_id) db.election_data in
    List.length election.voters

  let nb_candidates db election_id =
    let election = List.find (fun election -> election.id = election_id) db.election_data in
    List.length election.candidates
  
  let election_is_running db election_id =
    List.find (fun election -> election.id = election_id) db.election_data
    |> fun election -> election.is_running

  let vote db ~election_id ~voter_id ~ballot =
    let election = List.find (fun election -> election.id = election_id) db.election_data in
    election.have_voted <- IntSet.add voter_id election.have_voted;
    Array.iter2
      (fun candidate_ratings rating ->
        candidate_ratings.(rating) <- candidate_ratings.(rating) + 1)
      election.votes (Array.of_list ballot)

  let has_voted db ~election_id ~voter_id =
    List.find (fun election -> election.id = election_id) db.election_data
    |> fun election -> IntSet.mem voter_id election.have_voted
  
  let terminate_election db election_id =
    let election = List.find (fun election -> election.id = election_id) db.election_data in
    election.is_running <- false

  let vote_counts_of_election db election_id =
    let election = List.find (fun election -> election.id = election_id) db.election_data in
    election.votes

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
        voters = [0 ; 1];
        have_voted = IntSet.empty;
        votes = Array.init 3 (fun _ -> Array.make 7 0) };
      { id = 2;
        election_name = "Election 2";
        is_running = false;
        candidates = [{ candidate_name = "Bob"; party_name = "The Blue"; colour = "#0000FF" }];
        voters = [2];
        have_voted = IntSet.empty;
        votes = [| Array.make 7 0 |] };
    ] in
    { voter_data; election_data }
end