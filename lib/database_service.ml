open Domain

module Json = Yojson.Basic

module type S = sig
  type db
  
  val create : unit -> db
  val name_of_voter : db -> voter_id:int -> string option
  val election_summaries_of_voter : db -> voter_id:int -> Election_summary.t list
  val election_exists : db -> election_id:int -> bool
  val can_vote : db -> voter_id:int -> election_id:int -> bool
  val get_election : db -> election_id:int -> Election_info.t
  val nb_voters : db -> election_id:int -> int
  val candidates : db -> election_id:int -> Candidate.t list
  val election_is_running : db -> election_id:int -> bool
  val has_voted : db -> election_id:int -> voter_id:int -> bool
  val vote : db -> election_id:int -> voter_id:int -> ballot:(Rating.t CandidateMap.t) -> unit
  val terminate_election : db -> election_id:int -> unit
  val votes_of_election : db -> election_id:int -> int array CandidateMap.t
end

module IntSet = Set.Make(Int)

module Default : S = struct
  type election = {
    id : int;
    name : string;
    mutable is_running : bool;
    candidates : Candidate.t list;
    voters : int list;
    mutable have_voted : IntSet.t;
    votes : int array CandidateMap.t
  }

  let election_to_summary { id; name; is_running; _ } =
    Domain.Election_summary.({ id; name; is_running })

  let election_to_election_info { id; name; is_running; candidates; _ } =
    Domain.Election_info.{ id; name; is_running; candidates }

  type db = {
    voter_data : (int, string) Hashtbl.t;
    election_data : election list
  }

  let name_of_voter db ~voter_id = Hashtbl.find_opt db.voter_data voter_id

  let election_summaries_of_voter db ~voter_id =
    db.election_data
    |> List.filter_map (fun election ->
         if not (List.mem voter_id election.voters) then None
         else Some (election_to_summary election))

  let election_exists db ~election_id =
    List.exists (fun election -> election.id = election_id) db.election_data

  let can_vote db ~voter_id ~election_id =
    let election = List.find (fun election -> election.id = election_id) db.election_data in
    List.mem voter_id election.voters

  let get_election db ~election_id =
    List.find (fun election -> election.id = election_id) db.election_data
    |> election_to_election_info

  let nb_voters db ~election_id =
    List.find (fun election -> election.id = election_id) db.election_data
    |> fun election -> List.length election.voters

  let candidates db ~election_id =
    let election = List.find (fun election -> election.id = election_id) db.election_data in
    election.candidates
  
  let election_is_running db ~election_id =
    List.find (fun election -> election.id = election_id) db.election_data
    |> fun election -> election.is_running

  let vote db ~election_id ~voter_id ~(ballot : Rating.t CandidateMap.t) =
    let election = List.find (fun election -> election.id = election_id) db.election_data in
    election.have_voted <- IntSet.add voter_id election.have_voted;
    ballot |> CandidateMap.iter @@ fun candidate rating ->
      let candidate_ratings = CandidateMap.find candidate election.votes in
      let i = Rating.to_int rating in
      candidate_ratings.(i) <- candidate_ratings.(i) + 1

  let has_voted db ~election_id ~voter_id =
    List.find (fun election -> election.id = election_id) db.election_data
    |> fun election -> IntSet.mem voter_id election.have_voted
  
  let terminate_election db ~election_id =
    let election = List.find (fun election -> election.id = election_id) db.election_data in
    election.is_running <- false

  let votes_of_election db ~election_id =
    let election = List.find (fun election -> election.id = election_id) db.election_data in
    election.votes

  let create () =
    let voter_data = Hashtbl.of_seq @@ List.to_seq
      [(0, "Jean Dupont"); (1, "John Smith"); (2, "Giuseppe Ragazzo")]
    in
    let candidates1 = Candidate.[
      { id = 1; name = "Bob"; party = "The Blue"; colour = "#0000FF" };
      { id = 2; name = "Ronald"; party = "The Red"; colour = "#FF0000" };
      { id = 3; name = "Gabriel"; party = "The Green"; colour = "#00FF00" }
    ] in
    let candidate2 = Candidate.{ id = 1; name = "Bob"; party = "The Blue"; colour = "#0000FF" } in
    let election_data = [
      { id = 1;
        name = "Election 1";
        is_running = true;
        candidates = candidates1;
        voters = [0 ; 1];
        have_voted = IntSet.empty;
        votes =
          candidates1
          |> List.map (fun candidate -> candidate, Array.make 7 0)
          |> CandidateMap.of_list };
      { id = 2;
        name = "Election 2";
        is_running = false;
        candidates = [candidate2];
        voters = [2];
        have_voted = IntSet.empty;
        votes = CandidateMap.of_list [(candidate2, Array.make 7 0)] };
    ] in
    { voter_data; election_data }
end
