open Domain

module Json = Yojson.Basic

module IntMap = Utils.IntMap

module type S = sig
  type db
  
  val create : unit -> db
  val add_user_info : db -> voter_id:int -> UserInfo.t -> unit
  val username_exists : db -> username:string -> bool
  val get_user_info : db -> voter_id:int -> UserInfo.t option
  val get_all_user_info : db -> UserInfo.t IntMap.t
  val election_summaries_of_voter : db -> voter_id:int -> Election_summary.t list
  val election_exists : db -> election_id:int -> bool
  val can_vote : db -> voter_id:int -> election_id:int -> bool
  val get_election : db -> election_id:int -> Election_info.t
  val nb_voters : db -> election_id:int -> int
  val candidates : db -> election_id:int -> Candidate.t list
  val election_is_running : db -> election_id:int -> bool
  val has_voted : db -> election_id:int -> voter_id:int -> bool
  val vote : db -> election_id:int -> voter_id:int -> ballot:(Rating.t CandidateMap.t) -> unit
  val add_election : db -> Election.t -> int
  val terminate_election : db -> election_id:int -> unit
  val votes_of_election : db -> election_id:int -> int array CandidateMap.t
end

module IntSet = Set.Make(Int)

module Default (Config : sig
  val admin_username : string
end) = struct
  type election = {
    id : int;
    kind : string;
    name : string;
    date : string;
    mutable is_running : bool;
    candidates : Candidate.t list;
    voters : int list;
    mutable have_voted : IntSet.t;
    votes : int array CandidateMap.t
  }

  let election_to_summary { id; kind; name; date; is_running; _ } =
    Domain.Election_summary.({ id; kind; name; date; is_running })

  let election_to_election_info { id; kind; name; date; is_running; candidates; _ } =
    Domain.Election_info.{ id; kind; name; date; is_running; candidates }

  type db = {
    user_data : (int, UserInfo.t) Hashtbl.t;
    mutable id_counter : int;
    mutable election_data : election list
  }

  let add_user_info db ~voter_id user_info = Hashtbl.add db.user_data voter_id user_info

  let username_exists db ~username =
    db.user_data
    |> Hashtbl.to_seq_values
    |> Seq.exists (fun UserInfo.{ user; _ } -> user = username)
  
  let get_user_info db ~voter_id = Hashtbl.find_opt db.user_data voter_id

  let get_all_user_info db =
    db.user_data
    |> Hashtbl.to_seq
    |> IntMap.of_seq

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

  let add_election db Election.{ kind; name; date; candidates; voters } =
    let id = db.id_counter in
    let election = {
      id;
      kind;
      name;
      date;
      is_running = true;
      candidates;
      voters;
      have_voted = IntSet.empty;
      votes =
        candidates
        |> List.map (fun candidate -> candidate, Array.make 7 0)
        |> CandidateMap.of_list
    } in
    db.election_data <- election :: db.election_data;
    db.id_counter <- db.id_counter + 1;
    id
  
  let terminate_election db ~election_id =
    let election = List.find (fun election -> election.id = election_id) db.election_data in
    election.is_running <- false

  let votes_of_election db ~election_id =
    let election = List.find (fun election -> election.id = election_id) db.election_data in
    election.votes

  let create () =
    let user_data = Hashtbl.of_seq @@ List.to_seq UserInfo.[
      (0, { user = Config.admin_username; name = "Administrator" })
    ] in
    { user_data; id_counter = 1; election_data = [] }
end
