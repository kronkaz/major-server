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

module Default : S
