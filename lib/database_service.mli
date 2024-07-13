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

module Default : S
