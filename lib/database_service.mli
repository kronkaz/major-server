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

module Default : S
