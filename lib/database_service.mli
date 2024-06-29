module type S = sig
  type db
  
  val create : unit -> db
  val name_of_voter : db -> int -> string option
end