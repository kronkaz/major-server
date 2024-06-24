module type S = sig
  type db
  
  val create : unit -> db
end