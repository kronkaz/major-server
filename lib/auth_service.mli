module type S = sig
  type auth

  type credentials = {
    voter_id : int;
    password : string
  }

  val create : unit -> auth
  val valid_credentials : auth -> credentials -> bool
  val create_session : auth -> voter_id:int -> duration:int -> (string * string) option
end