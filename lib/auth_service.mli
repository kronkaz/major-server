module type S = sig
  type auth

  type credentials = {
    voter_id : int;
    password : string
  }

  val create : unit -> auth
  val create_user : auth -> password:string -> int
  val valid_credentials : auth -> credentials -> bool
  val valid_admin_credentials : auth -> user:string -> password:string -> bool
  val create_session : auth -> voter_id:int -> string * string
  val validate_session : auth -> access_token:string -> (int, string) result
  val refresh_session : auth -> refresh_token:string -> (string * string, string) result
  val delete_session : auth -> access_token:string -> (unit, string) result
end

module Default : S
