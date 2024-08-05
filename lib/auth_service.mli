module type S = sig
  type auth

  val create : unit -> auth
  val create_user : auth -> user:string -> password:string -> int
  val valid_credentials : auth -> user:string -> password:string -> int option
  val valid_admin_credentials : auth -> user:string -> password:string -> bool
  val create_session : auth -> voter_id:int -> string * string
  val validate_session : auth -> access_token:string -> (int, string) result
  val refresh_session : auth -> refresh_token:string -> (string * string, string) result
  val delete_session : auth -> access_token:string -> (unit, string) result
end

module Default : functor (_ : sig
  val admin_credentials : string * string
end) -> S
