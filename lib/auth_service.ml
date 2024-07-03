module type S = sig
  type auth

  type credentials = {
    voter_id : int;
    password : string
  }

  val create : unit -> auth
  val valid_credentials : auth -> credentials -> bool
  val create_session : auth -> voter_id:int -> string * string
  val validate_session : auth -> access_token:string -> (int, string) result
  val refresh_session : auth -> refresh_token:string -> (string * string, string) result
  val delete_session : auth -> access_token:string -> (unit, string) result
end

module Default : S = struct
  open Jose

  type credentials = {
    voter_id : int;
    password : string
  }

  type auth = {
    access_token_lifetime : int;
    refresh_token_lifetime : int;
    login : (int, string) Hashtbl.t;
    sessions : (int, string * string) Hashtbl.t
  }

  let valid_credentials auth credentials =
    match Hashtbl.find_opt auth.login credentials.voter_id with
    | Some password when password = credentials.password -> true
    | _ -> false

  let private_key = Jwk.make_oct "secret"

  type claims = {
    token_type : string;
    voter_id : int;
    exp : int
  }

  let claims_to_string_jwt claims = let open Jwt in
    empty_payload
    |> add_claim "token_type" (`String claims.token_type)
    |> add_claim "voter_id" (`Int claims.voter_id)
    |> add_claim "exp" (`Int claims.exp)
    |> fun payload -> sign ~payload private_key
    |> Result.get_ok (* no floats in the JSON values generated here *)
    |> to_string

  let claims_of_string_jwt str =
    match Jwt.of_string ~jwk:private_key ~now:(Ptime_clock.now ()) str with
    | Error `Expired -> Error "Expired token"
    | Error `Invalid_signature -> Error "Invalid signature on the token"
    | Error _ -> Error "Invalid JWT"
    | Ok jwt ->
      begin let (let*) = Option.bind in
        let* token_type = Jwt.get_string_claim jwt "token_type" in
        let* voter_id = Jwt.get_int_claim jwt "voter_id" in
        let* exp = Jwt.get_int_claim jwt "exp" in
        Some { token_type; voter_id; exp }
      end
      |> Option.to_result ~none:"Invalid claims in the token"

  let make_access_token ~voter_id ~duration =
    { token_type = "access_token"; voter_id; exp = int_of_float (Unix.time ()) + duration }
    |> claims_to_string_jwt

  let make_refresh_token ~voter_id ~duration =
    { token_type = "refresh_token"; voter_id; exp = int_of_float (Unix.time ()) + duration }
    |> claims_to_string_jwt

  let create_session auth ~voter_id =
    let access_token = make_access_token ~voter_id ~duration:auth.access_token_lifetime in
    let refresh_token = make_refresh_token ~voter_id ~duration:auth.refresh_token_lifetime in
    Hashtbl.replace auth.sessions voter_id (access_token, refresh_token);
    access_token, refresh_token

  let refresh_session auth ~refresh_token = let (let*) = Result.bind in
    let* { voter_id; _ } = claims_of_string_jwt refresh_token in
    match Hashtbl.find_opt auth.sessions voter_id with
    | Some (_, refresh_token') when refresh_token' = refresh_token ->
        Ok (create_session auth ~voter_id)
    | _ -> begin
      Hashtbl.remove auth.sessions voter_id;
      Error "Token not in use - invalidating the session"
    end

  let delete_session auth ~access_token = let (let*) = Result.bind in
    let* { voter_id; _ } = claims_of_string_jwt access_token in
    Ok (Hashtbl.remove auth.sessions voter_id)

  let validate_session auth ~access_token = let (let*) = Result.bind in
    let* { voter_id; _ } = claims_of_string_jwt access_token in
    match Hashtbl.find_opt auth.sessions voter_id with
    | Some (access_token', _) when access_token' = access_token -> Ok voter_id
    | _ -> begin
      Hashtbl.remove auth.sessions voter_id;
      Error "Token not in use - invalidating the session"
    end

  let create () =
    let test_login = Hashtbl.of_seq @@ List.to_seq [(0, "0000"); (1, "1111"); (2, "2222")] in
    { access_token_lifetime = 60;
      refresh_token_lifetime = 300;
      login = test_login;
      sessions = Hashtbl.create 7 }
end
