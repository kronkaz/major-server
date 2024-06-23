open Jose

type credentials = {
  voter_id : int;
  password : string
}

type auth = {
  login : (int, string) Hashtbl.t;
  sessions : (int, string * string) Hashtbl.t
}

let create () = {
  login = Hashtbl.create 7;
  sessions = Hashtbl.create 7
}

let valid_credentials auth credentials =
  match Hashtbl.find_opt auth.login credentials.voter_id with
  | Some password when password = credentials.password -> true
  | _ -> false

let private_key = Jwk.make_oct "secret"

let create_session auth ~voter_id ~duration =
  if Hashtbl.mem auth.sessions voter_id then
    None
  else let open Jwt in
    let access_token =
      empty_payload
      |> add_claim "voter_id" (`Int voter_id)
      |> add_claim "expiry"   (`Int (int_of_float (Unix.time ()) + duration))
      |> fun payload -> sign ~payload private_key
      |> Result.get_ok (* no floats in the JSON values generated here *)
      |> to_string
    in
    let refresh_token =
      empty_payload
      |> add_claim "voter_id" (`Int voter_id)
      |> fun payload -> sign ~payload private_key
      |> Result.get_ok
      |> to_string
    in
    Hashtbl.add auth.sessions voter_id (access_token, refresh_token);
    Some (access_token, refresh_token)
