open Jose

type credentials = {
  voter_id : int;
  password : string
}

type auth = {
  login : (int, string) Hashtbl.t;
  sessions : (int, string * string) Hashtbl.t
}

let valid_credentials auth credentials =
  match Hashtbl.find_opt auth.login credentials.voter_id with
  | Some password when password = credentials.password -> true
  | _ -> false

let private_key = Jwk.make_oct "secret"

let make_access_token ~voter_id ~duration = let open Jwt in
  empty_payload
  |> add_claim "token_type" (`String "access_token")
  |> add_claim "voter_id" (`Int voter_id)
  |> add_claim "expiry"   (`Int (int_of_float (Unix.time ()) + duration))
  |> fun payload -> sign ~payload private_key
  |> Result.get_ok (* no floats in the JSON values generated here *)
  |> to_string

let make_refresh_token ~voter_id ~duration = let open Jwt in
  empty_payload
  |> add_claim "token_type" (`String "refresh_token")
  |> add_claim "voter_id" (`Int voter_id)
  |> add_claim "expiry"   (`Int (int_of_float (Unix.time ()) + duration))
  |> fun payload -> sign ~payload private_key
  |> Result.get_ok (* no floats in the JSON values generated here *)
  |> to_string

let create_session auth ~voter_id ~duration =
  if Hashtbl.mem auth.sessions voter_id then
    None
  else
    let refresh_token = make_refresh_token ~voter_id ~duration in
    let access_token = make_access_token ~voter_id ~duration in
    Hashtbl.add auth.sessions voter_id (access_token, refresh_token);
    Some (access_token, refresh_token)

let create () =
  let test_login = Hashtbl.of_seq @@ List.to_seq [(0, "0000"); (1, "1111"); (2, "2222")] in
  let test_sessions =
    let refresh_token = make_refresh_token ~voter_id:1 ~duration:300 in
    let access_token = make_access_token ~voter_id:1 ~duration:300 in
    Hashtbl.of_seq @@ List.to_seq [(1, (access_token, refresh_token))]
  in
  { login = test_login; sessions = test_sessions }
