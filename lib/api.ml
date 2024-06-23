module Json = Yojson.Basic
module Db = Database_service

module M = App_utils
let token_lifetime_seconds = 5 * 60

module Make (Services : Services.S) = struct
  module Auth = Services.Auth

  let auth = Auth.create ()

  let get_json_body request = let open Lwt in
    (Dream.body request >|= Json.from_string)

  let parse_credentials = function
    | `Assoc [("voter_id", `Int voter_id); ("password", `String password)] ->
      Some (voter_id, password)
    | _ -> None

  let get_token_handler request = let open M.Syntax in
    let* json = M.lift_lwt (get_json_body request)
      |> M.on_error `Bad_Request ~message:"Request body is not a valid JSON" in
    let* voter_id, password = M.lift_option (parse_credentials json)
      |> M.on_error `Bad_Request ~message:"JSON object does not have the expected shape" in
    let* () =
      M.guard (Auth.valid_credentials auth { voter_id; password })
      |> M.on_error `Unauthorized ~message:"Invalid credentials"
    in
    let* access_token, refresh_token =
      M.lift_option (Auth.create_session auth ~voter_id ~duration:token_lifetime_seconds)
      |> M.on_error `Not_Acceptable ~message:"Session already exists for this voter"
    in
    let response_data = `Assoc [
      ("access_token", `String access_token);
      ("expires_in", `Int token_lifetime_seconds);
      ("refresh_token", `String refresh_token);
    ] in
    M.ok @@ Dream.json ~status:`OK (Json.to_string response_data)

  let get_token_handler request = M.retract (get_token_handler request)
end
