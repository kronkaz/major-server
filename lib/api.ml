module Json = Yojson.Basic
module Db = Database_service

module M = App_utils

module Make (Services : Services.S) = struct
  module Auth = Services.Auth
  module Db = Services.Db

  let auth = Auth.create ()
  let db = Db.create ()

  let get_json_body request = let open Lwt in
    (Dream.body request >|= Json.from_string)
  
  let parse_header_token request = let (let*) = Option.bind in
    let* auth_header = Dream.header request "Authorization" in
    Scanf.sscanf_opt auth_header "Bearer %s" Fun.id 

  let parse_credentials = function
    | `Assoc [("voter_id", `Int voter_id); ("password", `String password)] ->
      Some (voter_id, password)
    | _ -> None

  let create_session request = let open M.Syntax in
    let* json = M.lift_lwt (get_json_body request)
      |> M.on_error `Bad_Request ~message:"Request body is not a valid JSON" in
    let* voter_id, password = M.lift_option (parse_credentials json)
      |> M.on_error `Bad_Request ~message:"JSON object does not have the expected shape" in
    let* () = M.guard (Auth.valid_credentials auth { voter_id; password })
      |> M.on_error `Unauthorized ~message:"Invalid credentials" in
    let access_token, refresh_token = Auth.create_session auth ~voter_id in
    let response_data = `Assoc [
      ("access_token", `String access_token);
      ("refresh_token", `String refresh_token);
    ] in
    M.ok @@ Dream.json ~status:`OK (Json.to_string response_data)
  
  let refresh_session request = let open M.Syntax in
    let* refresh_token = M.lift_option (parse_header_token request)
      |> M.on_error `Unauthorized ~message:"Missing refresh token" in
    let* access_token, refresh_token = M.lift_result @@ Auth.refresh_session auth ~refresh_token
      |> M.on_error `Unauthorized in
    let response_data = `Assoc [
      ("access_token", `String access_token);
      ("refresh_token", `String refresh_token);
    ] in
    M.ok @@ Dream.json ~status:`OK (Json.to_string response_data)
  
  let delete_session request = let open M.Syntax in
    let* access_token = M.lift_option (parse_header_token request)
      |> M.on_error `Unauthorized ~message:"Missing access token" in
    let* () = M.lift_result @@ Auth.delete_session auth ~access_token
      |> M.on_error `Unauthorized in
    M.ok @@ Dream.respond ~status:`OK ""

  let whoami request = let open M.Syntax in
    let* access_token = M.lift_option (parse_header_token request)
      |> M.on_error `Unauthorized ~message:"Missing access token" in
    let* voter_id = M.lift_result @@ Auth.validate_session auth ~access_token
      |> M.on_error `Unauthorized in
    let* name = M.lift_option (Db.name_of_voter db voter_id)
      |> M.on_error `Internal_Server_Error ~message:"Voter not found" in
    M.ok @@ Dream.json ~status:`OK (Json.to_string @@ `Assoc [("name", `String name)])

  let create_session request = M.retract (create_session request)
  let refresh_session request = M.retract (refresh_session request)
  let delete_session request = M.retract (delete_session request)
  let whoami request = M.retract (whoami request)
end
