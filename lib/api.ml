module Json = Yojson.Basic
module Db = Database_service

module M = App_utils
  
let get_header_token request = let (let*) = Option.bind in
  begin
    let* auth_header = Dream.header request "Authorization" in
    Scanf.sscanf_opt auth_header "Bearer %s" Fun.id
  end
  |> M.lift_option
  |> M.on_error `Unauthorized ~message:"Missing token"

let get_json_body request =
  Dream.body request
  |> Lwt.map Json.from_string
  |> M.lift_lwt
  |> M.on_error `Bad_Request ~message:"Request body is not a valid JSON"

module Make (Services : Services.S) = struct
  module Auth = Services.Auth
  module Db = Services.Db

  let auth = Auth.create ()
  let db = Db.create ()

  (*   ┌───────────────────────────┐                                                              *)
  (* ──┤ Authentication middleware ├───────────────────────────────────────────────────────────── *)
  (*   └───────────────────────────┘                                                              *)

  let voter_id_header_name = "X-Voter-ID"

  let bearer_middleware handler request = let open M.Syntax in M.retract @@
    let* access_token = get_header_token request in
    let* voter_id = Auth.validate_session auth ~access_token
      |> M.lift_result
      |> M.on_error `Unauthorized
    in
    Dream.set_header request voter_id_header_name (string_of_int voter_id);
    M.ok @@ handler request
  
  let get_voter_id request =
    Dream.header request voter_id_header_name
    |> Option.get
    |> int_of_string


  let admin_middleware handler request = let open M.Syntax in M.retract @@
    let* user, password =
      begin let (let*) = Option.bind in
        let* auth_header = Dream.header request "Authorization" in
        let* encoded_credentials = Scanf.sscanf_opt auth_header "Basic %s" Fun.id in
        let* decoded_credentials = Result.to_option @@ Base64.decode encoded_credentials in
        match String.split_on_char ':' decoded_credentials with
        | [user; password] -> Some (user, password)
        | _ -> None
      end
      |> M.lift_option
      |> M.on_error `Unauthorized ~message:"Invalid credentials format"
    in
    let* () = M.guard (Auth.valid_admin_credentials auth ~user ~password)
      |> M.on_error `Unauthorized ~message:"Invalid credentials"
    in
    M.ok @@ handler request

  (*   ┌──────────────────┐                                                                       *)
  (* ──┤ Session handling ├────────────────────────────────────────────────────────────────────── *)
  (*   └──────────────────┘                                                                       *)
  
  let create_session request = let open M.Syntax in M.retract @@
    let* json = get_json_body request in
    let* voter_id, password =
      begin
        match json with
        | `Assoc [("voter_id", `Int voter_id); ("password", `String password)] ->
          Some (voter_id, password)
        | _ -> None
      end
      |> M.lift_option
      |> M.on_error `Bad_Request ~message:"JSON object does not have the expected shape"
    in
    let+ () = M.guard (Auth.valid_credentials auth { voter_id; password })
      |> M.on_error `Unauthorized ~message:"Invalid credentials"
    in
    let access_token, refresh_token = Auth.create_session auth ~voter_id in
    let response_body = Json.to_string @@ `Assoc [
      ("access_token", `String access_token);
      ("refresh_token", `String refresh_token);
    ] in
    Dream.response ~status:`OK response_body
  
  let refresh_session request = let open M.Syntax in M.retract @@
    let* refresh_token = get_header_token request in
    let+ access_token, refresh_token = Auth.refresh_session auth ~refresh_token
      |> M.lift_result
      |> M.on_error `Unauthorized (* message provided by auth service *)
    in
    let response_body = Json.to_string @@ `Assoc [
      ("access_token", `String access_token);
      ("refresh_token", `String refresh_token);
    ] in
    Dream.response ~status:`OK response_body
  
  let delete_session request = let open M.Syntax in M.retract @@
    let* access_token = get_header_token request in
    let+ () = Auth.delete_session auth ~access_token
      |> M.lift_result
      |> M.on_error `Unauthorized
    in
    Dream.response ~status:`OK ""

  (*   ┌───────────────┐                                                                          *)
  (* ──┤ Introspection ├───────────────────────────────────────────────────────────────────────── *)
  (*   └───────────────┘                                                                          *)

  let whoami request = let open M.Syntax in M.retract @@
    let+ name = Db.name_of_voter db (get_voter_id request)
      |> M.lift_option
      |> M.on_error `Internal_Server_Error ~message:"Voter not found"
    in
    Dream.response ~status:`OK (Json.to_string @@ `Assoc [("name", `String name)])

  (*   ┌───────────────────────────────┐                                                          *)
  (* ──┤ Domain-related user endpoints ├───────────────────────────────────────────────────────── *)
  (*   └───────────────────────────────┘                                                          *)

  let get_elections request = M.retract @@ M.return @@
    let elections_json = Db.election_summaries_of_voter db (get_voter_id request) in
    Dream.response ~status:`OK (Json.to_string elections_json)
  
  let get_election_details request = let open M.Syntax in M.retract @@
    let* election_id = int_of_string_opt (Dream.param request "id")
      |> M.lift_option
      |> M.on_error `Bad_Request ~message:"Wrong election ID format"
    in
    let* () = M.guard (Db.election_exists db election_id)
      |> M.on_error `Bad_Request ~message:"Election does not exist"
    in
    let+ () = M.guard (Db.can_vote db ~voter_id:(get_voter_id request) ~election_id)
      |> M.on_error `Forbidden ~message:"The current voter does not take part in this election"
    in
    let election = Db.get_election_by_id db election_id in
    Dream.response ~status:`OK (Json.to_string election)
  
  let vote request = let open M.Syntax in M.retract @@
    let* election_id = M.lift_option @@ int_of_string_opt (Dream.param request "id")
      |> M.on_error `Bad_Request ~message:"Wrong election ID format"
    in
    let* json = get_json_body request in
    let* ballot =
      begin
        match json with
        | `List l -> Utils.option_traverse (function `Int i -> Some i | _ -> None) l
        | _ -> None
      end
      |> M.lift_option
      |> M.on_error `Bad_Request ~message:"JSON object does not have the expected shape"
    in
    let voter_id = get_voter_id request in
    let* () = M.guard (Db.election_exists db election_id)
      |> M.on_error `Bad_Request ~message:"Election does not exist"
    in
    let* () = M.guard (Db.can_vote db ~voter_id ~election_id)
      |> M.on_error `Forbidden ~message:"The current voter does not take part in this election"
    in
    let* () = M.guard (Db.nb_voters db ~election_id = List.length ballot)
      |> M.on_error `Bad_Request ~message:"Incomplete ballot"
    in
    let* () = M.guard (Db.election_is_running db election_id)
      |> M.on_error `Forbidden ~message:"Election is no longer running"
    in
    let+ () = M.guard (not @@ Db.has_voted db ~voter_id ~election_id)
      |> M.on_error `Forbidden ~message:"The current voter has already voted"
    in
    Db.vote db ~election_id ~voter_id ~ballot;
    Dream.response ~status:`OK ""

  let get_election_results request = let open M.Syntax in M.retract @@
    let* election_id = int_of_string_opt (Dream.param request "id")
      |> M.lift_option
      |> M.on_error `Bad_Request ~message:"Wrong election ID format"
    in
    let* () = M.guard (Db.election_exists db election_id)
      |> M.on_error `Bad_Request ~message:"Election does not exist"
    in
    let* () = M.guard (not @@ Db.election_is_running db election_id)
      |> M.on_error `Forbidden ~message:"Election is still running"
    in
    let+ () = M.guard (Db.can_vote db ~voter_id:(get_voter_id request) ~election_id)
      |> M.on_error `Forbidden ~message:"The current voter does not take part in this election"
    in
    let vote_counts = Db.vote_counts_of_election db election_id in
    let nb_voters = Db.nb_voters db ~election_id in
    let winner, explanation, participation, results =
      Judgment.compute_results ~vote_counts ~nb_voters
    in
    let results_json =
      results
      |> Array.map (fun (majority_rating, scores) -> `Assoc [
        ("majority_rating", `Int majority_rating);
        ("scores", `List (scores |> Array.map (fun s -> `Float s) |> Array.to_list))
      ])
      |> fun a -> `List (Array.to_list a)
    in
    let response_body = Json.to_string @@ `Assoc [
      ("winner", `Int winner);
      ("explanation", `String (Judgment.Explanation.to_string explanation));
      ("participation", `Float participation);
      ("results", results_json)
    ] in
    Dream.response ~status:`OK response_body

  (*   ┌──────────────────────────┐                                                               *)
  (* ──┤ Administration endpoints ├────────────────────────────────────────────────────────────── *)
  (*   └──────────────────────────┘                                                               *)

  let terminate_election request = let open M.Syntax in M.retract @@
    let* election_id = int_of_string_opt (Dream.param request "id")
      |> M.lift_option
      |> M.on_error `Bad_Request ~message:"Wrong election ID format"
    in
    let* () = M.guard (Db.election_exists db election_id)
      |> M.on_error `Bad_Request ~message:"Election does not exist"
    in
    let+ () = M.guard (Db.election_is_running db election_id)
      |> M.on_error `Not_Acceptable ~message:"Election is not running"
    in
    Db.terminate_election db election_id;
    Dream.response ~status:`OK ""
end
