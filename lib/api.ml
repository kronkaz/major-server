module Json = Yojson.Basic
module Db = Database_service

module M = App_utils

open Domain

module IntMap = Utils.IntMap
  
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

  let auth = ref @@ Auth.create ()
  let db = ref @@ Db.create ()

(*   ┌───────────────────────────┐                                                                *)
(* ──┤ Authentication middleware ├─────────────────────────────────────────────────────────────── *)
(*   └───────────────────────────┘                                                                *)

  let voter_id_header_name = "X-Voter-ID"

  let bearer_middleware handler request = let open M.Syntax in M.retract @@
    let* access_token = get_header_token request in
    let* voter_id = Auth.validate_session !auth ~access_token
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
    let* () = M.guard (Auth.valid_admin_credentials !auth ~user ~password)
      |> M.on_error `Unauthorized ~message:"Invalid credentials"
    in
    M.ok @@ handler request

(*   ┌──────────────────┐                                                                         *)
(* ──┤ Session handling ├──────────────────────────────────────────────────────────────────────── *)
(*   └──────────────────┘                                                                         *)

  let create_user request = let open M.Syntax in M.retract @@
    let* json = get_json_body request in
    let* user, name, password =
      begin let (let*) = Option.bind in
        let* l = Utils.object_of_json_opt json in
        let* u = List.assoc_opt "username" l in
        let* n = List.assoc_opt "name" l in
        let* p = List.assoc_opt "password" l in
        let* user = Utils.string_of_json_opt u in
        let* name = Utils.string_of_json_opt n in
        let* password = Utils.string_of_json_opt p in
        Some (user, name, password)
      end
      |> M.lift_option
      |> M.on_error `Bad_Request ~message:"JSON object does not have the expected shape"
    in
    let* () = M.guard (String.length user > 0)
      |> M.on_error `Bad_Request ~message:"Empty username"
    in
    let* () = M.guard (String.length user <= 32)
      |> M.on_error `Bad_Request ~message:"Username is too long"
    in
    let valid_char c =
      let code = Char.code c in
      code >= 33 && code <= 126 (* non-space printable ascii *)
    in
    let* () = M.guard (String.for_all valid_char user)
      |> M.on_error `Bad_Request ~message:"Username is not non-space printable ascii only"
    in
    let* () = M.guard (not (Db.username_exists !db ~username:user))
      |> M.on_error `Not_Acceptable ~message:"This username is already taken"
    in
    let* () = M.guard (String.length name > 0)
      |> M.on_error `Bad_Request ~message:"Empty name"
    in
    let* () = M.guard (String.length name <= 64)
      |> M.on_error `Bad_Request ~message:"Name is too long"
    in
    let* () = M.guard (String.length password >= 8)
      |> M.on_error `Bad_Request ~message:"Password must contain at least 8 characters"
    in
    let+ () = M.guard (String.length password <= 64)
      |> M.on_error `Bad_Request ~message:"Password is too long"
    in
    let voter_id = Auth.create_user !auth ~user ~password in
    Db.add_user_info !db ~voter_id UserInfo.{ user; name };
    Dream.response ~status:`OK (Json.to_string @@ `Assoc [("id", `Int voter_id)])
  
  let create_session request = let open M.Syntax in M.retract @@
    let* json = get_json_body request in
    let* user, password =
      begin
        match json with
        | `Assoc [("username", `String username); ("password", `String password)] ->
          Some (username, password)
        | _ -> None
      end
      |> M.lift_option
      |> M.on_error `Bad_Request ~message:"JSON object does not have the expected shape"
    in
    let+ voter_id = Auth.valid_credentials !auth ~user ~password
      |> M.lift_option
      |> M.on_error `Unauthorized ~message:"Invalid credentials"
    in
    let access_token, refresh_token = Auth.create_session !auth ~voter_id in
    let response_body = Json.to_string @@ `Assoc [
      ("access_token", `String access_token);
      ("refresh_token", `String refresh_token);
    ] in
    Dream.response ~status:`OK response_body
  
  let refresh_session request = let open M.Syntax in M.retract @@
    let* refresh_token = get_header_token request in
    let+ access_token, refresh_token = Auth.refresh_session !auth ~refresh_token
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
    let+ () = Auth.delete_session !auth ~access_token
      |> M.lift_result
      |> M.on_error `Unauthorized
    in
    Dream.response ~status:`OK ""

(*   ┌───────────────┐                                                                            *)
(* ──┤ Introspection ├─────────────────────────────────────────────────────────────────────────── *)
(*   └───────────────┘                                                                            *)

  let whoami request = let open M.Syntax in M.retract @@
    let+ { user; name } = Db.get_user_info !db ~voter_id:(get_voter_id request)
      |> M.lift_option
      |> M.on_error `Internal_Server_Error ~message:"Voter not found"
    in
    Dream.response ~status:`OK (Json.to_string @@ `Assoc [
      ("username", `String user);
      ("name", `String name);
    ])

(*   ┌───────────────────────────────┐                                                            *)
(* ──┤ Domain-related user endpoints ├─────────────────────────────────────────────────────────── *)
(*   └───────────────────────────────┘                                                            *)

  let get_elections request = M.retract @@ M.return @@
    let elections_json =
      Db.election_summaries_of_voter !db ~voter_id:(get_voter_id request)
      |> List.map Election_summary.to_json
      |> fun l -> `List l
    in
    Dream.response ~status:`OK (Json.to_string elections_json)
  
  let get_election_details request = let open M.Syntax in M.retract @@
    let* election_id = int_of_string_opt (Dream.param request "id")
      |> M.lift_option
      |> M.on_error `Bad_Request ~message:"Wrong election ID format"
    in
    let* () = M.guard (Db.election_exists !db ~election_id)
      |> M.on_error `Bad_Request ~message:"Election does not exist"
    in
    let+ () = M.guard (Db.can_vote !db ~voter_id:(get_voter_id request) ~election_id)
      |> M.on_error `Forbidden ~message:"The current voter does not take part in this election"
    in
    let election = Election_info.to_json @@ Db.get_election !db ~election_id in
    Dream.response ~status:`OK (Json.to_string election)
  
  let vote request = let open M.Syntax in M.retract @@
    let* election_id = M.lift_option @@ int_of_string_opt (Dream.param request "id")
      |> M.on_error `Bad_Request ~message:"Wrong election ID format"
    in
    let* () = M.guard (Db.election_exists !db ~election_id)
      |> M.on_error `Bad_Request ~message:"Election does not exist"
    in
    let candidates = Db.candidates !db ~election_id in
    let* json = get_json_body request in
    let* ballot, ballot_size =
      begin
        match json with
        | `List l ->
          l
          |> List.map (function
               | `Assoc [("id", `Int cid); ("rating", `Int i)] -> let (let*) = Option.bind in
                  let* rating = Rating.of_int_opt i in
                  let* candidate =
                    List.find_opt (fun Candidate.{ id; _ } -> id = cid) candidates
                  in
                  Some (candidate, rating)
               | _ -> None
             )
          |> Utils.option_sequence
          |> Option.map (fun l -> CandidateMap.of_list l, List.length l)
        | _ -> None
      end
      |> M.lift_option
      |> M.on_error `Bad_Request ~message:"JSON object does not have the expected shape"
    in
    let* () = M.guard (ballot_size = List.length candidates)
      |> M.on_error `Bad_Request ~message:"Incomplete ballot"
    in
    let voter_id = get_voter_id request in
    let* () = M.guard (Db.can_vote !db ~voter_id ~election_id)
      |> M.on_error `Forbidden ~message:"The current voter does not take part in this election"
    in
    let* () = M.guard (Db.election_is_running !db ~election_id)
      |> M.on_error `Forbidden ~message:"Election is no longer running"
    in
    let+ () = M.guard (not @@ Db.has_voted !db ~voter_id ~election_id)
      |> M.on_error `Forbidden ~message:"The current voter has already voted"
    in
    Db.vote !db ~election_id ~voter_id ~ballot;
    Dream.response ~status:`OK ""

  (* compute a / b in percentage rounded to 0.01 *)
  let ratio100 a b = let open Decimal in
    of_int a / of_int b * of_int 100
    |> round ~n:2
    |> to_float

  let get_election_results request = let open M.Syntax in M.retract @@
    let* election_id = int_of_string_opt (Dream.param request "id")
      |> M.lift_option
      |> M.on_error `Bad_Request ~message:"Wrong election ID format"
    in
    let* () = M.guard (Db.election_exists !db ~election_id)
      |> M.on_error `Bad_Request ~message:"Election does not exist"
    in
    let* () = M.guard (not @@ Db.election_is_running !db ~election_id)
      |> M.on_error `Forbidden ~message:"Election is still running"
    in
    let+ () = M.guard (Db.can_vote !db ~voter_id:(get_voter_id request) ~election_id)
      |> M.on_error `Forbidden ~message:"The current voter does not take part in this election"
    in
    let votes = Db.votes_of_election !db ~election_id in
    let votes_cast =
      votes
      |> CandidateMap.to_seq
      |> Seq.uncons
      |> Option.get
      |> fst
      |> snd
      |> Array.fold_left (+) 0
    in
    let scores =
      votes
      |> CandidateMap.map (fun candidate_votes ->
           Array.map (fun rating_count -> ratio100 rating_count votes_cast) candidate_votes)
    in
    let participation = ratio100 votes_cast (Db.nb_voters !db ~election_id) in
    let majority_ratings, winners = Judgment.majority_judgment ~votes in
    let scores_and_ratings =
      CandidateMap.merge
        (fun _ scores_opt majority_rating_opt ->
          Utils.option_liftM2 (fun x y -> (x, y)) scores_opt majority_rating_opt)
        scores
        majority_ratings
    in
    let response_body = Json.to_string @@ `Assoc [
      ("winners", `List (List.map (fun Candidate.{ id; _ } -> `Int id) winners));
      ("participation", `Float participation);
      ("results",
        scores_and_ratings
        |> CandidateMap.to_list
        |> List.map (fun (Candidate.{ id; _ }, (scores, majority_rating)) -> `Assoc [
             ("id", `Int id);
             ("majority_rating",
               Option.fold majority_rating
                 ~none:`Null
                 ~some:(fun rating -> `Int (Rating.to_int rating)));
             ("scores", `List (scores |> Array.map (fun s -> `Float s) |> Array.to_list))
           ])
        |> fun l -> `List l)
    ] in
    Dream.response ~status:`OK response_body

(*   ┌──────────────────────────┐                                                                 *)
(* ──┤ Administration endpoints ├──────────────────────────────────────────────────────────────── *)
(*   └──────────────────────────┘                                                                 *)

  let get_users _ = M.retract @@ M.return @@
    begin
      Db.get_all_user_info !db
      |> IntMap.bindings
      |> List.map (fun (voter_id, UserInfo.{ user; name }) ->
          `Assoc [("id", `Int voter_id); ("username", `String user); ("name", `String name)])
      |> fun l -> Json.to_string (`List l)
      |> Dream.response ~status:`OK
    end

  let create_election request = let open M.Syntax in M.retract @@
    let* json = get_json_body request in
    let* election = Election.of_json_opt json
      |> M.lift_option
      |> M.on_error `Bad_Request ~message:"Election info is ill formed"
    in
    let* () = M.guard (String.length election.name > 0)
      |> M.on_error `Bad_Request ~message:"Empty election name"
    in
    let* () = M.guard (List.length election.candidates > 0)
      |> M.on_error `Bad_Request ~message:"No candidates given"
    in
    let+ () = M.guard (List.length election.voters > 0)
      |> M.on_error `Bad_Request ~message:"No voters given"
    in
    let election =
      { election with candidates =
          election.candidates
          |> List.mapi (fun i candidate -> Candidate.{ candidate with id = i }) } in
    Dream.response ~status:`OK @@ Json.to_string @@ `Int (Db.add_election !db election)

  let terminate_election request = let open M.Syntax in M.retract @@
    let* election_id = int_of_string_opt (Dream.param request "id")
      |> M.lift_option
      |> M.on_error `Bad_Request ~message:"Wrong election ID format"
    in
    let* () = M.guard (Db.election_exists !db ~election_id)
      |> M.on_error `Bad_Request ~message:"Election does not exist"
    in
    let+ () = M.guard (Db.election_is_running !db ~election_id)
      |> M.on_error `Not_Acceptable ~message:"Election is not running"
    in
    Db.terminate_election !db ~election_id;
    Dream.response ~status:`OK ""
  
  let reset_app _ =
    auth := Auth.create ();
    db := Db.create ();
    M.retract @@ M.return @@ Dream.response ~status:`OK ""
end
