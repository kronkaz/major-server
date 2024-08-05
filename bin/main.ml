open Major

module M = App_utils

let parse_args () =
  let test = ref false in
  let port = ref (-1) in
  Arg.parse
    [ ("--test", Set test, " Run the server in test mode");
      ("--port", Set_int port, " Open the server on the given port");
      ("-help", Unit Fun.id, "") ]
    (fun _ -> ())
    "major --port PORT [--test]";
  if !port = -1 then failwith "expected --port";
  if !port < 1024 || !port > 63335 then
    failwith (Printf.sprintf "invalid port: %d" !port);
  (!test, !port)

let parse_admin_credentials () =
  match Sys.getenv_opt "ADMIN_USER", Sys.getenv_opt "ADMIN_PASSWORD" with
  | Some user, Some password -> (user, password)
  | _ -> failwith "expected admin user and password as environment variables"

let () =
  let test, port = parse_args () in
  let admin_credentials = parse_admin_credentials () in
  if test then
    let module App = App.Make(struct
      module Auth = Auth_service.Default(struct let admin_credentials = admin_credentials end)
      module Db = Database_service.Default(struct let admin_username = fst admin_credentials end)
    end) in
    App.start { port }
  else
    ()
