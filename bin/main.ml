open Major

module M = App_utils

let parse_args () =
  let test = ref false in
  Arg.parse
    [ ("--test", Set test, " Run the server in test mode");
      ("-help", Unit Fun.id, "") ]
    (fun _ -> ())
    "major [--test]";
  !test

let parse_admin_credentials () = let (let*) = Option.bind in let (>>=) = Option.bind in
  let* port = Sys.getenv_opt "PORT" >>= int_of_string_opt in
  let* admin_user = Sys.getenv_opt "ADMIN_USER" in
  let* admin_password = Sys.getenv_opt "ADMIN_PASSWORD" in
  Some (port, admin_user, admin_password)
  
let () =
  let test = parse_args () in
  let (port, admin_user, admin_password) =
    Option.value (parse_admin_credentials ())
      ~default:(failwith "no valid port, admin user and admin password found in environment")
  in
  if port < 1024 || port > 63335 then
    failwith (Printf.sprintf "invalid port: %d" port);
  if test then
    let module App = App.Make(struct
      module Auth = Auth_service.Default(struct
        let admin_credentials = (admin_user, admin_password)
      end)
      module Db = Database_service.Default(struct let admin_username = admin_user end)
    end) in
    App.start { port; test }
  else
    ()
