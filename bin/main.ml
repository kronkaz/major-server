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

let () =
  let test, port = parse_args () in
  if test then
    let module App = App.Make(struct
      module Auth = Auth_service.Default
      module Db = Database_service.Default
    end) in
    App.start { port }
  else
    ()
