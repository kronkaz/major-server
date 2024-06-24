open Major

let () =
  let module App = App.Make(struct
    module Auth = Memory_auth
    module Db = Memory_database
  end) in
  App.start ()
