open Major

let () =
  let module App = App.Make(struct
    module Auth = Auth_service.Default
    module Db = Database_service.Default
  end) in
  App.start ()
