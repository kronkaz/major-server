open Major

let () =
  let module App = App.Make(struct
    module Auth = Memory_auth
  end) in
  App.start ()
