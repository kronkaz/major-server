include Lwt_result
let lift_result = lift

let lift_lwt ?(message = "") lwt = Lwt.catch (fun () -> ok lwt) (fun _ -> fail message)

let lift_option ?(message = "") = Option.fold ~some:return ~none:(fail message)

let on_error ?message status = map_error @@ fun message' ->
  Dream.response ~status (Option.value message ~default:message')

let guard ?(message = "") test = if test then Lwt.return_ok () else fail message

let retract m = Lwt.map (Result.fold ~ok:Fun.id ~error:Fun.id) m