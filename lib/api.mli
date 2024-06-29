module Make : functor (_ : Services.S) -> sig
  val create_session : Dream.request -> Dream.response Lwt.t
  val refresh_session : Dream.request -> Dream.response Lwt.t
  val delete_session : Dream.request -> Dream.response Lwt.t
  val whoami : Dream.request -> Dream.response Lwt.t
end
