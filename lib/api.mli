module Make : functor (_ : Services.S) -> sig
  val get_token_handler : Dream.request -> Dream.response Lwt.t
end
