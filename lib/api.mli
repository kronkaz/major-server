module Make : functor (_ : Services.S) -> sig
  val bearer_middleware : Dream.middleware
  val admin_middleware : Dream.middleware

  val create_user : Dream.request -> Dream.response Lwt.t
  val create_session : Dream.request -> Dream.response Lwt.t
  val refresh_session : Dream.request -> Dream.response Lwt.t
  val delete_session : Dream.request -> Dream.response Lwt.t
  val whoami : Dream.request -> Dream.response Lwt.t
  val get_elections : Dream.request -> Dream.response Lwt.t
  val get_election_details : Dream.request -> Dream.response Lwt.t
  val vote : Dream.request -> Dream.response Lwt.t
  val get_users : Dream.request -> Dream.response Lwt.t
  val create_election : Dream.request -> Dream.response Lwt.t
  val terminate_election : Dream.request -> Dream.response Lwt.t
  val get_election_results : Dream.request -> Dream.response Lwt.t
end
