module Make (Services : Services.S) = struct
  module Api = Api.Make(Services)

  type config = {
    port : int
  }

  let start { port } =
    Random.self_init ();
    Dream.run ~port
    @@ Dream.logger
    @@ Dream.router [
      Dream.scope "/v1" [] [
        (* authentication endpoints *)
        Dream.post "/user" Api.create_user;
        Dream.get "/session" Api.create_session;
        Dream.patch "/session/refresh" Api.refresh_session;
        Dream.delete "/session" Api.delete_session;

        (* app endpoints *)
        Dream.scope "/" [Api.bearer_middleware] [
          Dream.get "/whoami" Api.whoami;
          Dream.get "/elections" Api.get_elections;
          Dream.get "/elections/:id" Api.get_election_details;
          Dream.post "/elections/:id" Api.vote;
          Dream.get "/elections/:id/results" Api.get_election_results
        ];

        (* admin endpoints *)
        Dream.scope "/" [Api.admin_middleware] [
          Dream.get "/users" Api.get_users;
          Dream.post "/elections" Api.create_election;
          Dream.patch "/elections/:id" Api.terminate_election;
        ]
      ]
    ]
end
