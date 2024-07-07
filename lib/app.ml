module Make (Services : Services.S) = struct
  module Api = Api.Make(Services)

  let start () =
    Random.self_init ();
    Dream.run
    @@ Dream.logger
    @@ Dream.router [
      Dream.scope "/v1" [] [
        (* authentication endpoints *)
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
          Dream.patch "/elections/:id" Api.terminate_election;
        ]
      ]
    ]
end
