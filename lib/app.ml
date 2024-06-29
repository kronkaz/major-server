module Make (Services : Services.S) = struct
  module Api = Api.Make(Services)

  let start () =
    Random.self_init ();
    Dream.run
    @@ Dream.logger
    @@ Dream.router [
      Dream.scope "/v1" [] [
        Dream.get "/session" Api.create_session;
        Dream.patch "/session/refresh" Api.refresh_session;
        Dream.delete "/session" Api.delete_session;
        Dream.get "/whoami" Api.whoami
      ]
    ]
end
