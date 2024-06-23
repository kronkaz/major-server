module Make (Services : Services.S) = struct
  module Api = Api.Make(Services)

  let start () =
    Random.self_init ();
    Dream.run
    @@ Dream.logger
    @@ Dream.router [
      Dream.scope "/v1" [] [
        Dream.get "/token" Api.get_token_handler;
      ]
    ]
end
