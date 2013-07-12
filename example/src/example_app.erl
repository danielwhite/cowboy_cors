-module(example_app).
-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    Dispatch = cowboy_router:compile(
                 [
                  {'_', [
                         {"/[...]", example_handler, []}
                        ]}
                 ]),
    {ok, _} = cowboy:start_http(http, 100, [{port, 8080}],
                                [
                                 {env, [
                                        {dispatch, Dispatch},
                                        {cors_policy, example_policy}
                                       ]},
                                 {middlewares, [
                                                cowboy_router,
                                                cowboy_cors,
                                                cowboy_handler
                                               ]}
                                ]),
    example_sup:start_link().

stop(_State) ->
    ok.
