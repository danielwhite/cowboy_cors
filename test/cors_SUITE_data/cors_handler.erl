-module(cors_handler).
-behaviour(cowboy_http_handler).

-export([init/3, handle/2, terminate/3]).

init(_Transport, Req, _Opts) ->
    {ok, Req, undefined_state}.

handle(Req, State) ->
    Headers = [{<<"X-Exposed">>, <<"exposed">>}, {<<"X-Hidden">>, <<"hidden">>}],
    {ok, Req1} = cowboy_req:reply(200, Headers, <<"ok">>, Req),
    {ok, Req1, State}.

terminate(_Reason, _Req, _State) ->
    ok.
