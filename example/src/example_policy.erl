-module(example_policy).
-behaviour(cowboy_cors_policy).

-export([policy_init/1]).
-export([allowed_origins/2]).
-export([allow_credentials/2]).
-export([exposed_headers/2]).
-export([allowed_headers/2]).
-export([allowed_methods/2]).

policy_init(Req) ->
    {ok, Req, undefined_state}.

allowed_origins(Req, State) ->
    {[<<"http://client.cors-api.appspot.com">>], Req, State}.

allow_credentials(Req, State) ->
    {true, Req, State}.

exposed_headers(Req, State) ->
    {[<<"x-exposed">>], Req, State}.

allowed_headers(Req, State) ->
    {[<<"x-requested">>], Req, State}.

allowed_methods(Req, State) ->
    {[<<"GET">>, <<"POST">>, <<"PUT">>], Req, State}.
