-module(cors_policy).
-behaviour(cowboy_cors_policy).

-export([cors_policy_init/1]).
-export([cors_allowed_origins/2]).
-export([cors_allow_credentials/2]).
-export([cors_exposed_headers/2]).
-export([cors_allowed_headers/2]).
-export([cors_allowed_methods/2]).

cors_policy_init(Req) ->
    {ok, Req, undefined_state}.

cors_allowed_origins(Req, State) ->
    Allowed = [
               <<"http://allowed.example.com">>,
               <<"http://credentials.example.com">>
              ],
    {Allowed, Req, State}.

cors_allow_credentials(Req, State) ->
    case cowboy_req:header(<<"origin">>, Req) of
        {<<"http://credentials.example.com">>, Req1} ->
            {true, Req1, State};
        {_, Req1} ->
            {false, Req1, State}
    end.

cors_exposed_headers(Req, State) ->
    {[<<"x-exposed">>], Req, State}.

cors_allowed_headers(Req, State) ->
    {[<<"x-requested">>], Req, State}.

cors_allowed_methods(Req, State) ->
    {[<<"GET">>, <<"POST">>, <<"PUT">>], Req, State}.
