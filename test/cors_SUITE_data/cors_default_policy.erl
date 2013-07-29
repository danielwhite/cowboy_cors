-module(cors_default_policy).
-behaviour(cowboy_cors_policy).

-export([policy_init/1]).

policy_init(Req) ->
    {ok, Req, undefined_state}.
