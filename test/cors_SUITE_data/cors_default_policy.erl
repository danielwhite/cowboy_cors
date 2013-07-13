-module(cors_default_policy).
-behaviour(cowboy_cors_policy).

-export([cors_policy_init/1]).

cors_policy_init(Req) ->
    {ok, Req, undefined_state}.
