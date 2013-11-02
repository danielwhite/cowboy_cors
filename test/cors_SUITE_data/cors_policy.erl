-module(cors_policy).
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
    case parse_list(<<"allowed_origins">>, Req) of
        {[<<"*">>], Req1} ->
            {'*', Req1, State};
        {Allowed, Req1} ->
            {Allowed, Req1, State}
    end.

allow_credentials(Req, State) ->
    {IsAllowed, Req1} = parse_boolean(<<"allow_credentials">>, Req, false),
    {IsAllowed, Req1, State}.

exposed_headers(Req, State) ->
    {Exposed, Req1} = parse_list(<<"exposed_headers">>, Req),
    {Exposed, Req1, State}.

allowed_headers(Req, State) ->
    {Allowed, Req1} = parse_list(<<"allowed_headers">>, Req),
    {Allowed, Req1, State}.

allowed_methods(Req, State) ->
    {Allowed, Req1} = parse_list(<<"allowed_methods">>, Req),
    {Allowed, Req1, State}.

parse_list(Name, Req) ->
    case cowboy_req:qs_val(Name, Req) of
        {undefined, Req1} ->
            {[], Req1};
        {Bin, Req1} ->
            List = binary:split(Bin, <<",">>, [global]),
            {List, Req1}
    end.

parse_boolean(Name, Req, Default) ->
    case cowboy_req:qs_val(Name, Req) of
        {undefined, Req1} ->
            {Default, Req1};
        {<<"true">>, Req1} ->
            {true, Req1};
        {<<"false">>, Req1} ->
            {false, Req1}
    end.
