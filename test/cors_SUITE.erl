-module(cors_SUITE).

-include_lib("common_test/include/ct.hrl").

%% ct callbacks
-export([all/0]).
-export([groups/0]).
-export([init_per_suite/1]).
-export([end_per_suite/1]).
-export([init_per_group/2]).
-export([end_per_group/2]).

%% tests
-export([simple_no_origin_get/1]).
-export([simple_get/1]).
-export([simple_allowed_get/1]).
-export([simple_allowed_credentials_get/1]).

all() ->
    [
     {group, default},
     {group, policy}
    ].

groups() ->
    [
     {default, [parallel], [
                            simple_no_origin_get,
                            simple_get
                           ]},
     {policy, [parallel], [
                           simple_no_origin_get,
                           simple_get,
                           simple_allowed_get,
                           simple_allowed_credentials_get
                          ]}
    ].

init_per_suite(Config) ->
    application:start(crypto),
    application:start(ranch),
    application:start(cowboy),
    Config.

end_per_suite(_Config) ->
    application:stop(cowboy),
    application:stop(ranch),
    application:stop(crypto),
    ok.

init_per_group(default = Name, Config) ->
    Middlewares = [cowboy_cors, cowboy_handler],
    Env = [{handler, cors_handler},
           {handler_opts, []},
           {cors_policy, cors_default_policy}],
    {ok, _} = cowboy:start_http(Name, 100, [{port, 0}], [{env, Env}, {middlewares, Middlewares}]),
    Port = ranch:get_port(Name),
    [{port, Port} | Config];
init_per_group(policy = Name, Config) ->
    Middlewares = [cowboy_cors, cowboy_handler],
    Env = [{handler, cors_handler},
           {handler_opts, []},
           {cors_policy, cors_policy}],
    {ok, _} = cowboy:start_http(Name, 100, [{port, 0}], [{env, Env}, {middlewares, Middlewares}]),
    Port = ranch:get_port(Name),
    [{port, Port} | Config].

end_per_group(Name, _) ->
    cowboy:stop_listener(Name),
    ok.

%% Helpers

build_url(Path, Config) ->
    Port = ?config(port, Config),
    iolist_to_binary([<<"http://localhost:">>, integer_to_list(Port), Path]).

request(Method, Headers, Config) ->
    {ok, Client} = cowboy_client:init([]),
    {ok, Client2} = cowboy_client:request(Method, build_url(<<"/">>, Config), Headers, Client),
    cowboy_client:response(Client2).

%% Tests

simple_no_origin_get(Config) ->
    {ok, 200, Headers, _} = request(<<"GET">>, [], Config),
    false = lists:keyfind(<<"access-control-allow-origin">>, 1, Headers).

simple_get(Config) ->
    Origin = <<"http://banned.example.com">>,
    {ok, 200, Headers, _} = request(<<"GET">>, [{<<"Origin">>, Origin}], Config),
    false = lists:keyfind(<<"access-control-allow-origin">>, 1, Headers).

simple_allowed_get(Config) ->
    Origin = <<"http://allowed.example.com">>,
    {ok, 200, Headers, _} =
        request(<<"GET">>, [{<<"Origin">>, Origin}], Config),
    {_, Origin} = lists:keyfind(<<"access-control-allow-origin">>, 1, Headers),
    {_, <<"x-exposed">>} = lists:keyfind(<<"access-control-expose-headers">>, 1, Headers),
    false = lists:keyfind(<<"access-control-allow-credentials">>, 1, Headers).

simple_allowed_credentials_get(Config) ->
    Origin = <<"http://credentials.example.com">>,
    {ok, 200, Headers, _} =
        request(<<"GET">>, [{<<"Origin">>, Origin}], Config),
    {_, Origin} = lists:keyfind(<<"access-control-allow-origin">>, 1, Headers),
    {_, <<"x-exposed">>} = lists:keyfind(<<"access-control-expose-headers">>, 1, Headers),
    {_, <<"true">>} = lists:keyfind(<<"access-control-allow-credentials">>, 1, Headers).
