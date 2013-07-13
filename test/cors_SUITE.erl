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
-export([standard_no_origin_get/1]).
-export([standard_no_origin_options/1]).
-export([standard_get/1]).
-export([standard_options/1]).
-export([simple_allowed_get/1]).
-export([simple_allowed_credentials_get/1]).
-export([preflight_method/1]).
-export([preflight_no_method/1]).
-export([preflight_allowed_method/1]).
-export([preflight_credentials/1]).
-export([preflight_header/1]).
-export([preflight_allowed_header/1]).
-export([preflight_allowed_header_webkit/1]).

all() ->
    [
     {group, default},
     {group, policy}
    ].

groups() ->
    [
     {default, [parallel], [
                            standard_no_origin_get,
                            standard_no_origin_options,
                            standard_get,
                            standard_options,
                            preflight_method,
                            preflight_header
                           ]},
     {policy, [parallel], [
                           standard_no_origin_get,
                           standard_no_origin_options,
                           standard_get,
                           standard_options,
                           simple_allowed_get,
                           simple_allowed_credentials_get,
                           preflight_method,
                           preflight_no_method,
                           preflight_allowed_method,
                           preflight_credentials,
                           preflight_allowed_header,
                           preflight_allowed_header_webkit
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

preflight(Headers, Config) ->
    request(<<"OPTIONS">>, Headers, Config).

%% Tests

standard_no_origin_get(Config) ->
    {ok, 200, Headers, _} = request(<<"GET">>, [], Config),
    false = lists:keyfind(<<"access-control-allow-origin">>, 1, Headers).

standard_no_origin_options(Config) ->
    {ok, 200, Headers, _} = request(<<"OPTIONS">>, [], Config),
    false = lists:keyfind(<<"access-control-allow-origin">>, 1, Headers).

standard_get(Config) ->
    Origin = <<"http://banned.example.com">>,
    {ok, 200, Headers, _} = request(<<"GET">>, [{<<"Origin">>, Origin}], Config),
    false = lists:keyfind(<<"access-control-allow-origin">>, 1, Headers).

standard_options(Config) ->
    Origin = <<"http://banned.example.com">>,
    {ok, 200, Headers, _} = request(<<"OPTIONS">>, [{<<"Origin">>, Origin}], Config),
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

preflight_no_method(Config) ->
    Origin = <<"http://allowed.example.com">>,
    {ok, 200, Headers, _} =
        preflight([{<<"Origin">>, Origin}], Config),
    false = lists:keyfind(<<"access-control-allow-origin">>, 1, Headers),
    false = lists:keyfind(<<"access-control-allow-methods">>, 1, Headers),
    false = lists:keyfind(<<"access-control-allow-credentials">>, 1, Headers),
    false = lists:keyfind(<<"access-control-expose-headers">>, 1, Headers).

preflight_method(Config) ->
    Origin = <<"http://allowed.example.com">>,
    {ok, 200, Headers, _} =
        preflight([{<<"Origin">>, Origin}, {<<"Access-Control-Request-Method">>, <<"DELETE">>}], Config),
    false = lists:keyfind(<<"access-control-allow-origin">>, 1, Headers),
    false = lists:keyfind(<<"access-control-allow-methods">>, 1, Headers),
    false = lists:keyfind(<<"access-control-allow-credentials">>, 1, Headers),
    false = lists:keyfind(<<"access-control-expose-headers">>, 1, Headers).

preflight_allowed_method(Config) ->
    Origin = <<"http://allowed.example.com">>,
    {ok, 200, Headers, _} =
        preflight([{<<"Origin">>, Origin}, {<<"Access-Control-Request-Method">>, <<"PUT">>}], Config),
    {_, Origin} = lists:keyfind(<<"access-control-allow-origin">>, 1, Headers),
    {_, <<"PUT">>} = lists:keyfind(<<"access-control-allow-methods">>, 1, Headers),
    false = lists:keyfind(<<"access-control-allow-credentials">>, 1, Headers),
    false = lists:keyfind(<<"access-control-expose-headers">>, 1, Headers).

preflight_credentials(Config) ->
    Origin = <<"http://credentials.example.com">>,
    {ok, 200, Headers, _} =
        preflight([{<<"Origin">>, Origin}, {<<"Access-Control-Request-Method">>, <<"PUT">>}], Config),
    {_, Origin} = lists:keyfind(<<"access-control-allow-origin">>, 1, Headers),
    {_, <<"PUT">>} = lists:keyfind(<<"access-control-allow-methods">>, 1, Headers),
    {_, <<"true">>} = lists:keyfind(<<"access-control-allow-credentials">>, 1, Headers),
    false = lists:keyfind(<<"access-control-expose-headers">>, 1, Headers).

preflight_header(Config) ->
    Origin = <<"http://allowed.example.com">>,
    {ok, 200, Headers, _} =
        preflight([
                   {<<"Origin">>, Origin},
                   {<<"Access-Control-Request-Method">>, <<"PUT">>},
                   {<<"Access-Control-Request-Headers">>, <<"X-Custom">>}
                  ], Config),
    false = lists:keyfind(<<"access-control-allow-origin">>, 1, Headers),
    false = lists:keyfind(<<"access-control-allow-methods">>, 1, Headers),
    false = lists:keyfind(<<"access-control-allow-headers">>, 1, Headers),
    false = lists:keyfind(<<"access-control-allow-credentials">>, 1, Headers),
    false = lists:keyfind(<<"access-control-expose-headers">>, 1, Headers).

preflight_allowed_header(Config) ->
    Origin = <<"http://allowed.example.com">>,
    {ok, 200, Headers, _} =
        preflight([
                   {<<"Origin">>, Origin},
                   {<<"Access-Control-Request-Method">>, <<"PUT">>},
                   {<<"Access-Control-Request-Headers">>, <<"X-Requested">>}
                  ], Config),
    {_, Origin} = lists:keyfind(<<"access-control-allow-origin">>, 1, Headers),
    {_, <<"PUT">>} = lists:keyfind(<<"access-control-allow-methods">>, 1, Headers),
    {_, <<"X-Requested">>} = lists:keyfind(<<"access-control-allow-headers">>, 1, Headers),
    false = lists:keyfind(<<"access-control-allow-credentials">>, 1, Headers),
    false = lists:keyfind(<<"access-control-expose-headers">>, 1, Headers).

%% Test for Webkit browsers requesting 'Origin' header.
preflight_allowed_header_webkit(Config) ->
    Origin = <<"http://allowed.example.com">>,
    {ok, 200, Headers, _} =
        preflight([
                   {<<"Origin">>, Origin},
                   {<<"Access-Control-Request-Method">>, <<"PUT">>},
                   {<<"Access-Control-Request-Headers">>, <<"origin, x-requested">>}
                  ], Config),
    {_, Origin} = lists:keyfind(<<"access-control-allow-origin">>, 1, Headers),
    {_, <<"PUT">>} = lists:keyfind(<<"access-control-allow-methods">>, 1, Headers),
    {_, <<"origin, x-requested">>} = lists:keyfind(<<"access-control-allow-headers">>, 1, Headers),
    false = lists:keyfind(<<"access-control-allow-credentials">>, 1, Headers),
    false = lists:keyfind(<<"access-control-expose-headers">>, 1, Headers).
