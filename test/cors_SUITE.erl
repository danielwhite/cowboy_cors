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
-export([simple_wildcard_get/1]).
-export([simple_allowed_credentials_get/1]).
-export([simple_allowed_credentials_with_wildcard_origin/1]).
-export([simple_exposed_headers/1]).
-export([actual_options/1]).
-export([preflight_method/1]).
-export([preflight_allowed_method/1]).
-export([preflight_credentials/1]).
-export([preflight_wildcard_origin/1]).
-export([preflight_credentials_with_wildcard_origin/1]).
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
                            standard_options
                           ]},
     {policy, [parallel], [
                           standard_no_origin_get,
                           standard_no_origin_options,
                           standard_get,
                           standard_options,
                           simple_allowed_get,
                           simple_wildcard_get,
                           simple_allowed_credentials_get,
                           simple_allowed_credentials_with_wildcard_origin,
                           simple_exposed_headers,
                           actual_options,
                           preflight_method,
                           preflight_allowed_method,
                           preflight_credentials,
                           preflight_wildcard_origin,
                           preflight_credentials_with_wildcard_origin,
                           preflight_header,
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

build_url(Path, Options, Config) ->
    Port = ?config(port, Config),
    Params = build_params(Options),
    iolist_to_binary([<<"http://localhost:">>, integer_to_list(Port), Path, Params]).

build_params(Options) ->
    case lists:map(fun build_param/1, Options) of
        [] ->
            [];
        [["&" | First] | Rest] ->
            ["?", First, Rest]
    end.

build_param({Name, Value}) ->
    ["&", atom_to_list(Name), "=", format_option(Value)].

format_option(Bin) when is_binary(Bin) ->
    cowboy_http:urlencode(Bin);
format_option(Bool) when is_boolean(Bool) ->
    io_lib:fwrite("~p", [Bool]);
format_option(List) when is_list(List) ->
    IoList = lists:map(fun(X) -> [",", X] end, List),
    <<",", Bin/binary>> = iolist_to_binary(IoList),
    cowboy_http:urlencode(Bin).

request(Method, Headers, Options, Config) ->
    {ok, Client} = cowboy_client:init([]),
    Url = build_url(<<"/">>, Options, Config),
    ct:pal("Sending request to ~p", [Url]),
    {ok, Client2} = cowboy_client:request(Method, Url, Headers, Client),
    cowboy_client:response(Client2).

request(Method, Headers, Config) ->
    request(Method, Headers, [], Config).

%% Tests

standard_no_origin_get(Config) ->
    {ok, 204, Headers, _} = request(<<"GET">>, [], Config),
    false = lists:keyfind(<<"access-control-allow-origin">>, 1, Headers).

standard_no_origin_options(Config) ->
    {ok, 204, Headers, _} = request(<<"OPTIONS">>, [], Config),
    false = lists:keyfind(<<"access-control-allow-origin">>, 1, Headers).

standard_get(Config) ->
    Origin = <<"http://example.com">>,
    {ok, 204, Headers, _} = request(<<"GET">>, [{<<"Origin">>, Origin}], Config),
    false = lists:keyfind(<<"access-control-allow-origin">>, 1, Headers).

standard_options(Config) ->
    Origin = <<"http://example.com">>,
    {ok, 204, Headers, _} = request(<<"OPTIONS">>, [{<<"Origin">>, Origin}], Config),
    false = lists:keyfind(<<"access-control-allow-origin">>, 1, Headers).

simple_allowed_get(Config) ->
    Origin = <<"http://example.com">>,
    {ok, 204, Headers, _} =
        request(<<"GET">>,
                [{<<"Origin">>, Origin}],
                [{allowed_origins, [<<"http://example.org">>, Origin]},
                 {allowed_methods, [<<"PUT">>, <<"GET">>]}],
                Config),
    {_, Origin} = lists:keyfind(<<"access-control-allow-origin">>, 1, Headers),
    false = lists:keyfind(<<"access-control-allow-credentials">>, 1, Headers).

simple_wildcard_get(Config) ->
    Origin = <<"http://example.com">>,
    {ok, 204, Headers, _} =
        request(<<"GET">>,
                [{<<"Origin">>, Origin}],
                [{allowed_origins, "*"}],
                Config),
    {_, Origin} = lists:keyfind(<<"access-control-allow-origin">>, 1, Headers),
    false = lists:keyfind(<<"access-control-allow-credentials">>, 1, Headers).

simple_allowed_credentials_get(Config) ->
    Origin = <<"http://example.com">>,
    {ok, 204, Headers, _} =
        request(<<"GET">>,
                [{<<"Origin">>, Origin}],
                [{allowed_origins, Origin},
                 {allow_credentials, true}],
                Config),
    {_, Origin} = lists:keyfind(<<"access-control-allow-origin">>, 1, Headers),
    {_, <<"true">>} = lists:keyfind(<<"access-control-allow-credentials">>, 1, Headers).

simple_allowed_credentials_with_wildcard_origin(Config) ->
    Origin = <<"http://example.com">>,
    {ok, 204, Headers, _} =
        request(<<"GET">>,
                [{<<"Origin">>, Origin}],
                [{allowed_origins, "*"},
                 {allow_credentials, true}],
                Config),
    %% We MUST not see "*" as the value for this header if credentials are allowed.
    {_, Origin} = lists:keyfind(<<"access-control-allow-origin">>, 1, Headers),
    {_, <<"true">>} = lists:keyfind(<<"access-control-allow-credentials">>, 1, Headers).

simple_exposed_headers(Config) ->
    Origin = <<"http://example.com">>,
    Exposed = [<<"x-first">>, <<"x-second">>],
    {ok, 204, Headers, _} =
        request(<<"GET">>,
                [{<<"Origin">>, Origin}],
                [{allowed_origins, Origin},
                 {allowed_methods, <<"GET">>},
                 {exposed_headers, Exposed}],
                Config),
    {_, Origin} = lists:keyfind(<<"access-control-allow-origin">>, 1, Headers),
    {_, ExposedList} = lists:keyfind(<<"access-control-expose-headers">>, 1, Headers),
    Exposed = cowboy_http:nonempty_list(ExposedList, fun cowboy_http:token/2),
    false = lists:keyfind(<<"access-control-allow-credentials">>, 1, Headers).

actual_options(Config) ->
    %% OPTIONS request without Access-Control-Request-Method is not a pre-flight request.
    Origin = <<"http://example.com">>,
    {ok, 204, Headers, _} =
        request(<<"OPTIONS">>,
                [{<<"Origin">>, Origin}],
                [{allowed_origins, Origin}],
                Config),
    {_, Origin} = lists:keyfind(<<"access-control-allow-origin">>, 1, Headers),
    %% Ensure OPTIONS request was handled.
    {_, <<"exposed">>} = lists:keyfind(<<"x-exposed">>, 1, Headers).

preflight_method(Config) ->
    Origin = <<"http://example.com">>,
    {ok, 200, Headers, _} =
        request(<<"OPTIONS">>,
                [{<<"Origin">>, Origin},
                 {<<"Access-Control-Request-Method">>, <<"DELETE">>}],
                [{allowed_origins, Origin}],
                Config),
    false = lists:keyfind(<<"access-control-allow-origin">>, 1, Headers),
    false = lists:keyfind(<<"access-control-allow-methods">>, 1, Headers),
    false = lists:keyfind(<<"access-control-allow-credentials">>, 1, Headers),
    false = lists:keyfind(<<"access-control-expose-headers">>, 1, Headers),
    %% Pre-flight requests should not be completed by the handler.
    false = lists:keyfind(<<"x-exposed">>, 1, Headers).

preflight_allowed_method(Config) ->
    Origin = <<"http://example.com">>,
    {ok, 200, Headers, _} =
        request(<<"OPTIONS">>,
                [{<<"Origin">>, Origin},
                 {<<"Access-Control-Request-Method">>, <<"PUT">>}],
                [{allowed_origins, Origin},
                 {allowed_methods, [<<"GET">>, <<"PUT">>]}],
                Config),
    {_, Origin} = lists:keyfind(<<"access-control-allow-origin">>, 1, Headers),
    {_, <<"PUT">>} = lists:keyfind(<<"access-control-allow-methods">>, 1, Headers),
    false = lists:keyfind(<<"access-control-allow-credentials">>, 1, Headers),
    false = lists:keyfind(<<"access-control-expose-headers">>, 1, Headers),
    %% Pre-flight requests should not be completed by the handler.
    false = lists:keyfind(<<"x-exposed">>, 1, Headers).

preflight_credentials(Config) ->
    Origin = <<"http://example.com">>,
    {ok, 200, Headers, _} =
        request(<<"OPTIONS">>,
                [{<<"Origin">>, Origin},
                 {<<"Access-Control-Request-Method">>, <<"PUT">>}],
                [{allowed_origins, Origin},
                 {allowed_methods, <<"PUT">>},
                 {allow_credentials, true}],
                Config),
    {_, Origin} = lists:keyfind(<<"access-control-allow-origin">>, 1, Headers),
    {_, <<"PUT">>} = lists:keyfind(<<"access-control-allow-methods">>, 1, Headers),
    {_, <<"true">>} = lists:keyfind(<<"access-control-allow-credentials">>, 1, Headers),
    false = lists:keyfind(<<"access-control-expose-headers">>, 1, Headers),
    %% Pre-flight requests should not be completed by the handler.
    false = lists:keyfind(<<"x-exposed">>, 1, Headers).

preflight_wildcard_origin(Config) ->
    Origin = <<"http://example.com">>,
    {ok, 200, Headers, _} =
        request(<<"OPTIONS">>,
                [{<<"Origin">>, Origin},
                 {<<"Access-Control-Request-Method">>, <<"PUT">>}],
                [{allowed_origins, "*"},
                 {allowed_methods, <<"PUT">>}],
                Config),
    {_, Origin} = lists:keyfind(<<"access-control-allow-origin">>, 1, Headers),
    {_, <<"PUT">>} = lists:keyfind(<<"access-control-allow-methods">>, 1, Headers),
    false = lists:keyfind(<<"access-control-allow-credentials">>, 1, Headers),
    false = lists:keyfind(<<"access-control-expose-headers">>, 1, Headers),
    false = lists:keyfind(<<"access-control-max-age">>, 1, Headers),
    %% Pre-flight requests should not be completed by the handler.
    false = lists:keyfind(<<"x-exposed">>, 1, Headers).

preflight_credentials_with_wildcard_origin(Config) ->
    Origin = <<"http://example.com">>,
    {ok, 200, Headers, _} =
        request(<<"OPTIONS">>,
                [{<<"Origin">>, Origin},
                 {<<"Access-Control-Request-Method">>, <<"PUT">>}],
                [{allowed_origins, "*"},
                 {allow_credentials, true},
                 {allowed_methods, <<"PUT">>}],
                Config),
    {_, Origin} = lists:keyfind(<<"access-control-allow-origin">>, 1, Headers),
    {_, <<"PUT">>} = lists:keyfind(<<"access-control-allow-methods">>, 1, Headers),
    {_, <<"true">>} = lists:keyfind(<<"access-control-allow-credentials">>, 1, Headers),
    false = lists:keyfind(<<"access-control-expose-headers">>, 1, Headers),
    false = lists:keyfind(<<"access-control-max-age">>, 1, Headers),
    %% Pre-flight requests should not be completed by the handler.
    false = lists:keyfind(<<"x-exposed">>, 1, Headers).

preflight_header(Config) ->
    Origin = <<"http://example.com">>,
    {ok, 200, Headers, _} =
        request(<<"OPTIONS">>,
                [{<<"Origin">>, Origin},
                 {<<"Access-Control-Request-Method">>, <<"PUT">>},
                 {<<"Access-Control-Request-Headers">>, <<"X-Custom">>}],
                [{allowed_origins, Origin},
                 {allowed_methods, <<"PUT">>},
                 {allowed_headers, [<<"x-unused">>, <<"x-also-unused">>]}],
                Config),
    false = lists:keyfind(<<"access-control-allow-origin">>, 1, Headers),
    false = lists:keyfind(<<"access-control-allow-methods">>, 1, Headers),
    false = lists:keyfind(<<"access-control-allow-headers">>, 1, Headers),
    false = lists:keyfind(<<"access-control-allow-credentials">>, 1, Headers),
    false = lists:keyfind(<<"access-control-expose-headers">>, 1, Headers),
    %% Pre-flight requests should not be completed by the handler.
    false = lists:keyfind(<<"x-exposed">>, 1, Headers).

preflight_allowed_header(Config) ->
    Origin = <<"http://example.com">>,
    {ok, 200, Headers, _} =
        request(<<"OPTIONS">>,
                [{<<"Origin">>, Origin},
                 {<<"Access-Control-Request-Method">>, <<"PUT">>},
                 {<<"Access-Control-Request-Headers">>, <<"X-Requested">>}],
                [{allowed_origins, Origin},
                 {allowed_methods, <<"PUT">>},
                 {allowed_headers, [<<"x-allowed">>, <<"x-requested">>]}],
                Config),
    {_, Origin} = lists:keyfind(<<"access-control-allow-origin">>, 1, Headers),
    {_, <<"PUT">>} = lists:keyfind(<<"access-control-allow-methods">>, 1, Headers),
    {_, <<"X-Requested">>} = lists:keyfind(<<"access-control-allow-headers">>, 1, Headers),
    false = lists:keyfind(<<"access-control-allow-credentials">>, 1, Headers),
    false = lists:keyfind(<<"access-control-expose-headers">>, 1, Headers),
    %% Pre-flight requests should not be completed by the handler.
    false = lists:keyfind(<<"x-exposed">>, 1, Headers).

%% Test for Webkit browsers requesting 'Origin' header.
preflight_allowed_header_webkit(Config) ->
    Origin = <<"http://example.com">>,
    {ok, 200, Headers, _} =
        request(<<"OPTIONS">>,
                [{<<"Origin">>, Origin},
                 {<<"Access-Control-Request-Method">>, <<"PUT">>},
                 {<<"Access-Control-Request-Headers">>, <<"origin, x-requested">>}],
                [{allowed_origins, Origin},
                 {allowed_methods, <<"PUT">>},
                 {allowed_headers, [<<"x-allowed">>, <<"x-requested">>]}],
                Config),
    {_, Origin} = lists:keyfind(<<"access-control-allow-origin">>, 1, Headers),
    {_, <<"PUT">>} = lists:keyfind(<<"access-control-allow-methods">>, 1, Headers),
    {_, <<"origin, x-requested">>} = lists:keyfind(<<"access-control-allow-headers">>, 1, Headers),
    false = lists:keyfind(<<"access-control-allow-credentials">>, 1, Headers),
    false = lists:keyfind(<<"access-control-expose-headers">>, 1, Headers),
    %% Pre-flight requests should not be completed by the handler.
    false = lists:keyfind(<<"x-exposed">>, 1, Headers).

