%% @doc CORS Policy definition and implementation.
-module(cowboy_cors_policy).

-export([policy_init/1]).
-export([allowed_origins/2]).
-export([allow_credentials/2]).
-export([exposed_headers/2]).

%% Pre-flight specific callbacks.
-export([allowed_methods/2]).
-export([allowed_headers/2]).
-export([max_age/2]).


-type state() :: any().
-type req()   :: cowboy_req:req().

-callback policy_init(Req)
        -> {ok, Req, state()}
        when Req :: cowboy_req:req().


-spec policy_init(req()) -> {ok, req(), state()}.
policy_init(Req) ->
    {ok, Req, undefined}.

%% @doc
%%  Return whitelist origins.
%% @end
-spec allowed_origins(req(), state()) -> {'*' | [binary()], req(), state()}.
allowed_origins(Req, State) ->
    ValidOrigins = application:get_env(cowboy_cors, origin_whitelist, []),
    {ValidOrigins, Req, State}.

%% @doc
%%  Indicates which headers can be exposed as part of the response.
%%  Headers are read from configuration.
%% @end
-spec exposed_headers(req(), state()) -> {[binary()], req(), state()}.
exposed_headers(Req, State) ->
    ExposedHeaders = application:get_env(cowboy_cors, exposed_headers, []),
    {ExposedHeaders, Req, State}.

%% @doc
%%  Returns headers that are allowed to be passed in a pre-flight request.
%% @end
-spec allowed_headers(req(), state()) -> {[binary()], req(), state()}.
allowed_headers(Req, State) ->
    AllowedHeaders = application:get_env(cowboy_cors, allowed_headers, []),
    {AllowedHeaders, Req, State}.

-spec allow_credentials(req(), state()) -> {boolean(), req(), state()}.
allow_credentials(Req, State) ->
    Credential = application:get_env(cowboy_cors, allowed_credential, false),
    {Credential, Req, State}.

-spec allowed_methods(req(), state()) -> {[binary()], req(), state()}.
allowed_methods(Req, State) ->
    AllowedMethods = application:get_env(cowboy_cors, allowed_methods, []),
    {AllowedMethods, Req, State}.

-spec max_age(req(), state()) ->
                     {non_neg_integer() | undefined, req(), state()}.
max_age(Req, State) ->
    MaxAgeSecs = application:get_env(cowboy_cors, max_age, undefined),
    {MaxAgeSecs, Req, State}.
