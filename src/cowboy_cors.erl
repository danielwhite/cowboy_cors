%% @doc Cross-Origin Resource Sharing (CORS) middleware.
%%
%% Policy is defined through callbacks contained in a module named by
%% the <em>cors_policy</em> environment value.
%%
%% @see http://www.w3.org/TR/cors/
-module(cowboy_cors).
-behaviour(cowboy_middleware).

-export([execute/2]).

-record(state, {
          env                  :: cowboy_middleware:env(),
          origin               :: binary(),
          request_method       :: binary(),
          request_headers = [] :: [binary()],

          %% Policy handler.
          policy               :: atom(),
          policy_state         :: any()
}).

%% @private
execute(Req, Env) ->
    {_, Policy} = lists:keyfind(cors_policy, 1, Env),
    origin_present(Req, #state{env = Env, policy = Policy}).

%% CORS specification only applies to requests with an `Origin' header.
origin_present(Req, State) ->
    case cowboy_req:header(<<"origin">>, Req) of
        {undefined, Req1} ->
            terminate(Req1, State);
        {Origin, Req1} ->
            policy_init(Req1, State#state{origin = Origin})
    end.

policy_init(Req, State = #state{policy = Policy}) ->
    try Policy:policy_init(Req) of
        {ok, Req1, PolicyState} ->
            allowed_origins(Req1, State#state{policy_state = PolicyState})
    catch Class:Reason ->
                error_logger:error_msg(
                  "** Cowboy CORS policy ~p terminating in ~p/~p~n"
                  "   for the reason ~p:~p~n"
                  "** Request was ~p~n** Stacktrace: ~p~n~n",
                  [Policy, policy_init, 1, Class, Reason,
                   cowboy_req:to_list(Req), erlang:get_stacktrace()]),
            error_terminate(Req, State)
    end.

allowed_origins(Req, State = #state{origin = Origin}) ->
    {List, Req1, PolicyState} = call(Req, State, allowed_origins, []),
    case lists:member(Origin, List) of
        true ->
            %% both models are identical prior to this point
            choose_processing_model(Req1, State#state{policy_state = PolicyState});
        false ->
            terminate(Req, State#state{policy_state = PolicyState})
    end.

%% select either simple or preflight request processing model
choose_processing_model(Req, State) ->
    case cowboy_req:method(Req) of
        {<<"OPTIONS">>, Req1} ->
            preflight_model(Req1, State);
        {_Method, Req1} ->
            simple_model(Req1, State)
    end.

preflight_model(Req, State) ->
    request_method(Req, State).

simple_model(Req, State) ->
    {Req1, State1} = allow_credentials(Req, State),
    exposed_headers(Req1, State1).

request_method(Req, State) ->
    case cowboy_req:header(<<"access-control-request-method">>, Req) of
        {undefined, Req1} ->
            terminate(Req1, State);
        {Data, Req1} ->
            cowboy_http:token(Data,
                              fun(<<>>, Method) ->
                                      request_headers(Req1, State#state{request_method = Method});
                                 (_, _) ->
                                      terminate(Req1, State)
                              end)
    end.

request_headers(Req, State) ->
    {Headers, Req1} = cowboy_req:header(<<"access-control-request-headers">>, Req, <<>>),
    case cowboy_http:list(Headers, fun cowboy_http:token_ci/2) of
        {error, badarg} ->
            terminate(Req1, State);
        List ->
            allowed_methods(Req1, State#state{request_headers = List})
    end.

%% allow_methods/2 should return a list of binary method names
allowed_methods(Req, State = #state{request_method = Method}) ->
    {List, Req1, PolicyState} = call(Req, State, allowed_methods, []),
    case lists:member(Method, List) of
        false ->
            terminate(Req1, State#state{policy_state = PolicyState});
        true ->
            allowed_headers(Req1, State#state{policy_state = PolicyState})
    end.

allowed_headers(Req, State = #state{request_headers = Requested}) ->
    {List, Req1, PolicyState} = call(Req, State, allowed_headers, []),
    check_allowed_headers(Requested, List, Req1, State#state{policy_state = PolicyState}).

check_allowed_headers([], _, Req, State) ->
    set_preflight_headers(Req, State);
check_allowed_headers([<<"origin">>|Tail], Allowed, Req, State) ->
    %% KLUDGE: for browsers that include this header, but don't
    %% actually check it (i.e. Webkit).  Given that the 'Origin'
    %% header underpins the entire CORS framework, its inclusion in
    %% the requested headers is nonsensical.
    check_allowed_headers(Tail, Allowed, Req, State);
check_allowed_headers([Header|Tail], Allowed, Req, State) ->
    case lists:member(Header, Allowed) of
        false ->
            terminate(Req, State);
        true ->
            check_allowed_headers(Tail, Allowed, Req, State)
    end.

set_preflight_headers(Req, State) ->
    {Req1, State1} = allow_credentials(Req, State),
    set_allow_methods(Req1, State1).

set_allow_methods(Req, State = #state{request_method = Method}) ->
    Req1 = cowboy_req:set_resp_header(<<"access-control-allow-methods">>, Method, Req),
    set_allow_headers(Req1, State).

set_allow_headers(Req, State) ->
    %% Since we have already validated the requested headers, we can
    %% simply reflect the list back to the client.
    case cowboy_req:header(<<"access-control-request-headers">>, Req) of
        {undefined, Req1} ->
            terminate(Req1, State);
        {Headers, Req1} ->
            Req2 = cowboy_req:set_resp_header(<<"access-control-allow-headers">>, Headers, Req1),
            terminate(Req2, State)
    end.

%% allow_credentials/1 should return true or false.
allow_credentials(Req, State) ->
    expect(Req, State, allow_credentials, false,
           fun if_not_allow_credentials/2, fun if_allow_credentials/2).

%% If credentials are allowed, then the value of
%% `Access-Control-Allow-Origin' is limited to the requesting origin.
if_allow_credentials(Req, State = #state{origin = Origin}) ->
    Req1 = cowboy_req:set_resp_header(<<"access-control-allow-origin">>, Origin, Req),
    Req2 = cowboy_req:set_resp_header(<<"access-control-allow-credentials">>, <<"true">>, Req1),
    Req3 = cowboy_req:set_resp_header(<<"vary">>, <<"origin">>, Req2),
    {Req3, State}.

if_not_allow_credentials(Req, State = #state{origin = Origin}) ->
    Req1 = cowboy_req:set_resp_header(<<"access-control-allow-origin">>, Origin, Req),
    Req2 = cowboy_req:set_resp_header(<<"vary">>, <<"origin">>, Req1),
    {Req2, State}.

%% exposed_headers/2 should return a list of binary header names.
exposed_headers(Req, State) ->
    case call(Req, State, exposed_headers) of
        no_call ->
            terminate(Req, State);
        {List, Req1, PolicyState} ->
            Req2 = set_exposed_headers(Req1, List),
            terminate(Req2, State#state{policy_state = PolicyState})
    end.

set_exposed_headers(Req, []) ->
    Req;
set_exposed_headers(Req, [Header|Tail]) ->
    Req1 = cowboy_req:set_resp_header(<<"access-control-expose-headers">>, Header, Req),
    set_exposed_headers(Req1, Tail).

expect(Req, State, Callback, Expected, OnTrue, OnFalse) ->
    case call(Req, State, Callback, Expected) of
        {Expected, Req1, PolicyState} ->
            OnTrue(Req1, State#state{policy_state = PolicyState});
        {_Unexpected, Req1, PolicyState} ->
            OnFalse(Req1, State#state{policy_state = PolicyState})
    end.

call(Req, State, Callback) ->
    call(Req, State, Callback, no_call).

call(Req, State = #state{policy = Policy, policy_state = PolicyState}, Callback, Default) ->
    case erlang:function_exported(Policy, Callback, 2) of
        true ->
            try
                Policy:Callback(Req, PolicyState)
            catch Class:Reason ->
                    error_logger:error_msg(
                      "** Cowboy CORS policy ~p terminating in ~p/~p~n"
                      "   for the reason ~p:~p~n"
                      "** Request was ~p~n** Stacktrace: ~p~n~n",
                      [Policy, Callback, 2, Class, Reason,
                       cowboy_req:to_list(Req), erlang:get_stacktrace()]),
                    error_terminate(Req, State)
            end;
        false ->
            {Default, Req, PolicyState}
    end.

terminate(Req, #state{env = Env}) ->
    {ok, Req, Env}.

-spec error_terminate(cowboy_req:req(), #state{}) -> no_return().
error_terminate(_Req, _State) ->
    erlang:throw({?MODULE, error}).
