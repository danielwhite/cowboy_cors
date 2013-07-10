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
          env :: cowboy_middleware:env(),
          origin :: binary(),

          %% Policy handler.
          policy :: atom(),
          policy_state :: any()
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
    try Policy:cors_policy_init(Req) of
        {ok, Req1, PolicyState} ->
            allowed_origins(Req1, State#state{policy_state = PolicyState});
        {shutdown, Req1} ->
            terminate(Req1, State)
    catch Class:Reason ->
                error_logger:error_msg(
                  "** Cowboy CORS policy ~p terminating in ~p/~p~n"
                  "   for the reason ~p:~p~n"
                  "** Request was ~p~n** Stacktrace: ~p~n~n",
                  [Policy, cors_policy_init, 1, Class, Reason,
                   cowboy_req:to_list(Req), erlang:get_stacktrace()]),
            error_terminate(Req, State)
    end.

allowed_origins(Req, State = #state{origin = Origin}) ->
    case call(Req, State, cors_allowed_origins) of
        no_call ->
            terminate(Req, State);
        {List, Req1, PolicyState} ->
            case lists:member(Origin, List) of
                true ->
                    allow_credentials(Req1, State#state{policy_state = PolicyState});
                false ->
                    terminate(Req, State#state{policy_state = PolicyState})
            end
    end.

%% allow_credentials/2 should return true or false.
allow_credentials(Req, State) ->
    expect(Req, State, cors_allow_credentials, false,
           fun if_not_allow_credentials/2, fun if_allow_credentials/2).

%% If credentials are allowed, then the value of
%% `Access-Control-Allow-Origin' is limited to the requesting origin.
if_allow_credentials(Req, State = #state{origin = Origin}) ->
    Req1 = cowboy_req:set_resp_header(<<"access-control-allow-origin">>, Origin, Req),
    Req2 = cowboy_req:set_resp_header(<<"access-control-allow-credentials">>, <<"true">>, Req1),
    Req3 = cowboy_req:set_resp_header(<<"vary">>, <<"origin">>, Req2),
    exposed_headers(Req3, State).

if_not_allow_credentials(Req, State = #state{origin = Origin}) ->
    Req1 = cowboy_req:set_resp_header(<<"access-control-allow-origin">>, Origin, Req),
    Req2 = cowboy_req:set_resp_header(<<"vary">>, <<"origin">>, Req1),
    exposed_headers(Req2, State).

%% exposed_headers/2 should return a list of binary header names.
exposed_headers(Req, State) ->
    case call(Req, State, cors_exposed_headers) of
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
    case call(Req, State, Callback) of
        no_call ->
            OnTrue(Req, State);
        {Expected, Req1, PolicyState} ->
            OnTrue(Req1, State#state{policy_state = PolicyState});
        {_Unexpected, Req1, PolicyState} ->
            OnFalse(Req1, State#state{policy_state = PolicyState})
    end.

call(Req, State = #state{policy = Policy, policy_state = PolicyState}, Callback) ->
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
            no_call
    end.

terminate(Req, #state{env = Env}) ->
    {ok, Req, Env}.

-spec error_terminate(cowboy_req:req(), #state{}) -> no_return().
error_terminate(_Req, _State) ->
    erlang:throw({?MODULE, error}).
