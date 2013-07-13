-module(cowboy_cors_policy).

-type state() :: any().

-callback policy_init(Req)
        -> {ok, Req, state()}
        | {shutdown, Req}
        when Req :: cowboy_req:req().
