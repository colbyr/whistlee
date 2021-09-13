%%%-------------------------------------------------------------------
%% @doc whistlee public API
%% @end
%%%-------------------------------------------------------------------

-module(whistlee_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    io:format("whistlee: starting app~n"),
    {ok, _} = ranch:start_listener(
        hap,
        ranch_tcp,
        #{socket_opts => [{port, 9394}]},
        hap_protocol,
        []
    ),
    whistlee_sup:start_link().

stop(_State) ->
    ok.

%% internal functions
