%%%-------------------------------------------------------------------
%% @doc whistlee public API
%% @end
%%%-------------------------------------------------------------------

-module(whistlee_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    whistlee_sup:start_link().

stop(_State) ->
    ok.

%% internal functions
