-module(light).
-behavior(gen_server).
-export([
  init/1,
  handle_call/3,
  handle_cast/2,
  handle_info/2,
  start/1,
  start_link/1,
  code_change/3,
  terminate/2,
  disconnect/1,
  turn_off/1,
  turn_on/1
]).

init(LightId) ->
  io:format("Conntecting to light #~p~n", [LightId]),
  {ok, LightId}.


code_change(_OldVsn, State, _Extra) ->
  %% No change planned. The function is there for the behaviour,
  %% but will not be used. Only a version on the next
  {ok, State}.


handle_call({on}, _From, LightId) ->
  io:format("Turn ON light #~p~n", [LightId]),
  Resp = hue:light_on(LightId),
  {reply, Resp, LightId};

handle_call({off}, _From, LightId) ->
  io:format("Turn OFF light #~p~n", [LightId]),
  Resp = hue:light_off(LightId),
  {reply, Resp, LightId};

handle_call(terminate, _From, LightId) ->
  io:format("Disconnecting from light #~p~n", [LightId]),
  {stop, normal, ok, LightId}.


handle_cast(Unexpected, LightId) ->
  io:format("unexpected message ~p~n", [Unexpected]),
  {noreply, LightId}.


handle_info(Msg, LightId) ->
  io:format("unexpected message ~p~n", [Msg]),
  {noreply, LightId}.


terminate(normal, LightId) ->
  io:format("Light #~p was disconnected.~n",[LightId]),
  ok.


% Methods

start(LightId) -> gen_server:start(?MODULE, LightId).

start_link(LightId) -> gen_server:start_link(?MODULE, LightId, []).

turn_off(Pid) ->
  gen_server:call(Pid, {off}).

turn_on(Pid) ->
  gen_server:call(Pid, {on}).

disconnect(Pid) ->
  gen_server:call(Pid, terminate).

