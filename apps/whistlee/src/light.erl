-module(light).
-behavior(gen_server).
-export([
  blink/1,
  init/1,
  handle_call/3,
  handle_cast/2,
  handle_info/2,
  start_link/1,
  code_change/3,
  terminate/2,
  disconnect/1,
  turn_off/1,
  turn_on/1
]).

stop_blinker(State = #{blinker := TRef}) ->
  erlang:cancel_timer(TRef),
  maps:without([blinker], State).


init(State = #{id := LightId}) ->
  io:format("Conntecting to light #~p~n", [LightId]),
  {ok, State}.


code_change(_OldVsn, State, _Extra) ->
  %% No change planned. The function is there for the behaviour,
  %% but will not be used. Only a version on the next
  {ok, State}.


handle_call({on}, _From, State = #{id := LightId}) ->
  io:format("Turn ON light #~p~n", [LightId]),
  Resp = hue:light_on(LightId),
  {reply, Resp, stop_blinker(State)};

handle_call({off}, _From, State = #{id := LightId}) ->
  io:format("Turn OFF light #~p~n", [LightId]),
  Resp = hue:light_off(LightId),
  {reply, Resp, stop_blinker(State)};

handle_call(terminate, _From, State = #{id := LightId}) ->
  io:format("Disconnecting from light #~p~n", [LightId]),
  {stop, normal, ok, stop_blinker(State)}.


handle_cast(Unexpected, State) ->
  io:format("unexpected message ~p~n", [Unexpected]),
  {noreply, State}.


handle_info(blink_on, State = #{id := LightId}) ->
  hue:light_on(LightId),
  TRef = erlang:send_after(1000, self(), blink_off),
  {noreply, maps:put(blinker, TRef, State)};

handle_info(blink_off, State = #{id := LightId}) ->
  hue:light_off(LightId),
  TRef = erlang:send_after(1000, self(), blink_on),
  {noreply, maps:put(blinker, TRef, State)};

handle_info(Msg, State) ->
  io:format("unexpected message ~p~n", [Msg]),
  {noreply, State}.


terminate(normal, #{id := LightId}) ->
  io:format("Light #~p was disconnected.~n",[LightId]),
  ok.


% Methods

start_link(LightId) -> gen_server:start_link(?MODULE, #{id => LightId}, []).

blink(Pid) ->
  Pid ! blink_on.

turn_off(Pid) ->
  gen_server:call(Pid, {off}).

turn_on(Pid) ->
  gen_server:call(Pid, {on}).

disconnect(Pid) ->
  gen_server:call(Pid, terminate).

