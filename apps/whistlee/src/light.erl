-module(light).
-export([
  light/1,
  start/1,
  turn_off/1,
  turn_on/1
]).

light(LightId) ->
  receive
    {From, {on}} ->
      Resp = hue:light_on(LightId),
      From ! {self(), {ok, Resp}},
      light(LightId);

    {From, {off}} ->
      Resp = hue:light_off(LightId),
      From ! {self(), {ok, Resp}},
      light(LightId);

    terminate ->
      ok;

    Unexpected ->
      io:format("unexpected message ~p~n", [Unexpected])
  end.

start(LightId) ->
  spawn(?MODULE, light, [LightId]).

turn_off(Pid) ->
  Pid ! {self(), {off}},
  receive
    {Pid, Msg} -> Msg
  end.

turn_on(Pid) ->
  Pid ! {self(), {on}},
  receive
    {Pid, Msg} -> Msg
  end.
