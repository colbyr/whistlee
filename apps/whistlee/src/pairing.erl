-module(pairing).
-behavior(gen_server).
-export([
  init/1,
  handle_call/3,
  handle_cast/2,
  handle_info/2,
  start_link/1,
  terminate/2
]).

clog(Msg, Args) ->
  io:format("pairing: " ++ Msg ++ "~n", Args).
clog (Msg) ->
  clog(Msg, []).

start_link(Port) ->
  gen_server:start_link(?MODULE, {Port}, []).

init({Port}) ->
  {ok, ListenSocket} = gen_tcp:listen(Port, [{active, true}]),
  {ok, AcceptSocket} = gen_tcp:accept(ListenSocket),
  inet:setopts(AcceptSocket, [{active, true}]),
  {ok, {AcceptSocket, ListenSocket}}.

handle_call(_E, _From, State) ->
  clog("handle_call"),
  {noreply, State}.

handle_cast(_Msg, State) ->
  clog("handle_cast"),
  {noreply, State}.

handle_info({tcp, _Socket, Req}, State) ->
  clog("got request~n~n~p", [Req]),
  {noreply, State}.

terminate(normal, {AcceptSocket, ListenSocket}) ->
  clog("shutting down"),
  gen_tcp:close(AcceptSocket),
  gen_tcp:close(ListenSocket),
  ok;
terminate(_Reason, {AcceptSocket, ListenSocket}) ->
  io:format("terminate reason: ~p~n", [_Reason]),
  gen_tcp:close(AcceptSocket),
  gen_tcp:close(ListenSocket),
  ok.

