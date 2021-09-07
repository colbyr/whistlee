-module(discovery).
-behavior(gen_server).
-export([
  start_link/3,
  init/1,
  handle_call/3,
  handle_cast/2,
  handle_info/2,
  code_change/3,
  terminate/2
]).

log(Msg, Args) ->
  io:format("discovery: " ++ Msg ++ "~n", Args).
log (Msg) ->
  log(Msg, []).

start_link(Name, Type, Port) ->
  gen_server:start_link(?MODULE, {Name, Type, Port}, []).

init({Name, Type, Port}) ->
  log("registering"),
  {ok, Ref} = dnssd:register(Name, Type, Port),
  {ok, {Ref}}.

handle_call(_E, _From, State) ->
  log("handle_call"),
  {noreply, State}.

handle_cast(_Msg, State) ->
  log("handle_call"),
  {noreply, State}.

handle_info({dnssd, _Ref, {register, add, {Name, _, _}}}, State) ->
  log("registered ~p", [Name]),
  {noreply, State};
handle_info(UnknownMsg, State) ->
  log("unknown: ~p", [UnknownMsg]),
  {noreply, State}.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

terminate(normal, {Ref}) ->
  log("shutting down"),
  dnssd:stop(Ref);
terminate(_Reason, {Ref}) ->
  log("terminate reason: ~p", [_Reason]),
  dnssd:stop(Ref).

