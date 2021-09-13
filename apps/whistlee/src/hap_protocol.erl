-module(hap_protocol).
-behaviour(ranch_protocol).

-export([start_link/3]).
-export([init/3]).

clog(Msg, Args) ->
    io:format("pairing: " ++ Msg ++ "~n", Args).
clog(Msg) ->
    clog(Msg, []).

start_link(Ref, Transport, Opts) ->
    Pid = spawn_link(?MODULE, init, [Ref, Transport, Opts]),
    {ok, Pid}.

init(Ref, Transport, _Opts = []) ->
    {ok, Socket} = ranch:handshake(Ref),
    clog("starting"),
    loop(Socket, Transport).

loop(Socket, Transport) ->
    case Transport:recv(Socket, 0, 60000) of
        {ok, Data} when Data =/= <<4>> ->
            clog("req~n~n~p", [Data]),
            Lines = binary:split(Data, <<"\r\n">>, [global, trim_all]),
            Content = tlv8:decode_pairing(lists:last(Lines)),
            clog("decoded content:~n~p~n", [Content]),
            loop(Socket, Transport);
        _ ->
            ok = Transport:close(Socket)
    end.
