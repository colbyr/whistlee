-module(hue).
-export([
    endpoint/0,
    endpoint/1,
    light_info/1,
    light_off/1,
    light_on/1
]).

get_json(Url) ->
    {ok, _, _, ClientRef} = hackney:get(Url, [], <<>>, []),
    {ok, Body} = hackney:body(ClientRef),
    jiffy:decode(Body, [return_maps]).

put_json(Url, Payload) ->
    Json = jiffy:encode(Payload),
    {ok, _, _, ClientRef} = hackney:put(Url, [], Json, []),
    {ok, Body} = hackney:body(ClientRef),
    jiffy:decode(Body, [return_maps]).

endpoint() ->
    endpoint([]).

endpoint(Path) ->
    Ip = os:getenv("HUE_BRIDGE_ADDRESS"),
    UserId = os:getenv("HUE_USER_ID"),
    SeparatedPath = lists:join("/", Path),
    list_to_binary(
        "http://" ++ Ip ++ "/api/" ++ UserId ++ "/" ++
            lists:concat(SeparatedPath)
    ).

light_info(LightId) ->
    Url = endpoint(["lights", LightId]),
    get_json(Url).

light_off(LightId) ->
    Url = endpoint(["lights", LightId, "state"]),
    put_json(Url, #{<<"on">> => false}).

light_on(LightId) ->
    Url = endpoint(["lights", LightId, "state"]),
    put_json(Url, #{<<"on">> => true}).
