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

configuration_number() ->
    1.

% based on https://github.com/homebridge/HAP-NodeJS/blob/81652da554137d818d049f6f245e88efec43b1c5/src/lib/Accessory.ts#L85-L125
category(Category) ->
    case Category of
        other -> 1;
        switch -> 8;
        _ -> category(other)
    end.

% based on https://github.com/homebridge/HAP-NodeJS/blob/81652da554137d818d049f6f245e88efec43b1c5/src/lib/Advertiser.ts#L27-L34
pairing_feature_flag(Feature) ->
    case Feature of
        supports_hardware_authentication -> 16#01;
        supports_software_authentication -> 16#02
    end.

% based on https://github.com/homebridge/HAP-NodeJS/blob/81652da554137d818d049f6f245e88efec43b1c5/src/lib/Advertiser.ts#L17-L25
status_flag(Status) ->
    case Status of
        not_paired -> 16#01;
        not_joined_wifi -> 16#02;
        problem_detected -> 16#04
    end.

% based on https://github.com/homebridge/HAP-NodeJS/blob/81652da554137d818d049f6f245e88efec43b1c5/src/lib/Advertiser.ts#L160-L164
setup_hash(DeviceId, SetupId) ->
    Hash = crypto:hash(sha512, [DeviceId, SetupId]),
    <<Prefix:4/binary, _/binary>> = Hash,
    base64:encode_to_string(Prefix).

format_txt_value(Int) when is_integer(Int) ->
    io_lib:format("~p", [Int]);
format_txt_value(Value) ->
    Value.

format_txt(Txt) ->
    [{Key, format_txt_value(Value)} || {Key, Value} <- Txt].

log(Msg, Args) ->
    io:format("discovery: " ++ Msg ++ "~n", Args).
log(Msg) ->
    log(Msg, []).

start_link(Name, Type, Port) ->
    gen_server:start_link(?MODULE, {Name, Type, Port}, []).

init({Name, Type, Port}) ->
    log("registering"),
    DeviceId = "C3:5D:3A:AE:5E:FB",
    Txt = [
        % based on https://github.com/homebridge/HAP-NodeJS/blob/master/src/lib/Advertiser.ts#L147-L157

        % current configuration number
        {'c#', configuration_number()},
        % pairing feature flags
        {ff, pairing_feature_flag(supports_software_authentication)},
        % device id
        {id, DeviceId},
        % model name
        {md, "whistlee@0.1.0"},
        % protocol version
        {pv, "1.1"},
        % current state number (must be 1)
        {'s#', "1"},
        % status flag
        {sf, status_flag(not_paired)},
        % accessory category
        {ci, category(switch)},
        % setup hash
        {sh, setup_hash(DeviceId, "")}
    ],
    FormattedTxt = format_txt(Txt),
    log("registering~p", [FormattedTxt]),
    {ok, Ref} = dnssd:register(Name, Type, Port, FormattedTxt),
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
