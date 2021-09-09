-module(tlv8).
-export([
  decode/2,
  decode_by_schema/3,
  decode_pairing/1
]).

decode(Separator, Binary) ->
  BinaryEntries = binary:split(Binary, Separator),
  [decode_entry(Entry) || Entry <- BinaryEntries].

decode_entry(Binary) ->
  case decode_entry(Binary, #{}) of
    #{ invalid := Invalid } -> erlang:error(badarg, Invalid);
    Result -> Result
  end.

decode_entry(<<>>, Acc) ->
  Acc;
decode_entry(<<Type:8, Size:8, Value:(Size)/binary, NextBinary/binary>>, Acc) ->
  NextAcc = maps:put(Type, Value, Acc),
  decode_entry(NextBinary, NextAcc);
decode_entry(Binary, Acc) ->
  maps:put(invalid, Binary, Acc).

% #{ 16#01 => {name, value_converter} }
%

get_key_by_schema(Key, Schema) ->
  case maps:get(Key, Schema, undefined) of
    {NamedKey, _} -> NamedKey;
    undefined -> Key
  end.

get_value_by_schema(Key, Value, Schema) ->
  case maps:get(Key, Schema, undefined) of
    {_, bytes} -> Value;
    {_, integer} ->
      <<Int/integer>> = Value,
      Int;
    {_, integer_unsigned} ->
      <<Int:32/integer-unsigned>> = Value,
      Int;
    {_, utf8} -> binary_to_list(Value);
    {_, _} -> Value;
    undefined -> Value
  end.


decode_by_schema(Separator, Schema, Binary) ->
  [decode_entry_by_schema(Schema, Entry) || Entry <- decode(Separator, Binary)].

decode_entry_by_schema(Schema, RawData) ->
  SchemafiedList = [{
    get_key_by_schema(K, Schema),
    get_value_by_schema(K, V, Schema)
   } || {K, V} <- maps:to_list(RawData)],
  maps:from_list(SchemafiedList).

-define(PAIRING_TLV8_SEPARATOR, <<16#FF, 16#00>>).
-define(PAIRING_TLV8_SCHEMA, #{
  16#00 => {method, integer},
  16#01 => {identifier, utf8},
  16#02 => {salt, bytes},
  16#03 => {public_key, bytes},
  16#04 => {proof, bytes},
  16#05 => {encrypted_data, bytes},
  16#06 => {state, integer},
  16#07 => {error, integer},
  16#08 => {retry_delay, integer},
  16#09 => {certificate, bytes},
  16#0A => {signature, bytes},
  16#0B => {permissions, integer},
  16#0C => {fragment_data, bytes},
  16#0D => {fragment_last, bytes},
  16#13 => {flags, integer_unsigned}
}).

decode_pairing(Binary) ->
  decode_by_schema(?PAIRING_TLV8_SEPARATOR, ?PAIRING_TLV8_SCHEMA, Binary).


