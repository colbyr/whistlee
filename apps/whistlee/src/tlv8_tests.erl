-module(tlv8_tests).
-include_lib("eunit/include/eunit.hrl").

-define(SEPARATOR, <<16#ff, 16#00>>).

decode_test() ->
    ?assertMatch(
        [#{}],
        tlv8:decode(?SEPARATOR, <<>>)
    ),

    ?assertMatch(
        [#{16#1f := <<16#01, 16#02, 16#03>>}],
        tlv8:decode(?SEPARATOR, <<16#1f, 16#03, 16#01, 16#02, 16#03>>)
    ),

    ?assertMatch(
        [
            #{
                16#1f := <<16#01, 16#02, 16#03>>,
                16#00 := <<16#01, 16#02>>,
                16#0f := <<16#01, 16#02, 16#03, 16#04>>
            }
        ],
        tlv8:decode(
            ?SEPARATOR,
            <<
                16#1f,
                16#03,
                16#01,
                16#02,
                16#03,
                16#00,
                16#02,
                16#01,
                16#02,
                16#0f,
                16#04,
                16#01,
                16#02,
                16#03,
                16#04
            >>
        )
    ),

    ?assertMatch(
        [
            #{
                16#1f := <<16#01, 16#02, 16#03>>,
                16#00 := <<16#01, 16#02>>,
                16#0f := <<16#01, 16#02, 16#03, 16#04>>
            },
            #{
                16#1f := <<16#01, 16#02, 16#03>>,
                16#00 := <<16#01, 16#02>>,
                16#0f := <<16#01, 16#02, 16#03, 16#04>>
            }
        ],
        tlv8:decode(
            ?SEPARATOR,
            <<
                16#1f,
                16#03,
                16#01,
                16#02,
                16#03,
                16#00,
                16#02,
                16#01,
                16#02,
                16#0f,
                16#04,
                16#01,
                16#02,
                16#03,
                16#04,

                16#ff,
                16#00,

                16#1f,
                16#03,
                16#01,
                16#02,
                16#03,
                16#00,
                16#02,
                16#01,
                16#02,
                16#0f,
                16#04,
                16#01,
                16#02,
                16#03,
                16#04
            >>
        )
    ),

    ?assertError(
        badarg,
        tlv8:decode(?SEPARATOR, <<16#1f, 16#05, 16#01, 16#02, 16#03>>)
    ).

decode_by_schema_test() ->
    ?assertMatch(
        [#{16#1f := <<16#01, 16#02, 16#03>>}],
        tlv8:decode_by_schema(
            ?SEPARATOR,
            #{},
            <<16#1f, 16#03, 16#01, 16#02, 16#03>>
        )
    ),

    ?assertMatch(
        [#{test := 1}],
        tlv8:decode_by_schema(
            ?SEPARATOR,
            #{16#1f => {test, integer}},
            <<16#1f, 16#01, 16#01>>
        )
    ),

    ?assertMatch(
        [#{test := "wow"}],
        tlv8:decode_by_schema(
            ?SEPARATOR,
            #{16#1f => {test, utf8}},
            <<16#1f, 16#03, 119, 111, 119>>
        )
    ),

    ?assertMatch(
        [#{test := "so fancy"}],
        tlv8:decode_by_schema(
            ?SEPARATOR,
            #{16#1f => {test, fun(_) -> "so fancy" end}},
            <<16#1f, 16#03, 119, 111, 119>>
        )
    ),

    ?assertMatch(
        [#{one := 1, two := "two", three := <<3>>}],
        tlv8:decode_by_schema(
            ?SEPARATOR,
            #{
                16#01 => {one, integer},
                16#02 => {two, utf8},
                16#03 => {three, bytes}
            },
            <<
                16#01,
                16#01,
                16#01,
                16#02,
                16#03,
                116,
                119,
                111,
                16#03,
                16#01,
                16#03
            >>
        )
    ),

    ?assertMatch(
        [
            #{one := 1, two := "two", three := <<3>>},
            #{one := 1, two := "two", three := <<3>>}
        ],
        tlv8:decode_by_schema(
            ?SEPARATOR,
            #{
                16#01 => {one, integer},
                16#02 => {two, utf8},
                16#03 => {three, bytes}
            },
            <<
                16#01,
                16#01,
                16#01,
                16#02,
                16#03,
                116,
                119,
                111,
                16#03,
                16#01,
                16#03,

                16#ff,
                16#00,

                16#01,
                16#01,
                16#01,
                16#02,
                16#03,
                116,
                119,
                111,
                16#03,
                16#01,
                16#03
            >>
        )
    ).
