-module(basic_test).

-include_lib("eunit/include/eunit.hrl").

encode_decode_test() ->
    Data = lists:seq(0, 7),
    {ok, Encoded} = erldpc:encode(Data),
    ?assertEqual(16, length(Encoded)).
