-module(basic_test).

-include_lib("eunit/include/eunit.hrl").

encode_decode_tc128_test() ->
    Data = lists:seq(0, 7),
    {ok, Encoded} = erldpc:encode_tc128(Data),
    ?assertEqual(16, length(Encoded)).
encode_decode_tc256_test() ->
    Data = lists:seq(0, 15),
    {ok, Encoded} = erldpc:encode_tc256(Data),
    ?assertEqual(32, length(Encoded)).
encode_decode_tc512_test() ->
    Data = lists:seq(0, 31),
    {ok, Encoded} = erldpc:encode_tc512(Data),
    ?assertEqual(64, length(Encoded)).
encode_decode_tm1280_test() ->
    Data = lists:seq(0, 127),
    {ok, Encoded} = erldpc:encode_tm1280(Data),
    ?assertEqual(160, length(Encoded)).
encode_decode_tm1536_test() ->
    Data = lists:seq(0, 127),
    {ok, Encoded} = erldpc:encode_tm1536(Data),
    ?assertEqual(192, length(Encoded)).
encode_decode_tm2048_test() ->
    Data = lists:seq(0, 127),
    {ok, Encoded} = erldpc:encode_tm2048(Data),
    ?assertEqual(256, length(Encoded)).
encode_decode_tm5120_test() ->
    Data = lists:seq(0, 511),
    {ok, Encoded} = erldpc:encode_tm5120(Data),
    ?assertEqual(640, length(Encoded)).
encode_decode_tm6144_test() ->
    Data = lists:seq(0, 511),
    {ok, Encoded} = erldpc:encode_tm6144(Data),
    ?assertEqual(768, length(Encoded)).
encode_decode_tm8192_test() ->
    Data = lists:seq(0, 511),
    {ok, Encoded} = erldpc:encode_tm8192(Data),
    ?assertEqual(1024, length(Encoded)).
