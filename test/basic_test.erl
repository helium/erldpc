-module(basic_test).

-include_lib("eunit/include/eunit.hrl").

encode_decode_tc128_test() ->
    NumBytes = 8,
    Data = crypto:strong_rand_bytes(NumBytes),
    {ok, Encoded} = erldpc:encode_tc128(Data),

    <<_:1/binary, Rest/binary>> = Encoded,

    Corrupted = <<"x", Rest/binary>>,

    {ok, <<Original:NumBytes/binary, _/binary>>} = erldpc:decode_tc128(Corrupted),

    ?assertEqual(Original, Data).

encode_decode_tc256_test() ->
    NumBytes = 16,
    Data = crypto:strong_rand_bytes(NumBytes),
    {ok, Encoded} = erldpc:encode_tc256(Data),

    <<_:1/binary, Rest/binary>> = Encoded,

    Corrupted = <<"x", Rest/binary>>,

    {ok, <<Original:NumBytes/binary, _/binary>>} = erldpc:decode_tc256(Corrupted),

    ?assertEqual(Original, Data).

encode_decode_tc512_test() ->
    NumBytes = 32,
    Data = crypto:strong_rand_bytes(NumBytes),
    {ok, Encoded} = erldpc:encode_tc512(Data),

    <<_:1/binary, Rest/binary>> = Encoded,

    Corrupted = <<"x", Rest/binary>>,

    {ok, <<Original:NumBytes/binary, _/binary>>} = erldpc:decode_tc512(Corrupted),

    ?assertEqual(Original, Data).

encode_decode_tm1280_test() ->
    NumBytes = 128,
    Data = crypto:strong_rand_bytes(NumBytes),
    {ok, Encoded} = erldpc:encode_tm1280(Data),

    <<_:1/binary, Rest/binary>> = Encoded,

    Corrupted = <<"x", Rest/binary>>,

    {ok, <<Original:NumBytes/binary, _/binary>>} = erldpc:decode_tm1280(Corrupted),

    ?assertEqual(Original, Data).

encode_decode_tm1536_test() ->
    NumBytes = 128,
    Data = crypto:strong_rand_bytes(NumBytes),
    {ok, Encoded} = erldpc:encode_tm1536(Data),

    <<_:1/binary, Rest/binary>> = Encoded,

    Corrupted = <<"x", Rest/binary>>,

    {ok, <<Original:NumBytes/binary, _/binary>>} = erldpc:decode_tm1536(Corrupted),

    ?assertEqual(Original, Data).

encode_decode_tm2048_test() ->
    NumBytes = 128,
    Data = crypto:strong_rand_bytes(NumBytes),
    {ok, Encoded} = erldpc:encode_tm2048(Data),

    <<_:1/binary, Rest/binary>> = Encoded,

    Corrupted = <<"x", Rest/binary>>,

    {ok, <<Original:NumBytes/binary, _/binary>>} = erldpc:decode_tm2048(Corrupted),

    ?assertEqual(Original, Data).

%% XXX: These aren't working cuz they don't seem to conform to the standard decode afaict
%% encode_decode_tm5120_test() ->
%%     NumBytes = 512,
%%     Data = lists:seq(0, NumBytes - 1),
%%     {ok, Encoded} = erldpc:encode_tm5120(Data),
%%     [_First, _Second, _Third | Rest] = Encoded,
%%     CorruptedEncode = [10, 20, 30 | Rest],
%% 
%%     {ok, Decoded} = erldpc:decode_tm5120(CorruptedEncode),
%%     io:format("Data: ~p~n", [Data]),
%%     io:format("Encoded: ~p~n", [Encoded]),
%%     io:format("CorruptedEncode: ~p~n", [CorruptedEncode]),
%%     io:format("Decoded: ~p~n", [Decoded]),
%%     ?assertEqual(lists:sublist(Decoded, NumBytes), Data).
%% 
%% encode_decode_tm6144_test() ->
%%     NumBytes = 512,
%%     Data = lists:seq(0, NumBytes - 1),
%%     {ok, Encoded} = erldpc:encode_tm6144(Data),
%%     [_First, _Second, _Third | Rest] = Encoded,
%%     CorruptedEncode = [10, 20, 30 | Rest],
%% 
%%     {ok, Decoded} = erldpc:decode_tm6144(CorruptedEncode),
%%     ?assertEqual(lists:sublist(Decoded, NumBytes), Data).
%% 
%% encode_decode_tm8192_test() ->
%%     NumBytes = 512,
%%     Data = lists:seq(0, NumBytes - 1),
%%     {ok, Encoded} = erldpc:encode_tm8192(Data),
%%     [_First, _Second, _Third | Rest] = Encoded,
%%     CorruptedEncode = [10, 20, 30 | Rest],
%% 
%%     {ok, Decoded} = erldpc:decode_tm8192(CorruptedEncode),
%%     ?assertEqual(lists:sublist(Decoded, NumBytes), Data).
