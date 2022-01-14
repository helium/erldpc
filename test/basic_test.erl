-module(basic_test).

-include_lib("eunit/include/eunit.hrl").

flip(Data) ->
    << begin
           case rand:uniform(100) < 1 of
               true -> <<(B bxor 1):1/integer>>;
               false -> <<B:1/integer>>
           end
       end || <<B:1/integer>> <= Data >>.

encode_decode_bf_tc128_test() ->
    NumBytes = 8,
    Data = crypto:strong_rand_bytes(NumBytes),
    {ok, Encoded} = erldpc:encode_tc128(Data),
    io:format("EncSize: ~p~n", [byte_size(Encoded)]),
    {ok, Decoded} = erldpc:decode_bf_tc128(flip(Encoded)),
    io:format("DecSize: ~p~n", [byte_size(Decoded)]),
    ?assertEqual(Data, Decoded).

encode_decode_ms_tc128_test() ->
    NumBytes = 8,
    Data = crypto:strong_rand_bytes(NumBytes),
    {ok, Encoded} = erldpc:encode_tc128(Data),
    {ok, Decoded} = erldpc:decode_ms_tc128(flip(Encoded)),
    ?assertEqual(Data, Decoded).

encode_decode_ms_soft_tc128_test() ->
    NumBytes = 8,
    Data = crypto:strong_rand_bytes(NumBytes),
    {ok, Encoded} = erldpc:encode_tc128(Data),
    {ok, Decoded} = erldpc:decode_ms_soft_tc128(
                      flip(Encoded),
                      lists:map(fun(_) -> 0 end, lists:seq(1, 128))),
    ?assertEqual(Data, Decoded).

encode_decode_bf_tc256_test() ->
    NumBytes = 16,
    Data = crypto:strong_rand_bytes(NumBytes),
    {ok, Encoded} = erldpc:encode_tc256(Data),
    {ok, Decoded} = erldpc:decode_bf_tc256(flip(Encoded)),
    ?assertEqual(Data, Decoded).

encode_decode_ms_tc256_test() ->
    NumBytes = 16,
    Data = crypto:strong_rand_bytes(NumBytes),
    {ok, Encoded} = erldpc:encode_tc256(Data),
    {ok, Decoded} = erldpc:decode_ms_tc256(flip(Encoded)),
    ?assertEqual(Data, Decoded).

encode_decode_ms_soft_tc256_test() ->
    NumBytes = 16,
    Data = crypto:strong_rand_bytes(NumBytes),
    {ok, Encoded} = erldpc:encode_tc256(Data),
    {ok, Decoded} = erldpc:decode_ms_soft_tc256(
                      flip(Encoded),
                      lists:map(fun(_) -> 0 end, lists:seq(1, 256))),
    ?assertEqual(Data, Decoded).

encode_decode_bf_tc512_test() ->
    NumBytes = 32,
    Data = crypto:strong_rand_bytes(NumBytes),
    {ok, Encoded} = erldpc:encode_tc512(Data),
    {ok, Decoded} = erldpc:decode_bf_tc512(flip(Encoded)),
    ?assertEqual(Data, Decoded).

encode_decode_ms_tc512_test() ->
    NumBytes = 32,
    Data = crypto:strong_rand_bytes(NumBytes),
    {ok, Encoded} = erldpc:encode_tc512(Data),
    {ok, Decoded} = erldpc:decode_ms_tc512(flip(Encoded)),
    ?assertEqual(Data, Decoded).

encode_decode_ms_soft_tc512_test() ->
    NumBytes = 32,
    Data = crypto:strong_rand_bytes(NumBytes),
    {ok, Encoded} = erldpc:encode_tc512(Data),
    {ok, Decoded} = erldpc:decode_ms_soft_tc512(
                      flip(Encoded),
                      lists:map(fun(_) -> 0 end, lists:seq(1, 512))),
    ?assertEqual(Data, Decoded).

encode_decode_bf_tm1280_test() ->
    NumBytes = 128,
    Data = crypto:strong_rand_bytes(NumBytes),
    {ok, Encoded} = erldpc:encode_tm1280(Data),
    {ok, Decoded} = erldpc:decode_bf_tm1280(flip(Encoded)),
    ?assertEqual(Data, Decoded).

encode_decode_ms_tm1280_test() ->
    NumBytes = 128,
    Data = crypto:strong_rand_bytes(NumBytes),
    {ok, Encoded} = erldpc:encode_tm1280(Data),
    {ok, Decoded} = erldpc:decode_ms_tm1280(flip(Encoded)),
    ?assertEqual(Data, Decoded).

encode_decode_ms_soft_tm1280_test() ->
    NumBytes = 128,
    Data = crypto:strong_rand_bytes(NumBytes),
    {ok, Encoded} = erldpc:encode_tm1280(Data),
    {ok, Decoded} = erldpc:decode_ms_soft_tm1280(
                      flip(Encoded),
                      lists:map(fun(_) -> 0 end, lists:seq(1, 1280))),
    ?assertEqual(Data, Decoded).

encode_decode_bf_tm1536_test() ->
    NumBytes = 128,
    Data = crypto:strong_rand_bytes(NumBytes),
    {ok, Encoded} = erldpc:encode_tm1536(Data),
    {ok, Decoded} = erldpc:decode_bf_tm1536(flip(Encoded)),
    ?assertEqual(Data, Decoded).

encode_decode_ms_tm1536_test() ->
    NumBytes = 128,
    Data = crypto:strong_rand_bytes(NumBytes),
    {ok, Encoded} = erldpc:encode_tm1536(Data),
    {ok, Decoded} = erldpc:decode_ms_tm1536(flip(Encoded)),
    ?assertEqual(Data, Decoded).

encode_decode_ms_soft_tm1536_test() ->
    NumBytes = 128,
    Data = crypto:strong_rand_bytes(NumBytes),
    {ok, Encoded} = erldpc:encode_tm1536(Data),
    {ok, Decoded} = erldpc:decode_ms_soft_tm1536(
                      flip(Encoded),
                      lists:map(fun(_) -> 0 end, lists:seq(1, 1536))),
    ?assertEqual(Data, Decoded).

encode_decode_bf_tm2048_test() ->
    NumBytes = 128,
    Data = crypto:strong_rand_bytes(NumBytes),
    {ok, Encoded} = erldpc:encode_tm2048(Data),
    {ok, Decoded} = erldpc:decode_bf_tm2048(flip(Encoded)),
    ?assertEqual(Data, Decoded).

encode_decode_ms_tm2048_test() ->
    NumBytes = 128,
    Data = crypto:strong_rand_bytes(NumBytes),
    {ok, Encoded} = erldpc:encode_tm2048(Data),
    {ok, Decoded} = erldpc:decode_ms_tm2048(flip(Encoded)),
    ?assertEqual(Data, Decoded).

encode_decode_ms_soft_tm2048_test() ->
    NumBytes = 128,
    Data = crypto:strong_rand_bytes(NumBytes),
    {ok, Encoded} = erldpc:encode_tm2048(Data),
    {ok, Decoded} = erldpc:decode_ms_soft_tm2048(
                      flip(Encoded),
                      lists:map(fun(_) -> 0 end, lists:seq(1, 2048))),
    ?assertEqual(Data, Decoded).

encode_decode_bf_tm5120_test() ->
    NumBytes = 512,
    Data = crypto:strong_rand_bytes(NumBytes),
    {ok, Encoded} = erldpc:encode_tm5120(Data),
    {ok, Decoded} = erldpc:decode_bf_tm5120(flip(Encoded)),
    ?assertEqual(Data, Decoded).

encode_decode_ms_tm5120_test() ->
    NumBytes = 512,
    Data = crypto:strong_rand_bytes(NumBytes),
    {ok, Encoded} = erldpc:encode_tm5120(Data),
    {ok, Decoded} = erldpc:decode_ms_tm5120(flip(Encoded)),
    ?assertEqual(Data, Decoded).

encode_decode_ms_soft_tm5120_test() ->
    NumBytes = 512,
    Data = crypto:strong_rand_bytes(NumBytes),
    {ok, Encoded} = erldpc:encode_tm5120(Data),
    {ok, Decoded} = erldpc:decode_ms_soft_tm5120(
                      flip(Encoded),
                      lists:map(fun(_) -> 0 end, lists:seq(1, 5120))),
    ?assertEqual(Data, Decoded).

encode_decode_bf_tm6144_test() ->
    NumBytes = 512,
    Data = crypto:strong_rand_bytes(NumBytes),
    {ok, Encoded} = erldpc:encode_tm6144(Data),
    {ok, Decoded} = erldpc:decode_bf_tm6144(flip(Encoded)),
    ?assertEqual(Data, Decoded).

encode_decode_ms_tm6144_test() ->
    NumBytes = 512,
    Data = crypto:strong_rand_bytes(NumBytes),
    {ok, Encoded} = erldpc:encode_tm6144(Data),
    {ok, Decoded} = erldpc:decode_ms_tm6144(flip(Encoded)),
    ?assertEqual(Data, Decoded).

encode_decode_ms_soft_tm6144_test() ->
    NumBytes = 512,
    Data = crypto:strong_rand_bytes(NumBytes),
    {ok, Encoded} = erldpc:encode_tm6144(Data),
    {ok, Decoded} = erldpc:decode_ms_soft_tm6144(
                      flip(Encoded),
                      lists:map(fun(_) -> 0 end, lists:seq(1, 6144))),
    ?assertEqual(Data, Decoded).

encode_decode_bf_tm8192_test() ->
    NumBytes = 512,
    Data = crypto:strong_rand_bytes(NumBytes),
    {ok, Encoded} = erldpc:encode_tm8192(Data),
    {ok, Decoded} = erldpc:decode_bf_tm8192(flip(Encoded)),
    ?assertEqual(Data, Decoded).

encode_decode_ms_tm8192_test() ->
    NumBytes = 512,
    Data = crypto:strong_rand_bytes(NumBytes),
    {ok, Encoded} = erldpc:encode_tm8192(Data),
    {ok, Decoded} = erldpc:decode_ms_tm8192(flip(Encoded)),
    ?assertEqual(Data, Decoded).

encode_decode_ms_soft_tm8192_test() ->
    NumBytes = 512,
    Data = crypto:strong_rand_bytes(NumBytes),
    {ok, Encoded} = erldpc:encode_tm8192(Data),
    {ok, Decoded} = erldpc:decode_ms_soft_tm8192(
                      flip(Encoded),
                      lists:map(fun(_) -> 0 end, lists:seq(1, 8192))),
    ?assertEqual(Data, Decoded).
