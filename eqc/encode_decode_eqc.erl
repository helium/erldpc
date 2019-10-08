-module(encode_decode_eqc).

-include_lib("eqc/include/eqc.hrl").

-export([prop_check_tc128/0]).

prop_check_tc128() ->
    ?FORALL({Data, Corruption0, Corruption1}, {gen_bytes8(), gen_corruption(), gen_corruption()},
            begin
                Size = size(Data), %% 8 bytes for tc128
                {ok, Encoded} = erldpc:encode_tc128(Data),

                CorruptionA = abs(Corruption0) band 16#FFFFFFFFFFFFFFFF,
                CorruptionB = abs(Corruption1) band 16#FFFFFFFFFFFFFFFF,

                CorruptionVector = (CorruptionA bsl 128) + CorruptionB,

                NumErrors = popcount(CorruptionVector),

                <<EncodedAsInteger:128/integer-unsigned-little>> = Encoded,

                Corrupted = <<(EncodedAsInteger bxor CorruptionVector):128/integer-unsigned-little>>,

                {ok, <<Original:Size/binary, _/binary>>} = erldpc:decode_tc128(Corrupted),

                Check = case NumErrors of
                            N when N > 62 ->
                                Original /= Data;
                            N when N < 8->
                                Original == Data;
                            _ ->
                                %% yolo
                                true
                        end,

                ?WHENFAIL(begin
                              io:format("Data: ~p~n", [Data]),
                              io:format("NumErrors: ~p~n", [NumErrors]),
                              io:format("Encoded: ~p~n", [Encoded]),
                              io:format("Corrupted: ~p~n", [Corrupted]),
                              io:format("Original: ~p~n", [Original])
                          end,
                          conjunction([{verify_tc128, Check}]))

            end).

gen_bytes8() ->
    ?SUCHTHAT(B, binary(8), B /= <<0, 0, 0, 0, 0, 0, 0, 0>>).

gen_corruption() ->
    ?SUCHTHAT(I, largeint(), I /= 0).

popcount(N) ->
    popcount(N,0).

popcount(0,Acc) ->
    Acc;
popcount(N,Acc) ->
    popcount(N div 2, Acc + N rem 2).
