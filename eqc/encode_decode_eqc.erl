-module(encode_decode_eqc).

-include_lib("eqc/include/eqc.hrl").

-export([prop_check_tc128/0]).

prop_check_tc128() ->
    ?FORALL(Bin, gen_bytes8(),
            begin
                Data = binary_to_list(Bin),
                DataLength = length(Data),
                {ok, Encoded} = erldpc:encode_tc128(Data),
                EncodedLength = length(Encoded),

                [_First | Rest] = Encoded,
                CorruptedEncode = [10 | Rest],

                {ok, Decoded} = erldpc:decode_tc128(CorruptedEncode),
                DecodedLength = length(Decoded),

                Check = (EncodedLength == 2*DataLength andalso
                         DecodedLength == 2*DataLength andalso
                         lists:sublist(Decoded, DataLength) == Data),

               ?WHENFAIL(begin
                              io:format("Bin: ~p~n", [Bin]),
                              io:format("Data: ~p~n", [Data]),
                              io:format("Encoded: ~p~n", [Encoded]),
                              io:format("CorruptedEncode: ~p~n", [CorruptedEncode]),
                              io:format("Decoded: ~p~n", [Decoded])
                          end,
                          conjunction([{verify_tc128, Check}]))

            end).

gen_bytes8() ->
    binary(8).
