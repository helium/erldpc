-module(encode_decode_eqc).

-include_lib("eqc/include/eqc.hrl").

-export([prop_check_tc128/0]).

prop_check_tc128() ->
    ?FORALL({Data, Corruption}, {gen_bytes8(), gen_corruption()},
            begin
                Size = size(Data), %% 8 bytes for tc128
                {ok, Encoded} = erldpc:encode_tc128(Data),

                <<_:1/binary, Rest/binary>> = Encoded,

                Corrupted = <<Corruption/binary, Rest/binary>>,

                {ok, <<Original:Size/binary, _/binary>>} = erldpc:decode_tc128(Corrupted),

                Check = size(Encoded) == 2*Size andalso Original == Data,

                ?WHENFAIL(begin
                              io:format("Data: ~p~n", [Data]),
                              io:format("Encoded: ~p~n", [Encoded]),
                              io:format("Corrupted: ~p~n", [Corrupted]),
                              io:format("Original: ~p~n", [Original])
                          end,
                          conjunction([{verify_tc128, Check}]))

            end).

gen_bytes8() ->
    binary(8).

gen_corruption() ->
    binary(1).
