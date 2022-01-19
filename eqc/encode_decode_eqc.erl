-module(encode_decode_eqc).

-include_lib("eqc/include/eqc.hrl").

-export([prop_check_tc128/0]).

prop_check_tc128() ->
    ?FORALL(Bin, gen_bytes8(),
            begin
                {ok, Encoded} = erldpc:encode_tc128(Bin),
                {ok, Decoded} = erldpc:decode_ms_tc128(flip(Encoded)),

                Check = Decoded == Bin,

                ?WHENFAIL(begin
                              io:format("Bin: ~p~n", [Bin]),
                              io:format("Encoded: ~p~n", [Encoded]),
                              io:format("Decoded: ~p~n", [Decoded])
                          end,
                          conjunction([{verify_tc128, Check}]))

            end).

gen_bytes8() ->
    binary(8).

flip(Data) ->
    << begin
           case rand:uniform(100) < 1 of
               true -> <<(B bxor 1):1/integer>>;
               false -> <<B:1/integer>>
           end
       end || <<B:1/integer>> <= Data >>.
