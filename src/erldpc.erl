-module(erldpc).

%% API
-export([
         encode_tc128/1,
         decode_tc128/1,
         encode_tc256/1,
         decode_tc256/1,
         encode_tc512/1,
         decode_tc512/1,
         encode_tm1280/1,
         decode_tm1280/1,
         encode_tm1536/1,
         decode_tm1536/1,
         encode_tm2048/1,
         decode_tm2048/1,
         encode_tm5120/1,
         decode_tm5120/1,
         encode_tm6144/1,
         decode_tm6144/1,
         encode_tm8192/1,
         decode_tm8192/1
        ]).

%% Native lib support
-export([load/0]).
-on_load(load/0).

-define(ITERATIONS, 20).

-spec encode_tc128(Data :: binary()) -> {ok, binary()}.
encode_tc128(Data) when is_binary(Data)->
    Input = binary:bin_to_list(Data),
    {ok, Encoded} = encode_tc128_native(Input),
    {ok, binary:list_to_bin(Encoded)};
encode_tc128(Data) ->
    {ok, Encoded} = encode_tc128_native(Data),
    {ok, binary:list_to_bin(Encoded)}.

-spec decode_tc128(Data :: binary()) -> {ok, binary()}.
decode_tc128(Data) when is_binary(Data) ->
    Input = binary:bin_to_list(Data),
    {ok, Decoded} = decode_tc128_native(Input, ?ITERATIONS),
    {ok, binary:list_to_bin(Decoded)};
decode_tc128(Data) ->
    {ok, Decoded} = decode_tc128_native(Data, ?ITERATIONS),
    {ok, binary:list_to_bin(Decoded)}.

-spec encode_tc256(Data :: binary()) -> {ok, binary()}.
encode_tc256(Data) when is_binary(Data)->
    Input = binary:bin_to_list(Data),
    {ok, Encoded} = encode_tc256_native(Input),
    {ok, binary:list_to_bin(Encoded)};
encode_tc256(Data) ->
    {ok, Encoded} = encode_tc256_native(Data),
    {ok, binary:list_to_bin(Encoded)}.

-spec decode_tc256(Data :: binary()) -> {ok, binary()}.
decode_tc256(Data) when is_binary(Data) ->
    Input = binary:bin_to_list(Data),
    {ok, Decoded} = decode_tc256_native(Input, ?ITERATIONS),
    {ok, binary:list_to_bin(Decoded)};
decode_tc256(Data) ->
    {ok, Decoded} = decode_tc256_native(Data, ?ITERATIONS),
    {ok, binary:list_to_bin(Decoded)}.


-spec encode_tc512(Data :: binary()) -> {ok, binary()}.
encode_tc512(Data) when is_binary(Data)->
    Input = binary:bin_to_list(Data),
    {ok, Encoded} = encode_tc512_native(Input),
    {ok, binary:list_to_bin(Encoded)};
encode_tc512(Data) ->
    {ok, Encoded} = encode_tc512_native(Data),
    {ok, binary:list_to_bin(Encoded)}.

-spec decode_tc512(Data :: binary()) -> {ok, binary()}.
decode_tc512(Data) when is_binary(Data) ->
    Input = binary:bin_to_list(Data),
    {ok, Decoded} = decode_tc512_native(Input, ?ITERATIONS),
    {ok, binary:list_to_bin(Decoded)};
decode_tc512(Data) ->
    {ok, Decoded} = decode_tc512_native(Data, ?ITERATIONS),
    {ok, binary:list_to_bin(Decoded)}.


-spec encode_tm1280(Data :: binary()) -> {ok, binary()}.
encode_tm1280(Data) when is_binary(Data)->
    Input = binary:bin_to_list(Data),
    {ok, Encoded} = encode_tm1280_native(Input),
    {ok, binary:list_to_bin(Encoded)};
encode_tm1280(Data) ->
    {ok, Encoded} = encode_tm1280_native(Data),
    {ok, binary:list_to_bin(Encoded)}.

-spec decode_tm1280(Data :: binary()) -> {ok, binary()}.
decode_tm1280(Data) when is_binary(Data) ->
    Input = binary:bin_to_list(Data),
    {ok, Decoded} = decode_tm1280_native(Input, ?ITERATIONS),
    {ok, binary:list_to_bin(Decoded)};
decode_tm1280(Data) ->
    {ok, Decoded} = decode_tm1280_native(Data, ?ITERATIONS),
    {ok, binary:list_to_bin(Decoded)}.


-spec encode_tm1536(Data :: binary()) -> {ok, binary()}.
encode_tm1536(Data) when is_binary(Data)->
    Input = binary:bin_to_list(Data),
    {ok, Encoded} = encode_tm1536_native(Input),
    {ok, binary:list_to_bin(Encoded)};
encode_tm1536(Data) ->
    {ok, Encoded} = encode_tm1536_native(Data),
    {ok, binary:list_to_bin(Encoded)}.

-spec decode_tm1536(Data :: binary()) -> {ok, binary()}.
decode_tm1536(Data) when is_binary(Data) ->
    Input = binary:bin_to_list(Data),
    {ok, Decoded} = decode_tm1536_native(Input, ?ITERATIONS),
    {ok, binary:list_to_bin(Decoded)};
decode_tm1536(Data) ->
    {ok, Decoded} = decode_tm1536_native(Data, ?ITERATIONS),
    {ok, binary:list_to_bin(Decoded)}.


-spec encode_tm2048(Data :: binary()) -> {ok, binary()}.
encode_tm2048(Data) when is_binary(Data)->
    Input = binary:bin_to_list(Data),
    {ok, Encoded} = encode_tm2048_native(Input),
    {ok, binary:list_to_bin(Encoded)};
encode_tm2048(Data) ->
    {ok, Encoded} = encode_tm2048_native(Data),
    {ok, binary:list_to_bin(Encoded)}.

-spec decode_tm2048(Data :: binary()) -> {ok, binary()}.
decode_tm2048(Data) when is_binary(Data) ->
    Input = binary:bin_to_list(Data),
    {ok, Decoded} = decode_tm2048_native(Input, ?ITERATIONS),
    {ok, binary:list_to_bin(Decoded)};
decode_tm2048(Data) ->
    {ok, Decoded} = decode_tm2048_native(Data, ?ITERATIONS),
    {ok, binary:list_to_bin(Decoded)}.


-spec encode_tm5120(Data :: binary()) -> {ok, binary()}.
encode_tm5120(Data) when is_binary(Data)->
    Input = binary:bin_to_list(Data),
    {ok, Encoded} = encode_tm5120_native(Input),
    {ok, binary:list_to_bin(Encoded)};
encode_tm5120(Data) ->
    {ok, Encoded} = encode_tm5120_native(Data),
    {ok, binary:list_to_bin(Encoded)}.

-spec decode_tm5120(Data :: binary()) -> {ok, binary()}.
decode_tm5120(Data) when is_binary(Data) ->
    Input = binary:bin_to_list(Data),
    {ok, Decoded} = decode_tm5120_native(Input, ?ITERATIONS),
    {ok, binary:list_to_bin(Decoded)};
decode_tm5120(Data) ->
    {ok, Decoded} = decode_tm5120_native(Data, ?ITERATIONS),
    {ok, binary:list_to_bin(Decoded)}.


-spec encode_tm6144(Data :: binary()) -> {ok, binary()}.
encode_tm6144(Data) when is_binary(Data)->
    Input = binary:bin_to_list(Data),
    {ok, Encoded} = encode_tm6144_native(Input),
    {ok, binary:list_to_bin(Encoded)};
encode_tm6144(Data) ->
    {ok, Encoded} = encode_tm6144_native(Data),
    {ok, binary:list_to_bin(Encoded)}.

-spec decode_tm6144(Data :: binary()) -> {ok, binary()}.
decode_tm6144(Data) when is_binary(Data) ->
    Input = binary:bin_to_list(Data),
    {ok, Decoded} = decode_tm6144_native(Input, ?ITERATIONS),
    {ok, binary:list_to_bin(Decoded)};
decode_tm6144(Data) ->
    {ok, Decoded} = decode_tm6144_native(Data, ?ITERATIONS),
    {ok, binary:list_to_bin(Decoded)}.


-spec encode_tm8192(Data :: binary()) -> {ok, binary()}.
encode_tm8192(Data) when is_binary(Data)->
    Input = binary:bin_to_list(Data),
    {ok, Encoded} = encode_tm8192_native(Input),
    {ok, binary:list_to_bin(Encoded)};
encode_tm8192(Data) ->
    {ok, Encoded} = encode_tm8192_native(Data),
    {ok, binary:list_to_bin(Encoded)}.

-spec decode_tm8192(Data :: binary()) -> {ok, binary()}.
decode_tm8192(Data) when is_binary(Data) ->
    Input = binary:bin_to_list(Data),
    {ok, Decoded} = decode_tm8192_native(Input, ?ITERATIONS),
    {ok, binary:list_to_bin(Decoded)};
decode_tm8192(Data) ->
    {ok, Decoded} = decode_tm8192_native(Data, ?ITERATIONS),
    {ok, binary:list_to_bin(Decoded)}.


load() ->
    erlang:load_nif(filename:join(priv(), "liberldpc"), none).

not_loaded(Line) ->
    erlang:nif_error({error, {not_loaded, [{module, ?MODULE}, {line, Line}]}}).

priv()->
  case code:priv_dir(?MODULE) of
      {error, _} ->
          EbinDir = filename:dirname(code:which(?MODULE)),
          AppPath = filename:dirname(EbinDir),
          filename:join(AppPath, "priv");
      Path ->
          Path
  end.

encode_tc128_native(_Data) ->
    not_loaded(?LINE).
decode_tc128_native(_Data, _Iterations) ->
    not_loaded(?LINE).


encode_tc256_native(_Data) ->
    not_loaded(?LINE).
decode_tc256_native(_Data, _Iterations) ->
    not_loaded(?LINE).

encode_tc512_native(_Data) ->
    not_loaded(?LINE).
decode_tc512_native(_Data, _Iterations) ->
    not_loaded(?LINE).


encode_tm1280_native(_Data) ->
    not_loaded(?LINE).
decode_tm1280_native(_Data, _Iterations) ->
    not_loaded(?LINE).


encode_tm1536_native(_Data) ->
    not_loaded(?LINE).
decode_tm1536_native(_Data, _Iterations) ->
    not_loaded(?LINE).


encode_tm2048_native(_Data) ->
    not_loaded(?LINE).
decode_tm2048_native(_Data, _Iterations) ->
    not_loaded(?LINE).


encode_tm5120_native(_Data) ->
    not_loaded(?LINE).
decode_tm5120_native(_Data, _Iterations) ->
    not_loaded(?LINE).


encode_tm6144_native(_Data) ->
    not_loaded(?LINE).
decode_tm6144_native(_Data, _Iterations) ->
    not_loaded(?LINE).


encode_tm8192_native(_Data) ->
    not_loaded(?LINE).
decode_tm8192_native(_Data, _Iterations) ->
    not_loaded(?LINE).

