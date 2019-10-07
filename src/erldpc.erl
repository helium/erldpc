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

encode_tc128(_Data) ->
    not_loaded(?LINE).
decode_tc128(_Data) ->
    decode_tc128(_Data, ?ITERATIONS).
decode_tc128(_Data, _Iterations) ->
    decode_tc128(_Data).

encode_tc256(_Data) ->
    not_loaded(?LINE).
decode_tc256(_Data) ->
    decode_tc256(_Data, ?ITERATIONS).
decode_tc256(_Data, _Iterations) ->
    decode_tc256(_Data).

encode_tc512(_Data) ->
    not_loaded(?LINE).
decode_tc512(_Data) ->
    decode_tc512(_Data, ?ITERATIONS).
decode_tc512(_Data, _Iterations) ->
    decode_tc512(_Data).

encode_tm1280(_Data) ->
    not_loaded(?LINE).
decode_tm1280(_Data) ->
    decode_tm1280(_Data, ?ITERATIONS).
decode_tm1280(_Data, _Iterations) ->
    decode_tm1280(_Data).

encode_tm1536(_Data) ->
    not_loaded(?LINE).
decode_tm1536(_Data) ->
    decode_tm1536(_Data, ?ITERATIONS).
decode_tm1536(_Data, _Iterations) ->
    decode_tm1536(_Data).

encode_tm2048(_Data) ->
    not_loaded(?LINE).
decode_tm2048(_Data) ->
    decode_tm2048(_Data, ?ITERATIONS).
decode_tm2048(_Data, _Iterations) ->
    decode_tm2048(_Data).

encode_tm5120(_Data) ->
    not_loaded(?LINE).
decode_tm5120(_Data) ->
    decode_tm5120(_Data, ?ITERATIONS).
decode_tm5120(_Data, _Iterations) ->
    decode_tm5120(_Data).

encode_tm6144(_Data) ->
    not_loaded(?LINE).
decode_tm6144(_Data) ->
    decode_tm6144(_Data, ?ITERATIONS).
decode_tm6144(_Data, _Iterations) ->
    decode_tm6144(_Data).

encode_tm8192(_Data) ->
    not_loaded(?LINE).
decode_tm8192(_Data) ->
    decode_tm8192(_Data, ?ITERATIONS).
decode_tm8192(_Data, _Iterations) ->
    decode_tm8192(_Data).

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
