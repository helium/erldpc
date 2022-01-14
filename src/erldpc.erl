-module(erldpc).

%% API
-export([
         encode_tc128/1,
         decode_bf_tc128/1, decode_bf_tc128/2,
         decode_ms_tc128/1, decode_ms_tc128/2,
         decode_ms_soft_tc128/1, decode_ms_soft_tc128/2,
         encode_tc256/1,
         decode_bf_tc256/1, decode_bf_tc256/2,
         decode_ms_tc256/1, decode_ms_tc256/2,
         decode_ms_soft_tc256/1, decode_ms_soft_tc256/2,
         encode_tc512/1,
         decode_bf_tc512/1, decode_bf_tc512/2,
         decode_ms_tc512/1, decode_ms_tc512/2,
         decode_ms_soft_tc512/1, decode_ms_soft_tc512/2,
         encode_tm1280/1,
         decode_bf_tm1280/1, decode_bf_tm1280/2,
         decode_ms_tm1280/1, decode_ms_tm1280/2,
         decode_ms_soft_tm1280/1, decode_ms_soft_tm1280/2,
         encode_tm1536/1,
         decode_bf_tm1536/1, decode_bf_tm1536/2,
         decode_ms_tm1536/1, decode_ms_tm1536/2,
         decode_ms_soft_tm1536/1, decode_ms_soft_tm1536/2,
         encode_tm2048/1,
         decode_bf_tm2048/1, decode_bf_tm2048/2,
         decode_ms_tm2048/1, decode_ms_tm2048/2,
         decode_ms_soft_tm2048/1, decode_ms_soft_tm2048/2,
         encode_tm5120/1,
         decode_bf_tm5120/1, decode_bf_tm5120/2,
         decode_ms_tm5120/1, decode_ms_tm5120/2,
         decode_ms_soft_tm5120/1, decode_ms_soft_tm5120/2,
         encode_tm6144/1,
         decode_bf_tm6144/1, decode_bf_tm6144/2,
         decode_ms_tm6144/1, decode_ms_tm6144/2,
         decode_ms_soft_tm6144/1, decode_ms_soft_tm6144/2,
         encode_tm8192/1,
         decode_bf_tm8192/1, decode_bf_tm8192/2,
         decode_ms_tm8192/1, decode_ms_tm8192/2,
         decode_ms_soft_tm8192/1, decode_ms_soft_tm8192/2
        ]).

%% Native lib support
-export([load/0]).
-on_load(load/0).

-define(ITERATIONS, 20).

encode_tc128(_Data) ->
    not_loaded(?LINE).
decode_bf_tc128(_Data) ->
    decode_bf_tc128(_Data, ?ITERATIONS).
decode_bf_tc128(_Data, _Iterations) ->
    not_loaded(?LINE).
decode_ms_tc128(_Data) ->
    decode_ms_tc128(_Data, ?ITERATIONS).
decode_ms_tc128(_Data, _Iterations) ->
    not_loaded(?LINE).
decode_ms_soft_tc128(_List) ->
    decode_ms_soft_tc128(_List, ?ITERATIONS).
decode_ms_soft_tc128(_List, _Iterations) ->
    not_loaded(?LINE).

encode_tc256(_Data) ->
    not_loaded(?LINE).
decode_bf_tc256(_Data) ->
    decode_bf_tc256(_Data, ?ITERATIONS).
decode_bf_tc256(_Data, _Iterations) ->
    not_loaded(?LINE).
decode_ms_tc256(_Data) ->
    decode_ms_tc256(_Data, ?ITERATIONS).
decode_ms_tc256(_Data, _Iterations) ->
    not_loaded(?LINE).
decode_ms_soft_tc256(_List) ->
    decode_ms_soft_tc256(_List, ?ITERATIONS).
decode_ms_soft_tc256(_List, _Iterations) ->
    not_loaded(?LINE).

encode_tc512(_Data) ->
    not_loaded(?LINE).
decode_bf_tc512(_Data) ->
    decode_bf_tc512(_Data, ?ITERATIONS).
decode_bf_tc512(_Data, _Iterations) ->
    not_loaded(?LINE).
decode_ms_tc512(_Data) ->
    decode_ms_tc512(_Data, ?ITERATIONS).
decode_ms_tc512(_Data, _Iterations) ->
    not_loaded(?LINE).
decode_ms_soft_tc512(_List) ->
    decode_ms_soft_tc512(_List, ?ITERATIONS).
decode_ms_soft_tc512(_List, _Iterations) ->
    not_loaded(?LINE).

encode_tm1280(_Data) ->
    not_loaded(?LINE).
decode_bf_tm1280(_Data) ->
    decode_bf_tm1280(_Data, ?ITERATIONS).
decode_bf_tm1280(_Data, _Iterations) ->
    not_loaded(?LINE).
decode_ms_tm1280(_Data) ->
    decode_ms_tm1280(_Data, ?ITERATIONS).
decode_ms_tm1280(_Data, _Iterations) ->
    not_loaded(?LINE).
decode_ms_soft_tm1280(_List) ->
    decode_ms_soft_tm1280(_List, ?ITERATIONS).
decode_ms_soft_tm1280(_List, _Iterations) ->
    not_loaded(?LINE).

encode_tm1536(_Data) ->
    not_loaded(?LINE).
decode_bf_tm1536(_Data) ->
    decode_bf_tm1536(_Data, ?ITERATIONS).
decode_bf_tm1536(_Data, _Iterations) ->
    not_loaded(?LINE).
decode_ms_tm1536(_Data) ->
    decode_ms_tm1536(_Data, ?ITERATIONS).
decode_ms_tm1536(_Data, _Iterations) ->
    not_loaded(?LINE).
decode_ms_soft_tm1536(_List) ->
    decode_ms_soft_tm1536(_List, ?ITERATIONS).
decode_ms_soft_tm1536(_List, _Iterations) -> not_loaded(?LINE).

encode_tm2048(_Data) ->
    not_loaded(?LINE).
decode_bf_tm2048(_Data) ->
    decode_bf_tm2048(_Data, ?ITERATIONS).
decode_bf_tm2048(_Data, _Iterations) ->
    not_loaded(?LINE).
decode_ms_tm2048(_Data) ->
    decode_ms_tm2048(_Data, ?ITERATIONS).
decode_ms_tm2048(_Data, _Iterations) ->
    not_loaded(?LINE).
decode_ms_soft_tm2048(_List) ->
    decode_ms_soft_tm2048(_List, ?ITERATIONS).
decode_ms_soft_tm2048(_List, _Iterations) ->
    not_loaded(?LINE).

encode_tm5120(_Data) ->
    not_loaded(?LINE).
decode_bf_tm5120(_Data) ->
    decode_bf_tm5120(_Data, ?ITERATIONS).
decode_bf_tm5120(_Data, _Iterations) ->
    not_loaded(?LINE).
decode_ms_tm5120(_Data) ->
    decode_ms_tm5120(_Data, ?ITERATIONS).
decode_ms_tm5120(_Data, _Iterations) ->
    not_loaded(?LINE).
decode_ms_soft_tm5120(_List) ->
    decode_ms_soft_tm5120(_List, ?ITERATIONS).
decode_ms_soft_tm5120(_List, _Iterations) ->
    not_loaded(?LINE).

encode_tm6144(_Data) ->
    not_loaded(?LINE).
decode_bf_tm6144(_Data) ->
    decode_bf_tm6144(_Data, ?ITERATIONS).
decode_bf_tm6144(_Data, _Iterations) ->
    not_loaded(?LINE).
decode_ms_tm6144(_Data) ->
    decode_ms_tm6144(_Data, ?ITERATIONS).
decode_ms_tm6144(_Data, _Iterations) ->
    not_loaded(?LINE).
decode_ms_soft_tm6144(_List) ->
    decode_ms_soft_tm6144(_List, ?ITERATIONS).
decode_ms_soft_tm6144(_List, _Iterations) ->
    not_loaded(?LINE).

encode_tm8192(_Data) ->
    not_loaded(?LINE).
decode_bf_tm8192(_Data) ->
    decode_bf_tm8192(_Data, ?ITERATIONS).
decode_bf_tm8192(_Data, _Iterations) ->
    not_loaded(?LINE).
decode_ms_tm8192(_Data) ->
    decode_ms_tm8192(_Data, ?ITERATIONS).
decode_ms_tm8192(_Data, _Iterations) ->
    not_loaded(?LINE).
decode_ms_soft_tm8192(_List) ->
    decode_ms_soft_tm8192(_List, ?ITERATIONS).
decode_ms_soft_tm8192(_List,  _Iterations) ->
    not_loaded(?LINE).

load() ->
    erlang:load_nif(filename:join(priv(), "libnative"), none).

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
