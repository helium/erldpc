-module(erldpc).

%% API
-export([
         encode_tc128/1,
         decode_tc128/1, decode_tc128/2,
         encode_tc256/1,
         decode_tc256/1, decode_tc256/2,
         encode_tc512/1,
         decode_tc512/1, decode_tc512/2,
         encode_tm1280/1,
         decode_tm1280/1, decode_tm1280/2,
         encode_tm1536/1,
         decode_tm1536/1, decode_tm1536/2,
         encode_tm2048/1,
         decode_tm2048/1, decode_tm2048/2,
         encode_tm5120/1,
         decode_tm5120/1, decode_tm5120/2,
         encode_tm6144/1,
         decode_tm6144/1, decode_tm6144/2,
         encode_tm8192/1,
         decode_tm8192/1, decode_tm8192/2
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
    not_loaded(?LINE).

encode_tc256(_Data) ->
    not_loaded(?LINE).
decode_tc256(_Data) ->
    decode_tc256(_Data, ?ITERATIONS).
decode_tc256(_Data, _Iterations) ->
    not_loaded(?LINE).

encode_tc512(_Data) ->
    not_loaded(?LINE).
decode_tc512(_Data) ->
    decode_tc512(_Data, ?ITERATIONS).
decode_tc512(_Data, _Iterations) ->
    not_loaded(?LINE).

encode_tm1280(_Data) ->
    not_loaded(?LINE).
decode_tm1280(_Data) ->
    decode_tm1280(_Data, ?ITERATIONS).
decode_tm1280(_Data, _Iterations) ->
    not_loaded(?LINE).

encode_tm1536(_Data) ->
    not_loaded(?LINE).
decode_tm1536(_Data) ->
    decode_tm1536(_Data, ?ITERATIONS).
decode_tm1536(_Data, _Iterations) ->
    not_loaded(?LINE).

encode_tm2048(_Data) ->
    not_loaded(?LINE).
decode_tm2048(_Data) ->
    decode_tm2048(_Data, ?ITERATIONS).
decode_tm2048(_Data, _Iterations) ->
    not_loaded(?LINE).

encode_tm5120(_Data) ->
    not_loaded(?LINE).
decode_tm5120(_Data) ->
    decode_tm5120(_Data, ?ITERATIONS).
decode_tm5120(_Data, _Iterations) ->
    not_loaded(?LINE).

encode_tm6144(_Data) ->
    not_loaded(?LINE).
decode_tm6144(_Data) ->
    decode_tm6144(_Data, ?ITERATIONS).
decode_tm6144(_Data, _Iterations) ->
    not_loaded(?LINE).

encode_tm8192(_Data) ->
    not_loaded(?LINE).
decode_tm8192(_Data) ->
    decode_tm8192(_Data, ?ITERATIONS).
decode_tm8192(_Data, _Iterations) ->
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
