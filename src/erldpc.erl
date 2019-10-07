-module(erldpc).

%% API
-export([
         encode_tc128/1,
         encode_tc256/1,
         encode_tc512/1,
         encode_tm1280/1,
         encode_tm1536/1,
         encode_tm2048/1,
         encode_tm5120/1,
         encode_tm6144/1,
         encode_tm8192/1
        ]).

%% Native lib support
-export([load/0]).
-on_load(load/0).

encode_tc128(_Data) ->
    not_loaded(?LINE).

encode_tc256(_Data) ->
    not_loaded(?LINE).

encode_tc512(_Data) ->
    not_loaded(?LINE).

encode_tm1280(_Data) ->
    not_loaded(?LINE).

encode_tm1536(_Data) ->
    not_loaded(?LINE).

encode_tm2048(_Data) ->
    not_loaded(?LINE).

encode_tm5120(_Data) ->
    not_loaded(?LINE).

encode_tm6144(_Data) ->
    not_loaded(?LINE).

encode_tm8192(_Data) ->
    not_loaded(?LINE).

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
