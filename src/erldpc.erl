-module(erldpc).

%% API
-export([encode/1]).

%% Native lib support
-export([load/0]).
-on_load(load/0).

encode(_Data) ->
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
