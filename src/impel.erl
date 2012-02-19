-module(impel).
-export([start/0,
	make/0]).

start() ->
    ok = ensure(cowboy),
    ok = application:start(impel).

ensure(Application) ->
    ensure_started(application:start(Application)).

ensure_started({error, {already_started, _}}) ->
    ok;
ensure_started(ok) ->
    ok.


make() ->
    make:all([load]).
