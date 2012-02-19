-module(impel_hierarchy).
-export([name/0,
	 children/0,
	 children/1,
	 update/2,
	 event_manager/1,
	 type/1,
	 created/1,
	 updated/1,
	 key/1,
	 stop/0]).

name() ->
    ?MODULE.

-type key() :: term().
-type value() :: term().
-opaque path() :: [key()].


-spec children() -> [term()].
children() ->
    children([]).

-spec children(path()) -> {ok, [term()]} | {error, not_found}.
children(Path) ->
    gen_server:call(name(), {children, Path}).

-spec update(path(), value()) -> ok.
update(Path, Value) ->
    gen_server:call(name(), {update, Path, Value}).

-spec event_manager(path()) -> pid().
event_manager(Path) ->
    gen_server:call(name(), {event_manager, Path}).



-spec type([term()]) -> term().
type(L) ->
    proplists:get_value(type, L).

-spec updated([term()]) -> calendar:datetime().
updated(L) ->
    proplists:get_value(updated, L).

-spec created([term()]) -> calendar:datetime().
created(L) ->
    proplists:get_value(created, L).

-spec key([term()]) -> term().
key(L) ->
    proplists:get_value(key, L).
			  


-spec stop() -> ok.
stop() ->
    gen_server:call(name(), stop).
    
