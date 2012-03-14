-module(impel_hierarchy).

%%
%% external API
%%
-export([name/0,
	 children/0,
	 children/1,
	 update/2,
	 delete/1,
	 event_manager/1,
	 type/1,
	 created/1,
	 updated/1,
	 key/1,
	 stop/0]).

%%
%% internal API only available to other members of the cluster
%%
-export([merge/1,
	 state/0]).

name() ->
    ?MODULE.

-type key() :: term().
-type value() :: term().
-opaque path() :: [key()].


-spec children() -> [term()].
children() ->
    children([]).

-spec children(path()) -> {ok, [term()]} | {error, {not_found, term(), list()}}.
children(Path) ->
    gen_server:call(name(), {children, Path}).

-spec update(path(), value()) -> ok.
update(Path, Value) ->
    gen_server:abcast(name(), {update, Path, Value}).

-spec delete(path()) -> ok | {error, {not_found, term(), list()}}.
delete(Path) ->
    gen_server:abcast(name(), {delete, Path}).

-spec event_manager(path()) -> {ok, pid()} | {error, term()}.
event_manager(Path) ->
    gen_server:call(name(), {event_manager, Path}).

state() ->
    gen_server:call(name(), state).

merge(State) ->
    gen_server:call(name(), {merge, State}).



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
    
