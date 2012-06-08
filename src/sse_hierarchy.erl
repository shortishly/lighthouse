%% Copyright (c) 2012, Peter Morgan <peter.james.morgan@gmail.com>
%%
%% Permission to use, copy, modify, and/or distribute this software for any
%% purpose with or without fee is hereby granted, provided that the above
%% copyright notice and this permission notice appear in all copies.
%%
%% THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
%% WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
%% MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
%% ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
%% WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
%% ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
%% OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.

-module(sse_hierarchy).

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
	 values/1,
	 stop/0]).

%%
%% internal API only available to other members of the cluster
%%
-export([merge/1,
	 hierarchy/0]).

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
    gen_server:abcast(name(), {update, Path, Value}),
    ok.

-spec delete(path()) -> ok | {error, {not_found, term(), list()}}.
delete(Path) ->
    gen_server:abcast(name(), {delete, Path}),
    ok.

-spec event_manager(path()) -> {ok, pid()} | {error, term()}.
event_manager(Path) ->
    gen_server:call(name(), {event_manager, Path}).

-spec values(path()) -> {ok, term()} | {error, term()}.
values(Path) ->
    gen_server:call(name(), {values, Path}).

hierarchy() ->
    gen_server:call(name(), hierarchy).

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
    
