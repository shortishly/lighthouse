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

-module(sse_hierarchy_event).
-export([add_handler/2,
	 add_handler/3,
	 delete_handler/2,
	 delete_handler/3,
	 notify_update/4]).

-spec add_handler(pid(), gen_event:handler()) -> gen_event:add_handler_ret().
add_handler(Manager, Handler) ->
    add_handler(Manager, Handler, []).

-spec add_handler(pid(), gen_event:handler(), list()) -> gen_event:add_handler_ret().
add_handler(Manager, Handler, Args) ->
    sse_monitoring:increment_counter(hierarchy_event_handlers),
    gen_event:add_handler(Manager, Handler, Args).

-spec delete_handler(pid(), gen_event:handler()) -> gen_event:del_handler_ret().
delete_handler(Manager, Handler) ->
    delete_handler(Manager, Handler, []).

-spec delete_handler(pid(), gen_event:handler(), list()) -> gen_event:del_handler_ret().
delete_handler(Manager, Handler, Args) ->
    sse_monitoring:decrement_counter(hierarchy_event_handlers),
    gen_event:delete_handler(Manager, Handler, Args).

-spec notify_update(pid(), term(), term(), term()) -> ok.
notify_update(Manager, Path, Id, Value) ->
    sse_monitoring:increment_counter(hierarchy_event_updates),
    gen_event:notify(Manager, {update, Path, Id, Value}).
