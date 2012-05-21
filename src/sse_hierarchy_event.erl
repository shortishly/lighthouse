-module(sse_hierarchy_event).
-export([add_handler/2,
	 add_handler/3,
	 delete_handler/2,
	 delete_handler/3,
	 notify_update/3]).

add_handler(Manager, Handler) ->
    add_handler(Manager, Handler, []).

add_handler(Manager, Handler, Args) ->
    sse_monitoring:increment_counter(hierarchy_event_handlers),
    gen_event:add_handler(Manager, Handler, Args).

delete_handler(Manager, Handler) ->
    delete_handler(Manager, Handler, []).

delete_handler(Manager, Handler, Args) ->
    sse_monitoring:decrement_counter(hierarchy_event_handlers),
    gen_event:delete_handler(Manager, Handler, Args).

notify_update(Manager, Path, Value) ->
    sse_monitoring:increment_counter(hierarchy_event_updates),
    notify(Manager, {update, Path, Value}).

notify(Manager, Message) ->
    gen_event:notify(Manager, Message).
