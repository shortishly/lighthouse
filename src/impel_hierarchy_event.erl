-module(impel_hierarchy_event).
-export([add_handler/2,
	 add_handler/3,
	 delete_handler/2,
	 delete_handler/3,
	 notify_update/3]).

add_handler(Manager, Handler) ->
    add_handler(Manager, Handler, []).

add_handler(Manager, Handler, Args) ->
    gen_event:add_handler(Manager, Handler, Args).

delete_handler(Manager, Handler) ->
    delete_handler(Manager, Handler, []).

delete_handler(Manager, Handler, Args) ->
    gen_event:delete_handler(Manager, Handler, Args).

notify_update(Manager, Path, Value) ->
    notify(Manager, {update, Path, Value}).

notify(Manager, Message) ->
    gen_event:notify(Manager, Message).
