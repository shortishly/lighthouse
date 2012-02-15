-module(impel_hierarchy_event).
-export([add_handler/1,
	 add_handler/2,
	 notify_add_child/3,
	 notify_update_child/2,
	 manager/0]).

add_handler(Handler) ->
    add_handler(Handler, []).

add_handler(Handler, Args) ->
    gen_event:add_handler(manager(), Handler, Args).

notify_add_child(Parent, Child, Label) ->
    notify(manager(), {add_child, Parent, Child, Label}).

notify_update_child(Child, Label) ->
    notify(manager(), {update_child, Child, Label}).

manager() ->
    impel_hierarchy_event_manager.

notify(Manager, Message) ->
    gen_event:notify(Manager, Message).
