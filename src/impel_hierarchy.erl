-module(impel_hierarchy).
-export([name/0,
	 children/0,
	 children/1,
	 add_child/1,
	 add_child/2,
	 update_child/2,
	 stop/0]).

name() ->
    ?MODULE.

-opaque child() :: term().


-spec children() -> [child()].
children() ->
    gen_server:call(name(), children).

-spec children(child()) -> [child()].
children(Parent) ->
    gen_server:call(name(), {children, Parent}).

-spec add_child(term()) -> child().
add_child(Label) ->
    gen_server:call(name(), {add_child, Label}).

-spec add_child(child(), term()) -> child().
add_child(Parent, Label) ->
    gen_server:call(name(), {add_child, Parent, Label}).

-spec update_child(child(), term()) -> ok.
update_child(Child, Label) ->
    gen_server:call(name(), {update_child, Child, Label}).

-spec stop() -> ok.
stop() ->
    gen_server:call(name(), stop).
    
