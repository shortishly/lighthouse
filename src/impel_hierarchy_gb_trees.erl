-module(impel_hierarchy_gb_trees).
-behaviour(gen_server).

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([start_link/0]).

%% ------------------------------------------------------------------
%% gen_server Function Exports
%% ------------------------------------------------------------------

-export([init/1,
	 handle_call/3,
	 handle_cast/2,
	 handle_info/2,
         terminate/2,
	 code_change/3]).

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------

start_link() ->
    gen_server:start_link({local, impel_hierarchy:name()}, ?MODULE, [], []).

%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------

-record(node, {tree = gb_trees:empty() :: gb_trees:tree(),
	       created = calendar:universal_time() :: calendar:datetime(),
	       updated = calendar:universal_time() :: calendar:datetime()}).
-record(leaf, {value :: term(),
	       event_manager :: pid(),
	       created = calendar:universal_time() :: calendar:datetime(),
	       updated = calendar:universal_time() :: calendar:datetime()}).
-record(state, {root = #node{}}).

init(Args) ->
    init(Args, #state{}).

init([], State) ->
    {ok, State}.

handle_call({children, Path}, Recipient, State) ->
    reply(Recipient, fun() -> children(Path, State) end, State);

handle_call({event_manager, Path}, Recipient, State) ->
    reply(Recipient, fun() -> event_manager(Path, State) end, State);

handle_call({update, Path, Value}, _, State) ->
    {reply, ok, update(Path, Value, State)};

handle_call(stop, _, _) ->
    {stop, normal, ok, undefined}.



handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------



children(Path, #state{root = Root}) ->
    children(Path, Root, []).

children([], #node{tree = Tree}, _) ->
    {ok, children(gb_trees:next(gb_trees:iterator(Tree)))};

children([H], #node{tree = Tree}, Steps) ->
    case gb_trees:lookup(H, Tree) of
	{value, SubNode} when is_record(SubNode, node) ->
	    children([], SubNode, [H | Steps]);
	
	{value, Leaf} when is_record(Leaf, leaf) ->
	    {error, {is_a_leaf, H, lists:reverse(Steps)}};

	none ->
	    {error, {not_found, H, lists:reverse(Steps)}}
    end;

children([H | T], #node{tree = Tree}, Steps) ->
    case gb_trees:lookup(H, Tree) of
	{value, SubNode} when is_record(SubNode, node) ->
	    children(T, SubNode, [H | Steps]);

	{value, Leaf} when is_record(Leaf, leaf) ->
	    {error, {is_a_leaf, H, lists:reverse(Steps)}};

	none ->
	    {error, {not_found, H, lists:reverse(Steps)}}
    end.


children({Key, V, Iterator}) ->
    [child(Key, V) | children(gb_trees:next(Iterator))];
children(none) ->
    [].

child(Key, V) when is_record(V, node) ->
    [{type, branch},
     {key, Key},
     {created, V#node.created},
     {updated, V#node.updated}];
child(Key, V) when is_record(V, leaf) ->
    [{type, leaf},
     {key, Key},
     {created, V#leaf.created},
     {updated, V#leaf.updated},
     {value, V#leaf.value}].

update(Path, V, #state{root = Root}) ->
    #state{root = update(Path, V, Root, [])}.

update([K], V, #node{tree = Tree} = Node, A) ->
    Updated = calendar:universal_time(),
    case gb_trees:lookup(K, Tree) of
	{value, #leaf{event_manager = EventManager} = Existing} ->
	    impel_hierarchy_event:notify_update(EventManager, lists:reverse([K | A]), V),
	    Node#node{tree = gb_trees:enter(K, Existing#leaf{value = V, updated = Updated}, Tree), updated = Updated};

	none ->
	    {ok, EventManager} = gen_event:start(),
	    Node#node{tree = gb_trees:enter(K, #leaf{value = V, event_manager = EventManager}, Tree), updated = Updated}
    end;
update([H | T], V, #node{tree = Tree} = Node, A) ->
    case gb_trees:lookup(H, Tree) of
	{value, SubNode} when is_record(SubNode, node) ->
	    Node#node{tree = gb_trees:update(H, update(T, V, SubNode, [H | A]), Tree)};

	none ->
	    Node#node{tree = gb_trees:insert(H, update(T, V, #node{}, [H | A]), Tree)}
    end.


event_manager(Path, #state{root = Root}) ->
    event_manager(Path, Root);
event_manager([H], #node{tree = Tree}) ->
    case gb_trees:lookup(H, Tree) of
	{value, #leaf{event_manager = EventManager}} ->
	    {ok, EventManager};
	none ->
	    {error, not_found}
    end;
event_manager([H | T], #node{tree = Tree}) ->
    case gb_trees:lookup(H, Tree) of
	{value, SubNode} when is_record(SubNode, node) ->
	    event_manager(T, SubNode);
	none ->
	    {error, not_found}
    end.





reply(Recipient, Response, State) ->
    spawn_link(fun() -> gen_server:reply(Recipient, Response()) end),
    {noreply, State}.

