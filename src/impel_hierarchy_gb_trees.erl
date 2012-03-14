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
-record(leaf, {event_manager :: pid(),
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

handle_call(state, _, State) ->
    {reply, State, State};

handle_call({delete, Path}, _, S1) ->
    case delete(Path, S1) of
	{ok, S2} ->
	    {reply, ok, S2};

	{error, _} = Otherwise ->
	    {reply, Otherwise, S1}
    end;

handle_call({merge, Donor}, _, State) ->
    {reply, ok, State#state{root = merge(Donor, State)}};

handle_call(stop, _, State) ->
    {stop, normal, ok, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_,  S) ->
    cleanup(S),
    ok.

cleanup(#state{root = Root} = S) ->
    cleanup(Root, S).

cleanup(#node{tree = Tree}, S) ->
    cleanup(gb_trees:next(gb_trees:iterator(Tree)), S);

cleanup({Key, _, Iterator}, S1) ->
    {ok, S2} = delete([Key], S1),
    cleanup(gb_trees:next(Iterator), S2);

cleanup(none, S) ->
    S.


code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------


merge(#state{root = Donor}, #state{root = Recipient}) ->
    merge(Donor, Recipient);
merge(#node{tree = Donor}, #node{} = Recipient) ->
    merge(gb_trees:next(gb_trees:iterator(Donor)), Recipient);
merge({Key, #leaf{} = Leaf, Iterator}, #node{tree = Tree} = Recipient) ->
    case gb_trees:lookup(Key, Tree) of
	{value, #leaf{}} ->
	    merge(gb_trees:next(Iterator), Recipient);
	none ->
	    {ok, EventManager} = gen_event:start(),
	    merge(gb_trees:next(Iterator),
		  Recipient#node{tree = gb_trees:insert(Key, Leaf#leaf{event_manager = EventManager}, Tree)})
    end;
merge({Key, #node{} = Node, Iterator}, #node{tree = Tree} = Recipient) ->
    case gb_trees:lookup(Key, Tree) of
	{value, #node{} = SubTree} ->
	    merge(gb_trees:next(Iterator),
		  Recipient#node{tree = gb_trees:update(Key, merge(Node, SubTree), Tree)});
	none ->
	    merge(gb_trees:next(Iterator),
		  Recipient#node{tree = gb_trees:insert(Key, merge(Node, #node{}), Tree)})
    end;
merge(none, Recipient) ->
    Recipient.




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
     {updated, V#leaf.updated}].

update(Path, V, #state{root = Root} = S) ->
    S#state{root = update(Path, V, Root, [])}.

update([K], V, #node{tree = Tree} = Node, A) ->
    Updated = calendar:universal_time(),
    case gb_trees:lookup(K, Tree) of
	{value, #leaf{event_manager = EventManager} = Existing} ->
	    impel_hierarchy_event:notify_update(EventManager, lists:reverse([K | A]), V),
	    Node#node{tree = gb_trees:enter(K, Existing#leaf{updated = Updated}, Tree), updated = Updated};

	none ->
	    {ok, EventManager} = gen_event:start(),
	    Node#node{tree = gb_trees:enter(K, #leaf{event_manager = EventManager}, Tree), updated = Updated}
    end;
update([H | T], V, #node{tree = Tree} = Node, A) ->
    case gb_trees:lookup(H, Tree) of
	{value, SubNode} when is_record(SubNode, node) ->
	    Node#node{tree = gb_trees:update(H, update(T, V, SubNode, [H | A]), Tree)};

	none ->
	    Node#node{tree = gb_trees:insert(H, update(T, V, #node{}, [H | A]), Tree)}
    end.

delete(Path, #state{root = R1} = S) ->
    case delete(Path, R1, []) of
	{ok, R2} ->
	    {ok, S#state{root = R2}};

	{error, _} = Otherwise ->
	    Otherwise
    end.

delete([K], #node{tree = Tree} = Node, A) ->
    Updated = calendar:universal_time(),
    case gb_trees:lookup(K, Tree) of
	{value, #leaf{event_manager = EventManager}} ->
	    ok = gen_event:stop(EventManager),
	    {ok, Node#node{tree = gb_trees:delete(K, Tree), updated = Updated}};

	{value, #node{tree = SubTree} = SubNode} when is_record(SubNode, node) ->
	    [delete([Key], SubNode, [K | A]) || Key <- gb_trees:keys(SubTree)],
	    {ok, Node#node{tree = gb_trees:delete(K, Tree), updated = Updated}};

	none ->
	    {error, {not_found, K, lists:reverse(A)}}
    end;
delete([H | T], #node{tree = Tree} = Node, A) ->
    case gb_trees:lookup(H, Tree) of
	{value, SubNode1} when is_record(SubNode1, node) ->
	    case delete(T, SubNode1, [H | A]) of
		{ok, SubNode2} ->
		    {ok, Node#node{tree = gb_trees:update(H, SubNode2, Tree)}};
		
		{error, _} = Otherwise ->
		    Otherwise
	    end;

	none ->
	    {error, {not_found, H, lists:reverse(A)}}
    end.




event_manager(Path, #state{root = Root}) ->
    event_manager(Path, Root);
event_manager([H], #node{tree = Tree}) ->
    case gb_trees:lookup(H, Tree) of
	{value, #leaf{event_manager = EventManager}} ->
	    {ok, EventManager};

	{value, #node{}} ->
	    {error, not_a_leaf};

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

