-module(sse_hierarchy_gb_trees).
-behaviour(gen_server).

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([start_link/0,
	 start_link/1]).

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
    start_link([]).

start_link(Parameters) ->
    gen_server:start_link({local, sse_hierarchy:name()}, ?MODULE, Parameters, []).

%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------

-record(node, {tree = gb_trees:empty() :: gb_trees:tree(),
	       created = calendar:universal_time() :: calendar:datetime(),
	       updated = calendar:universal_time() :: calendar:datetime(),
	       samples,
	       id = 1,
	       values = []}).

-record(leaf, {created = calendar:universal_time() :: calendar:datetime(),
	       updated = calendar:universal_time() :: calendar:datetime(),
	       samples,
	       id = 1,
	       values = []}).

-record(event_manager, {hash :: number(), pid :: pid()}).

-record(state, {root = #node{},
		samples = 36,
		event_managers = ets:new(event_managers, [set,
							  protected,
							  {keypos, 2},
							  {heir, none},
							  {write_concurrency, false},
							  {read_concurrency, false}])}).


init(Args) ->
    process_flag(trap_exit, true),
    init(Args, #state{}).

init([{samples, Samples} | T], State) ->
    init(T, State#state{samples = Samples});
init([{included_applications, _} | T], State) ->
    init(T, State);
init([], State) ->
    {ok, State}.

handle_call({children, Path}, Recipient, State) ->
    reply(Recipient, fun() -> children(Path, State) end, State);

handle_call({event_manager, Path}, _, State) ->
    {reply, event_manager(Path, State), State};

handle_call({update, Path, Value}, _, State) ->
    {reply, ok, update(Path, Value, State)};

handle_call({values, Path}, Recipient, State) ->
    reply(Recipient, fun() -> values(Path, State) end, State);

handle_call(hierarchy, _, #state{root = Root} = State) ->
    {reply, #state{root = Root}, State};

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

handle_cast({update, Path, Value}, State) ->
    {noreply, update(Path, Value, State)};
handle_cast({delete, Path}, S1) ->
    case delete(Path, S1) of
	{ok, S2} ->
	    {noreply, S2};

	{error, _} ->
	    {noreply, S1}
    end.


handle_info(_Info, State) ->
    {noreply, State}.

terminate(_,  S) ->
    cleanup(S),
    ok.

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
	    merge(gb_trees:next(Iterator),
		  Recipient#node{tree = gb_trees:insert(Key, Leaf, Tree)})
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

child(Key, #node{created = Created, updated = Updated}) ->
    [{type, branch},
     {key, Key},
     {created, Created},
     {updated, Updated}];
child(Key, #leaf{created = Created, updated = Updated}) ->
    [{type, leaf},
     {key, Key},
     {created, Created},
     {updated, Updated}].


values(Path, #state{root = Root}) ->
    values(Path, Root);
values([K], #node{tree = Tree}) ->
    case gb_trees:lookup(K, Tree) of
	{value, #leaf{values = Values}} ->
	    {ok, Values};
	{value, #node{values = Values}} ->
	    {ok, Values};
	none ->
	    {error, not_found}
    end;
values([H | T], #node{tree = Tree}) ->
    case gb_trees:lookup(H, Tree) of
	{value, V} ->
	    values(T, V);
	none ->
	    {error, not_found}
    end.



update(Path, Value, #state{root = Root, samples = Samples} = S) ->
    S#state{root = update(Path, Root, event_manager(Path, S), Path, Value, Samples)}.

update([K], #node{tree = Tree} = Node, EventManager, Path, Value, Samples) ->
    Updated = calendar:universal_time(),
    case gb_trees:lookup(K, Tree) of
	{value, #leaf{id = Id, values = Values, samples = N} = Existing} ->
	    sse_hierarchy_event:notify_update(EventManager, Path, Id, Value),
	    Node#node{tree = gb_trees:enter(K, Existing#leaf{updated = Updated,
							     values = [{Id, Value} | lists:sublist(Values, N)],
							     id = Id + 1}, Tree), updated = Updated};

	{value, #node{id = Id, values = Values, samples = N} = Existing} ->
	    sse_hierarchy_event:notify_update(EventManager, Path, Id, Value),
	    Node#node{tree = gb_trees:enter(K, Existing#node{updated = Updated,
							     values = [{Id, Value} | lists:sublist(Values, N)],
							     id = Id + 1}, Tree)};

	none ->
	    Id = 1,
	    sse_hierarchy_event:notify_update(EventManager, Path, Id, Value),
	    Node#node{tree = gb_trees:enter(K, #leaf{id = Id + 1,
						     values = [{Id, Value}],
						     samples = Samples}, Tree), updated = Updated}
    end;
update([H | T], #node{tree = Tree} = Node, EventManager, Path, Value, Samples) ->
    case gb_trees:lookup(H, Tree) of
	{value, #node{} = SubNode} ->
	    Node#node{tree = gb_trees:update(H, update(T, SubNode, EventManager, Path, Value, Samples), Tree)};

	{value, #leaf{created = Created}} ->
	    Node#node{tree = gb_trees:update(H, update(T, #node{created = Created, samples = Samples}, EventManager, Path, Value, Samples), Tree)};

	none ->
	    Node#node{tree = gb_trees:insert(H, update(T, #node{samples = Samples}, EventManager, Path, Value, Samples), Tree)}
    end.

delete(Path, #state{root = R1, event_managers = EventManagers} = S) ->
    case delete(Path, R1, [], EventManagers) of
	{ok, R2} ->
	    {ok, S#state{root = R2}};

	{error, _} = Otherwise ->
	    Otherwise
    end.

delete([K], #node{tree = Tree} = Node, A, EventManagers) ->
    Updated = calendar:universal_time(),
    case gb_trees:lookup(K, Tree) of
	{value, #leaf{}} ->
	    delete_event_manager(lists:reverse([K | A]), EventManagers),
	    {ok, Node#node{tree = gb_trees:delete(K, Tree), updated = Updated}};

	{value, #node{tree = SubTree} = SubNode} when is_record(SubNode, node) ->
	    [delete([Key], SubNode, [K | A], EventManagers) || Key <- gb_trees:keys(SubTree)],
	    delete_event_manager(lists:reverse([K | A]), EventManagers),	    
	    {ok, Node#node{tree = gb_trees:delete(K, Tree), updated = Updated}};

	none ->
	    {error, {not_found, K, lists:reverse(A)}}
    end;
delete([H | T], #node{tree = Tree} = Node, A, EventManagers) ->
    case gb_trees:lookup(H, Tree) of
	{value, SubNode1} when is_record(SubNode1, node) ->
	    case delete(T, SubNode1, [H | A], EventManagers) of
		{ok, SubNode2} ->
		    {ok, Node#node{tree = gb_trees:update(H, SubNode2, Tree)}};
		
		{error, _} = Otherwise ->
		    Otherwise
	    end;

	none ->
	    {error, {not_found, H, lists:reverse(A)}}
    end.


event_manager(Path, #state{event_managers = EventManagers} = State) ->
    Hash = erlang:phash2(Path),
    case ets:lookup(EventManagers, Hash) of
	[#event_manager{pid = Pid}] ->
	    Pid;

	[] ->
	    {ok, Pid} = gen_event:start(),
	    case ets:insert_new(EventManagers, #event_manager{hash = Hash, pid = Pid}) of
		true ->
		    Pid;

		false ->
		    ok = gen_event:stop(Pid),
		    event_manager(Path, State)
	    end
    end.

delete_event_manager(Path, EventManagers) ->
    Hash = erlang:phash2(Path),
    case ets:lookup(EventManagers, Hash) of
	[#event_manager{pid = Pid}] ->
	    ok = gen_event:stop(Pid),
	    true = ets:delete(EventManagers, Hash);
	[] ->
	    ok
    end.


cleanup(#state{root = Root} = S) ->
    cleanup(Root, S).

cleanup(#node{tree = Tree}, S) ->
    cleanup(gb_trees:next(gb_trees:iterator(Tree)), S);

cleanup({Key, _, Iterator}, S1) ->
    {ok, S2} = delete([Key], S1),
    cleanup(gb_trees:next(Iterator), S2);

cleanup(none, S) ->
    S.




reply(Recipient, Response, State) ->
    spawn_link(fun() -> gen_server:reply(Recipient, Response()) end),
    {noreply, State}.

