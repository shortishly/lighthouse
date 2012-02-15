-module(impel_hierarchy_digraph).
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

-record(state, {root, graph}).

init(Args) ->
    G = digraph:new(),
    R = digraph:add_vertex(G),
    init(Args, #state{root = R, graph = G}).

init([], State) ->
    {ok, State}.

handle_call(children, Recipient, State) ->
    reply(Recipient, fun() -> children(State) end, State);
handle_call({children, Parent}, Recipient, State) ->
    reply(Recipient, fun() -> children(Parent, State) end, State);
handle_call({add_child, Label}, _, State) ->
    {reply, add_child(Label, State), State};
handle_call({add_child, Parent, Label}, _, State) ->
    {reply, add_child(Parent, Label, State), State};
handle_call({update_child, Child, Label}, _, State) ->
    {reply, update_child(Child, Label, State), State}.


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

children(#state{root = R} = S) ->
    children(R, S).

children(Parent, #state{graph = G}) ->
    [digraph:vertex(G, Child) || Child <- digraph:out_neighbours(G, Parent)].

add_child(Label, #state{root = R} = S) ->
    add_child(R, Label, S).

add_child(Parent, Label, #state{graph = G}) ->
    Child = digraph:add_vertex(G, digraph:add_vertex(G), Label),
    digraph:add_edge(G, Parent, Child),
    impel_hierarchy_event:notify_add_child(Parent, Child, Label),
    Child.

update_child(Child, Label, #state{graph = G}) ->
    digraph:add_vertex(G, Child, Label),
    impel_hierarchy_event:notify_update_child(Child, Label),
    Child.

reply(Recipient, Response, State) ->
    spawn_link(fun() -> gen_server:reply(Recipient, Response()) end),
    {noreply, State}.
