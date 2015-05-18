%% Copyright (c) 2012-2015 Peter Morgan <peter.james.morgan@gmail.com>
%%
%% Licensed under the Apache License, Version 2.0 (the "License");
%% you may not use this file except in compliance with the License.
%% You may obtain a copy of the License at
%%
%% http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing, software
%% distributed under the License is distributed on an "AS IS" BASIS,
%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%% See the License for the specific language governing permissions and
%% limitations under the License.

-module(lighthouse_node).
-behaviour(gen_server).
-export([
	 start_link/0,
	 start_link/1,
	 start_link/2,
	 start_link/3,
	 name/1,
	 samples/1,
	 samples/2,
	 values/1,
	 created/1,
	 updated/1,
	 properties/1,
	 property/3,
	 property/2,
	 edges/1,
	 edge/2,
	 add_edge/3,
	 edges_to/1,
	 edges_to_with_label/2,
	 stop/1,
	 subscribe/1,
	 subscribers/1,
	 node_ref/1,
	 where_is/1,
	 root/0,
	 counter/1
	]).
-export([
	 init/1,
	 code_change/3,
	 handle_call/3,
	 handle_cast/2,
	 handle_info/2,
	 terminate/2
	]).

-define(ROOT, root).

start_link() ->
    start_link(?ROOT).

start_link(Name) ->
    start_link(Name, #{}).

start_link(Name, Properties) ->
    start_link(Name, Properties, #{}).

start_link(Name, Properties, Edges) ->
    gen_server:start_link(node_ref(Name), ?MODULE, [Name, Properties, Edges], []).

name(Node) ->   
    gen_server:call(Node, name).

samples(Node) ->   
    gen_server:call(Node, samples).

samples(Node, Samples) ->   
    gen_server:call(Node, {samples, Samples}).

values(Node) ->   
    gen_server:call(Node, values).

created(Node) ->   
    gen_server:call(Node, created).

updated(Node) ->   
    gen_server:call(Node, updated).

properties(Node) ->
    gen_server:call(Node, properties).

property(Node, Key, Value) ->
    gen_server:call(Node, {property, Key, Value}).

property(Node, Key) ->
    gen_server:call(Node, {property, Key}).

edges(Node) ->
    gen_server:call(Node, edges).

add_edge(FromNode, Label, ToNode) ->
    gen_server:call(FromNode, {add_edge, Label, ToNode}).

edge(Node, Label) ->
    gen_server:call(Node, {edge, Label}).

edges_to(Node) ->
    gproc:lookup_local_properties({?MODULE, edge, lighthouse_node:name(Node)}).

edges_to_with_label(ToNode, Label) ->
    [FromNode || {FromNode, L} <- edges_to(ToNode), L == Label].

stop(Node) ->
    gen_server:cast(Node, stop).

subscribe(Node) ->
    gproc:reg({p, l, {?MODULE, node, lighthouse_node:name(Node)}}).

subscribers(Node) ->
    [Pid || {Pid, _} <- gproc:lookup_local_properties({?MODULE, node, lighthouse_node:name(Node)})].

root() ->
    node_ref(?ROOT).

node_ref(Name) ->
    {via, gproc, {n, l, {?MODULE, node, Name}}}.


init([Name, Properties, Edges]) ->
    lists:foreach(fun
		      ({Edge, ToNode}) ->
			  self() ! {add_edge, Edge, ToNode}
		  end,
		  maps:to_list(Edges)),
    Now = calendar:universal_time(),
    true = gproc:add_local_counter(counter(messages), 0),
    {ok, #{name => Name, properties => Properties, created => Now, updated => Now, counter => 0, values => [], samples => 0}}.

handle_call({edge, Label}, _, S) ->
    case gproc:select([{{{p, l, {?MODULE, edge, '$1'}}, self(), Label}, [], ['$1']}]) of
	[Node] ->
	    {reply, node_ref(Node), S};
	[] ->
	    {reply, error, S}
    end;
handle_call(name, _, #{name := Name} = S) ->
    {reply, Name, S};
handle_call(samples, _, #{samples := Samples} = S) ->
    {reply, Samples, S};
handle_call({samples, Samples}, _, S) ->
    {reply, S#{samples => Samples}, S};
handle_call(values, _, #{values := Values} = S) ->
    {reply, Values, S};
handle_call(created, _, #{created := Created} = S) ->
    {reply, Created, S};
handle_call(updated, _, #{updated := Updated} = S) ->
    {reply, Updated, S};
handle_call(properties, _, #{properties := Properties} = S) ->
    {reply, Properties, S};
handle_call({property, Key}, _, #{properties := Properties} = S) ->
    {reply, maps:find(Key, Properties), S};
handle_call({property, Key, Value}, _, #{properties := Properties, counter := Counter, samples := 0} = S) ->
    case maps:find(Key, Properties) of
	{ok, Value} ->
	    {reply, ok, S};
	_ ->
	    notify({change, Key}, Value, S),
	    {reply, ok, S#{properties := maps:put(Key, Value, Properties), updated => calendar:universal_time(), counter := Counter+1}}
    end;
handle_call({property, Key, Value}, _, #{properties := Properties, counter := Counter, values := Values, samples := Samples} = S) ->
    case maps:find(Key, Properties) of
	{ok, Value} ->
	    {reply, ok, S};
	_ ->
	    notify({change, Key}, Value, S),
	    {reply, ok, S#{properties := maps:put(Key, Value, Properties), updated => calendar:universal_time(), counter := Counter+1, values := [{Counter, Properties} | lists:sublist(Values, Samples)]}}
    end;
handle_call(edges, _, S) ->
    {reply, maps:from_list([{Edge, lighthouse_node:node_ref(Node)} || {Edge, Node} <- gproc:select([{{{p, l, {?MODULE, edge, '$2'}}, self(), '$1'}, [], [{{'$1', '$2'}}]}])]), S};
handle_call({add_edge, Label, ToNode}, _, S) ->
    gproc:add_local_property({?MODULE, edge, lighthouse_node:name(ToNode)}, Label),
    {reply, ok, S#{updated => calendar:universal_time()}}.

handle_cast(stop, S) ->
    {stop, normal, S}.

handle_info({add_edge, Label, ToNode}, S) ->
    gproc:add_local_property({?MODULE, edge, lighthouse_node:name(ToNode)}, Label),
    {noreply, S}.

code_change(_, State, _) ->
    {ok, State}.

terminate(_, _) ->
    gproc:goodbye().

notify(Event, Value, #{name := Name, counter := Counter, created := Created}) ->
    gproc:update_counter({c, l, counter(messages)}, 1),
    gproc:send({p, l, {?MODULE, node, Name}}, #{module => ?MODULE, from => self(), event => Event, value => Value, counter => Counter, created => Created}).

where_is(Label) ->
    lighthouse_util:where_is(node_ref(Label)).

counter(Type) ->
    {?MODULE, Type}.

