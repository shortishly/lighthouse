-module(impel_cowboy_http_supervisor).
-behaviour(supervisor).
-export([start_link/0, init/1]).


start_link() ->
    start_link([]).

start_link(P) ->
    supervisor:start_link(?MODULE, P).

init(_) ->
    Dispatch = [
		{'_', [
		       {[<<"static">>, '...'],
			impel_cowboy_http_static_resource,
			[{root, "static"}]},

		       {[<<"monitoring">>],
			impel_cowboy_monitoring_eventsource_resource,
			[]},

		       {[<<"admin">>],
			impel_cowboy_admin_resource,
			[]},

		       {[<<"nodes">>],
			impel_cowboy_nodes_resource,
			[]},

		       {[<<"node">>, '...'], 
			impel_cowboy_node_resource, 
			[]},

		       {[<<"es">>, '...'], 
			impel_cowboy_node_eventsource_resource, 
			[]}
		      ]}
	       ],
    {ok, {{one_for_one, 5, 10},
	  [cowboy:child_spec(http, 1000,
			     cowboy_tcp_transport, [{port, 8080},
						    {max_connections, 1000000}],
			     cowboy_http_protocol, [{dispatch, Dispatch}])]}}.
    
