-module(sse_cowboy_http_supervisor).
-behaviour(supervisor).
-export([start_link/0, init/1]).


start_link() ->
    start_link([]).

start_link(P) ->
    supervisor:start_link(?MODULE, P).

init(_) ->
    Dispatch = [
		{'_', [
		       {[],
			sse_cowboy_http_redirect_resource,
			[{location, <<"/static/index.html">>},
			 {status, 302}]},

		       {[<<"static">>, '...'],
			sse_cowboy_http_static_resource,
			[{root, "static"}]},

		       {[<<"monitoring">>],
			sse_cowboy_monitoring_eventsource_resource,
			[]},

		       {[<<"event">>, <<"push">>],
			sse_cowboy_event_push_resource,
			[]},

		       {[<<"admin">>],
			sse_cowboy_admin_resource,
			[]},

		       {[<<"nodes">>],
			sse_cowboy_nodes_resource,
			[]},

		       {[<<"node">>, '...'], 
			sse_cowboy_node_resource, 
			[]},

		       {[<<"es">>, '...'], 
			sse_cowboy_node_eventsource_resource, 
			[]}
		      ]}
	       ],
    {ok, {{one_for_one, 5, 10},
	  [cowboy:child_spec(http, 1000,
			     cowboy_tcp_transport, [{port, 8080},
						    {max_connections, 1000000}],
			     cowboy_http_protocol, [{dispatch, Dispatch}])]}}.
    
