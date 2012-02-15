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
		       {[<<"nodes">>], simple_http_eventsource_handler, []},
		       {[<<"node">>, node], simple_http_eventsource_handler, []},
		       {[<<"node">>, node, <<"live">>], impel_hierarchy_http_eventsource_emitter, []},
		       {'_', default_handler, []}
		      ]}
	       ],
    {ok, {{one_for_one, 5, 10},
	  [cowboy:child_spec(http, 100,
			     cowboy_tcp_transport, [{port, 8080}],
			     cowboy_http_protocol, [{dispatch, Dispatch}])]}}.
    
