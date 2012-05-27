-module(sse_supervisor).
-behaviour(supervisor).

%% API
-export([start_link/0,
	 start_link/1]).

%% Supervisor callbacks
-export([init/1]).

%% Helper macro for declaring children of supervisor
-define(CHILD(I, Type), {I, {I, start_link, []}, permanent, 5000, Type, [I]}).
-define(CHILD(I, T, P), {I, {I, start_link, P}, permanent, 5000, T, [I]}).

%% ===================================================================
%% API functions
%% ===================================================================

start_link() ->
    start_link([]).

start_link(Parameters) ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, Parameters).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init(Parameters) ->
    {ok, { {one_for_one, 5, 10}, children(Parameters)}}.

children(Parameters) ->
    [?CHILD(sse_hierarchy_gb_trees, worker, Parameters),
     ?CHILD(sse_monitor_node_server, worker),
     ?CHILD(sse_monitoring_server, worker),
     ?CHILD(sse_cowboy_http_supervisor, supervisor)].
    
