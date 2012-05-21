-module(sse_supervisor).
-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

%% Helper macro for declaring children of supervisor
-define(CHILD(I, Type), {I, {I, start_link, []}, permanent, 5000, Type, [I]}).

%% ===================================================================
%% API functions
%% ===================================================================

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init([]) ->
    {ok, { {one_for_one, 5, 10}, [?CHILD(sse_hierarchy_gb_trees, worker),
				  ?CHILD(sse_monitor_node_server, worker),
				  ?CHILD(sse_monitoring_server, worker),
				  ?CHILD(sse_cowboy_http_supervisor, supervisor)]}}.