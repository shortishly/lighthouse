
-module(impel_supervisor).

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
    {ok, { {one_for_one, 5, 10}, [?CHILD(impel_hierarchy_digraph, worker),
				  ?CHILD(impel_webmachine_http_supervisor, supervisor),
				  ?CHILD(impel_cowboy_http_supervisor, supervisor),
				  event_manager()]}}.

event_manager() ->
    {event_manager,
     {gen_event, start_link, [{local, impel_hierarchy_event:manager()}]},
      permanent,
      5000,
      worker,
      dynamic}. 
