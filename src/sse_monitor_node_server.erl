-module(sse_monitor_node_server).
-behaviour(gen_server).
-define(SERVER, ?MODULE).

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
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------

init(Args) ->
    process_flag(trap_exit, true),
    ok = net_kernel:monitor_nodes(true),
    {ok, Args}.

handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info({nodeup, Node}, State) ->
    error_logger:info_report([{module, ?MODULE},
			      {nodeup, Node}]),
    nodeup(Node),
    {noreply, State};

handle_info({nodedown, Node}, State) ->
    error_logger:info_report([{module, ?MODULE},
			      {nodedown, Node}]),
    {noreply, State}.


terminate(_Reason, _State) ->
    net_kernel:monitor_nodes(false).

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------

nodeup(Node) ->
    case is_sse_node(Node) of
	true ->
	    sse_hierarchy:merge(rpc:call(Node, sse_hierarchy, hierarchy, []));
	_ ->
	    nop
    end.

is_sse_node(Node) ->
    rpc:call(Node, code, which, [?MODULE]) =/= non_existing.
    
