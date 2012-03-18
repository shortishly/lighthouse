-module(impel_monitoring_server).
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

-record(state, {interval=5000,
		samples=[],
		n=36,
		timer
	       }).

init(Args) ->
    init(Args, #state{}).

init([], #state{interval=Interval} = S) ->
    {ok, Timer} = timer:send_interval(Interval, sample),
    {ok, update(S#state{timer = Timer})}.


handle_call(samples, _, #state{samples = Samples} = State) ->
    {reply, Samples, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(sample, State) ->
    {noreply, update(State)}.

terminate(_, #state{timer = Timer}) ->
    {ok, cancel} = timer:cancel(Timer),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------

update(#state{samples = Samples, n = N} = S) ->
    S#state{samples = lists:foldl(fun({Key, Sample}, A) ->
					  [{Key, [Sample | lists:sublist(proplists:get_value(Key, Samples, []), N)]} | A]
				  end, [], samples())}.


samples() ->
    RunQueue = statistics(run_queue),
    {{input, Input}, {output, Output}} = statistics(io),
    {ContextSwitches, 0} = statistics(context_switches),
    {TotalReductions, Reductions} = statistics(reductions),
    [{processes, length(processes())},
     {run_queue, RunQueue},
     {input_bytes, Input},
     {output_bytes, Output},
     {context_switches, ContextSwitches},
     {total_reductions, TotalReductions},
     {reductions, Reductions}
    ].

