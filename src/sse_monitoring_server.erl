%% Copyright (c) 2012, Peter Morgan <peter.james.morgan@gmail.com>
%%
%% Permission to use, copy, modify, and/or distribute this software for any
%% purpose with or without fee is hereby granted, provided that the above
%% copyright notice and this permission notice appear in all copies.
%%
%% THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
%% WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
%% MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
%% ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
%% WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
%% ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
%% OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.

-module(sse_monitoring_server).
-behaviour(gen_server).
-define(SERVER, ?MODULE).

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([start_link/0,
	 start_link/1]).

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
    start_link([]).

start_link(Args) ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, Args, []).

%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------

-record(state, {interval=5000,
		samples=[],
		counters=dict:new(),
		n=36,
		timer,
		topic,
		event
	       }).

init(Args) ->
    process_flag(trap_exit, true),
    init(Args, #state{}).

init([{monitoring, Parameters} | T], S) ->
    init(T, parameters(Parameters, S));
init([{_, _} | T], S) ->
    init(T, S);
init([], #state{interval = Interval} = S) ->
    {ok, Timer} = timer:send_interval(Interval, sample),
    {ok, update(S#state{timer = Timer})}.

parameters([{topic, Topic} | T], S) ->
    parameters(T, S#state{topic = Topic});
parameters([{event, Event} | T], S) ->
    parameters(T, S#state{event = Event});
parameters([{interval, Interval} | T], S) ->
    parameters(T, S#state{interval = Interval});
parameters([{n, N} | T], S) ->
    parameters(T, S#state{n = N});
parameters([], S) ->
    S.

handle_call(samples, _, #state{samples = Samples} = State) ->
    {reply, Samples, State}.

handle_cast({increment_counter, Counter}, #state{counters = Counters} = State) ->
    {noreply, State#state{counters = dict:update_counter(Counter, 1, Counters)}};

handle_cast({decrement_counter, Counter}, #state{counters = Counters} = State) ->
    {noreply, State#state{counters = dict:update_counter(Counter, -1, Counters)}}.

handle_info(sample, #state{topic = Topic, event = Event} = S1) ->
    S2 = #state{samples = Samples} = update(S1),
    sse_hierarchy:update(Topic, [{data, [{monitoring, Samples}]}, {event, Event}]),
    {noreply, S2}.

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
				  end, [], samples(S))}.


samples(#state{counters = Counters}) ->
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
     {reductions, Reductions} | dict:to_list(Counters)
    ].

