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

-module(lighthouse_stats).
-behaviour(gen_server).
-export([
	 start_link/0,
	 stop/0,
	 sample/0
	]).
-export([
	 init/1,
	 code_change/3,
	 handle_call/3,
	 handle_cast/2,
	 handle_info/2,
	 terminate/2
	]).


start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

sample() ->
    gen_server:call(?MODULE, sample).

stop() ->
    gen_server:cast(?MODULE, stop).

init([]) ->
    true = gproc:add_local_aggr_counter(lighthouse_topic_event_stream_resource:counter(messages)),
    true = gproc:add_local_aggr_counter(lighthouse_topic_event_stream_resource:counter(streams)),
    true = gproc:add_local_aggr_counter(lighthouse_node:counter(messages)),
    {ok, #{}}.

handle_call(sample, _, S) ->
    {reply, statistics(), S}.

handle_cast(stop, S) ->
    {stop, normal, S}.

handle_info(_, S) ->
    {stop, error, S}.

code_change(_, State, _) ->
    {ok, State}.

terminate(_, _) ->
    gproc:goodbye().

statistics() ->
    #{
       graph => #{
	 nodes => length(lighthouse_graph:nodes()),
	 messages => gproc:lookup_local_aggr_counter(lighthouse_node:counter(messages)),
	 edges => length(lighthouse_graph:edges())
	},
       
       stream => #{
	 messages => gproc:lookup_local_aggr_counter(lighthouse_topic_event_stream_resource:counter(messages)),
	 streams => gproc:lookup_local_aggr_counter(lighthouse_topic_event_stream_resource:counter(streams))
	}
     }.
      
