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

-module(lighthouse_stats_sampler).
-behaviour(gen_server).
-export([
	 start_link/0,
	 stop/0
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
    gen_server:start_link(ref(), ?MODULE, [], []).

stop() ->
    gen_server:cast(ref(), stop).

ref() ->
    {via, gproc, {n, l, ?MODULE}}.


init([]) ->
    case lighthouse:get_env(stats_sampler_timeout) of
	undefined ->
	    ignore;

	Timeout ->
	    {ok, #{}, list_to_integer(Timeout)}
    end.

handle_call(_, _, S) ->
    {stop, error, S}.

handle_cast(stop, S) ->
    {stop, normal, S}.

handle_info(timeout, S) ->
    case elastic:index_document(daily, "stats", jsx:encode(lighthouse_stats:sample())) of
	{ok, #{<<"created">> := true}} ->
	    {stop, normal, S};

	{error, Reason} ->
	    {stop, Reason, S}
    end.

code_change(_, State, _) ->
    {ok, State}.

terminate(_, _) ->
    gproc:goodbye().
