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

-module(lighthouse_stats_supervisor).
-behaviour(supervisor).
-export([
	 start_link/0,
	 which_children/0
	]).
-export([
	 init/1
	]).

start_link() ->
    supervisor:start_link(ref(), ?MODULE, []).

ref() ->
    {via, gproc, {n, l, ?MODULE}}.

init([]) ->
    {ok, {{one_for_one, 1, 5}, [lighthouse_supervisor:worker(lighthouse_stats),
				lighthouse_supervisor:worker(lighthouse_stats_sampler)]}}.

which_children() ->
    supervisor:which_children(ref()).
    



