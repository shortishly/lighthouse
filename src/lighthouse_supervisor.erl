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

-module(lighthouse_supervisor).
-behaviour(supervisor).

-export([
	 start_link/0,
	 supervisor/1,
	 supervisor/2,
	 worker/1,
	 worker/2,
	 worker/3,
	 worker/4
	]).

-export([
	 init/1
	]).

start_link() ->
	supervisor:start_link({local, ?MODULE}, ?MODULE, []).

supervisor(Module) ->
    supervisor(Module, permanent).

supervisor(Module, Restart) ->
    {Module, {Module, start_link, []}, Restart, infinity, supervisor, [Module]}.

worker(Module) ->
    worker(Module, permanent).

worker(Module, Restart) ->
    worker(Module, Restart, []).

worker(Module, Restart, Parameters) ->
    worker(Module, Module, Restart, Parameters).    

worker(Id, Module, Restart, Parameters) ->
    {Id, {Module, start_link, Parameters}, Restart, 5000, worker, [Module]}.

init([]) ->
	Procs = [supervisor(lighthouse_node_supervisor), worker(lighthouse_node)],
	{ok, {{one_for_one, 1, 5}, Procs}}.
    
