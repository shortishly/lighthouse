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

-module(lighthouse_node_supervisor).
-behaviour(supervisor).
-export([
	 start_link/0,
	 start_child/0,
	 start_child/1,
	 start_child/2,
	 start_child/3,
	 which_children/0
	]).
-export([
	 init/1
	]).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
    {ok, {{simple_one_for_one, 1, 5}, [lighthouse_supervisor:worker(lighthouse_node, transient)]}}.

start_child() ->
    supervisor:start_child(?MODULE, []).

start_child(Name) ->
    supervisor:start_child(?MODULE, [Name]).

start_child(Name, Properties) ->
    supervisor:start_child(?MODULE, [Name, Properties]).

start_child(Name, Properties, Edges) ->
    supervisor:start_child(?MODULE, [Name, Properties, Edges]).

which_children() ->
    supervisor:which_children(?MODULE).
    



