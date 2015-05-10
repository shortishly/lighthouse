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

-module(lighthouse_graph).
-export([
	 nodes/0,
	 edges/0
	]).


nodes() ->
    [lighthouse_node:node_ref(Name) || Name <- gproc:select([{{{n, l, {lighthouse_node, node, '$1'}}, '_', '_'}, [], ['$1']}])].		

edges() ->
    [#{from => lighthouse_node:node_ref(lighthouse_node:name(From)), label => Label, to => lighthouse_node:node_ref(To)} || {From, Label, To} <- gproc:select([{{{p, l, {lighthouse_node, edge, '$1'}}, '$2', '$3'}, [], [{{'$2', '$3', '$1'}}]}])].		
