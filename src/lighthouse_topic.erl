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

-module(lighthouse_topic).
-export([
	 walk_to/1,
	 ensure_present/1,
	 path_to/1,
	 join/1
	]).


walk_to(Topic) ->
    walk_to(path_to(Topic), lighthouse_node:root()).

walk_to([Label | Path], Parent) ->
    case lighthouse_node:edge(lighthouse_node:node_ref(Label), parent) of
	error ->
	    {error, Label};
	Parent ->
	    walk_to(Path, lighthouse_node:node_ref(Label))
    end;
walk_to([], Node) ->
    Node.

ensure_present(Topic) ->
    lists:foldl(fun
		    (Name, Parent) ->
			case lighthouse_node_supervisor:start_child(Name, #{}, #{parent => Parent}) of
			    {ok, _} ->
				lighthouse_node:node_ref(Name);
			    {error, {already_started, _}} ->
				lighthouse_node:node_ref(Name)
			end
		end,
		lighthouse_node:root(),
		path_to(Topic)).
		
path_to(Topic) ->
    lists:reverse(lists:foldl(fun(Node, []) ->
				      [Node];
				 (Node, [Parent | GrandParent]) ->
				      [<<Parent/binary, "/", Node/binary>>, Parent | GrandParent]
			      end,
			      [],
			      binary:split(Topic, <<"/">>, [global]))).

join(Elements) ->
    lists:foldl(fun
		    (Element, <<>>) ->
			Element;
		    (Element, A) ->
			<<A/binary, "/", Element/binary>>
		end,
		<<>>,
		Elements).

