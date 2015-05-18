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

-module(lighthouse_topic_resource).
-export([
	 init/3,
	 rest_init/2,
	 content_types_provided/2,
	 resource_exists/2,
	 to_atom/2
	]).


init(_, _, _) ->
    {upgrade, protocol, cowboy_rest}.

rest_init(Req, [root]) ->
    {ok, Req, #{node => lighthouse_node:root()}};

rest_init(Req, []) ->
    {ok, Req, #{}}.

content_types_provided(Req, State) ->
    {[{{<<"application">>, <<"atom+xml">>, '*'}, to_atom}], Req, State}.

resource_exists(Req, #{node := _} = State) ->
    {true, Req, State};
resource_exists(Req, State) ->
    {Path, Req2} = cowboy_req:path_info(Req),
    case lighthouse_node:where_is(lighthouse_topic:join(Path)) of
	undefined ->
	    {false, Req2, State};
	_ ->
	    {true, Req2, State#{node => lighthouse_node:node_ref(lighthouse_topic:join(Path))}}
    end.

to_atom(Req, #{node := Node} = State) ->
    {HostURL, Req2} = cowboy_req:host_url(Req),
    {{stream, atom_streamer(HostURL, Node)}, Req2, State}.

atom_streamer(HostURL, Node) ->
    fun(Socket, Transport) ->
	    header(Socket, Transport, Node),
	    entries(Socket, Transport, HostURL, lighthouse_node:edges_to_with_label(Node, parent)),
	    footer(Socket, Transport, Node)
    end.

-define(XML, "<?xml version='1.0' encoding='utf-8'?>").

header(Socket, Transport, Node) ->
    Transport:send(Socket, [?XML, "<feed xmlns='http://www.w3.org/2005/Atom'>", title(Node)]).

title(Node) ->
    ["<title>", name(Node), "</title>"].

name(Node) ->
    case lighthouse_node:name(Node) of
	root ->
	    "topics";
	Name ->
	    Name
    end.

entries(Socket, Transport, HostURL, Children) ->
    lists:foreach(fun(Child) -> Transport:send(Socket, entry(HostURL, Child)) end, Children).

entry(HostURL, Node) ->
    ["<entry>", title(Node), topic_url(HostURL, Node), stream_url(HostURL, Node), updated(Node), "</entry>"].

topic_url(HostURL, Node) ->
    ["<link href='", HostURL, "/topic/", lighthouse_node:name(Node), "' type='application/atom+xml'/>"].

stream_url(HostURL, Node) ->
    ["<link href='", HostURL, "/stream/", lighthouse_node:name(Node), "' type='text/event-stream'/>"].

updated(Node) ->
    ["<updated>", rfc3339(lighthouse_node:updated(Node)), "</updated>"].

rfc3339({{Year, Month, Day}, {Hour, Minute, Second}}) ->
    io_lib:format("~4..0w-~2..0w-~2..0wT~2..0w:~2..0w:~2..0wZ", [Year, Month, Day, Hour, Minute, Second]).

footer(Socket, Transport, _Node) ->
    Transport:send(Socket, ["</feed>"]).
