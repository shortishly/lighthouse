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

-module(lighthouse_topic_event_stream_resource).
-export([
	 init/3,
	 info/3,
	 terminate/3
	 ]).


init(_, Req, []) ->
    {Path, Req2} = cowboy_req:path_info(Req),
    case lighthouse_node:where_is(lighthouse_topic:join(Path)) of
	undefined ->
	    {ok, Req3} = cowboy_req:reply(404, [{<<"content-type">>, <<"text/plain">>}], "Topic not found.", Req2),
	    {shutdown, Req3, no_state};

	Node ->
	    Headers = [{<<"content-type">>, <<"text/event-stream">>},
		       {<<"cache-control">>, <<"no-cache">>}],
	    {ok, Req3} = cowboy_req:chunked_reply(200, Headers, Req2),
	    lighthouse_node:subscribe(Node),
	    replay_events(Req3, Node, #{})
    end.

replay_events(Req, Node, State) ->
    case cowboy_req:header(<<"Last-Event-Id">>, Req) of
	{undefined, Req2} ->
	    {loop, Req2, State};

	{Value, Req2} ->
	    replay_events(Req2, lists:reverse(lighthouse_node:values(Node)), binary_to_integer(Value), State)
    end.

replay_events(Req, [{EventId, Event} | Events], LastEventId, State) when EventId > LastEventId ->
    case cowboy_req:chunk(["id: ", integer_to_list(EventId), "\ndata: ", Event, "\n\n"], Req) of
	ok ->
	    replay_events(Req, Events, LastEventId, State);
	_ ->
	    {shutdown, Req, State}
    end;
replay_events(Req, _, LastEventId, State) ->
    {loop, Req, State#{event_id => LastEventId}}.



info(#{event := {change, _}, module := lighthouse_node, counter := Counter}, Req, #{event_id := EventId} = State) when Counter < EventId ->
    {loop, Req, State};
info(#{event := {change, Property}, module := lighthouse_node, counter := Counter, value := Value}, Req, State) ->
    case cowboy_req:chunk(["id: ", integer_to_list(Counter), "\nevent: ", Property, "\ndata: ", Value, "\n\n"], Req) of
	ok ->
	    {loop, Req, State};
	_ ->
	    {shutdown, Req, State}
    end.
		

terminate(_, _, _) ->
    gproc:goodbye().
    


