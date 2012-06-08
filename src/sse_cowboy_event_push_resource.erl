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

-module(sse_cowboy_event_push_resource).
-export([init/3,
	 rest_init/2,
	 allowed_methods/2,
	 content_types_provided/2,
	 post_is_create/2,
	 process_post/2,
	 malformed_request/2]).


-record(state, {topic, value}).

init({tcp, http}, _, _) ->
    {upgrade, protocol, cowboy_http_rest}.

rest_init(R, _) ->
    {ok, R, #state{}}.

allowed_methods(R, S) ->
    {['POST'], R, S}.

content_types_provided(R, S) ->
    {[{{<<"text">>, <<"html">>, []}, not_defined}], R, S}.

post_is_create(R, S) ->
    {false, R, S}.

malformed_request(R1, State) ->
    {Bindings, R2} = cowboy_http_req:body_qs(R1),
    case {topic(Bindings), data(Bindings), event(Bindings)} of
	{undefined, _, _} ->
	    {true, R2, State};

	{_, undefined, undefined} ->
	    {true, R2, State};

	{Topic, Data, undefined} ->
	    {false, R2, State#state{topic = Topic, value = [{data, Data}]}};

	{Topic, undefined, Event} ->
	    {false, R2, State#state{topic = Topic, value = [{event, Event}]}};

	{Topic, Data, Event} ->
	    {false, R2, State#state{topic = Topic, value = [{data, Data}, {event, Event}]}}
    end.

process_post(R, #state{topic = Topic, value = Value} = S) ->
    sse_hierarchy:update(binary:split(Topic, <<"/">>, [global]), Value),
    {true, R,S}.

topic(Bindings) ->
    proplists:get_value(<<"topic">>, Bindings).

data(Bindings) ->
    proplists:get_value(<<"data">>, Bindings).

event(Bindings) ->
    proplists:get_value(<<"event">>, Bindings).
    
