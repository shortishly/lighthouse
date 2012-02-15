-module(impel_hierarchy_http_eventsource_emitter).
-behaviour(cowboy_http_handler).
-export([init/3, handle/2, terminate/2]).

-record(state, {node, handler}).

init({tcp, http}, R1, []) ->
    case cowboy_http_req:binding(node, R1) of
	{undefined, R2} ->
	    {ok, R3} = cowboy_http_req:reply(400, R2),
	    {shutdown, R3, undefined};

	{Node, R2} ->
	    Handler = {impel_hierarchy_http_eventsource_handler, {emitter, self()}},
	    impel_hierarchy_event:add_handler(Handler, [{emitter, self()}]),
	    {ok, R2, #state{node = Node, handler = Handler}}
    end.

handle(Req, State) ->
    Headers = [{'Content-Type', <<"text/event-stream">>}],
    {ok, Req2} = cowboy_http_req:chunked_reply(200, Headers, Req),
    handle_loop(Req2, State).

handle_loop(Req, State) ->
    receive
	shutdown ->
	    {ok, Req, State};
	
	{cowboy_http_req, resp_sent} ->
	    handle_loop(Req, State);
	
	{event, Event} ->
	    case cowboy_http_req:chunk(io_lib:format("id: ~p~ndata: ~p~n~n", [id(), Event]), Req) of
		{error, closed} ->
		    {ok, Req, State};
		
		ok ->
		    handle_loop(Req, State)
	    end
    end.

terminate(_Req, _State) ->
    ok.

id() ->
    {Mega, Sec, Micro} = erlang:now(),
    Id = (Mega * 1000000 + Sec) * 1000000 + Micro,
    integer_to_list(Id, 16).
