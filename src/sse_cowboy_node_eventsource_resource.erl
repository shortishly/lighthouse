-module(sse_cowboy_node_eventsource_resource).
-behaviour(cowboy_http_handler).
-export([init/3,
	 handle/2,
	 terminate/2]).

-record(state, {handler, event_manager, timeout = 5000}).

init({tcp, http}, R1, []) ->
    sse_monitoring:increment_counter(eventsource_connections),
    {Path, R2} = cowboy_http_req:path_info(R1),
    EventManager = sse_hierarchy:event_manager(Path),
    Id = {emitter, self()},
    Handler = {sse_hierarchy_http_eventsource_handler, Id},
    sse_hierarchy_event:add_handler(EventManager, Handler, [Id]),
    {ok, R2, #state{handler = Handler, event_manager = EventManager}}.

handle(Req, State) ->
    Headers = [{'Content-Type', <<"text/event-stream">>},
	       {'Cache-Control', <<"no-cache">>}],
    {ok, Req2} = cowboy_http_req:chunked_reply(200, Headers, Req),
    handle_loop(Req2, State).

handle_loop(Req, #state{event_manager = EventManager, handler = Handler, timeout = Timeout} = State) ->
    receive
	shutdown ->
	    sse_hierarchy_event:delete_handler(EventManager, Handler),
	    {ok, Req, State};
	
	{cowboy_http_req, resp_sent} ->
	    handle_loop(Req, State);
	
	{event, {update, _, Value}} ->
	    case cowboy_http_req:chunk(io_lib:format("data: ~s~n~n", [jsx:to_json(Value, [])]), Req) of
		{error, closed} ->
		    sse_hierarchy_event:delete_handler(EventManager, Handler),
		    sse_monitoring:increment_counter(eventsource_outbound_messages_error_closed),
		    {ok, Req, State};
		
		ok ->
		    sse_monitoring:increment_counter(eventsource_outbound_messages_sent_ok),
		    handle_loop(Req, State)
	    end

    after Timeout ->
	    case cowboy_http_req:chunk(io_lib:format("data: ~p~n~n", ["ping"]), Req) of
		{error, closed} ->
		    sse_hierarchy_event:delete_handler(EventManager, Handler),
		    sse_monitoring:increment_counter(eventsource_outbound_ping_error_closed),
		    {ok, Req, State};
		
		ok ->
		    sse_monitoring:increment_counter(eventsource_outbound_ping_sent_ok),
		    handle_loop(Req, State)
	    end
    end.

terminate(_Req, _State) ->
    sse_monitoring:decrement_counter(eventsource_connections),
    ok.
