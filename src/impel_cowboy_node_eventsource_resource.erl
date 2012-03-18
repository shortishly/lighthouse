-module(impel_cowboy_node_eventsource_resource).
-behaviour(cowboy_http_handler).
-export([init/3,
	 handle/2,
	 terminate/2]).

-record(state, {handler, event_manager, timeout = 5000}).

init({tcp, http}, R1, []) ->
    impel_monitoring:increment_counter(connections),
    {Path, R2} = cowboy_http_req:path_info(R1),
    case impel_hierarchy:event_manager(Path) of
	{ok, EventManager} ->
	    Id = {emitter, self()},
	    Handler = {impel_hierarchy_http_eventsource_handler, Id},
	    impel_hierarchy_event:add_handler(EventManager, Handler, [Id]),
	    {ok, R2, #state{handler = Handler, event_manager = EventManager}};

	{error, not_found} ->
	    {ok, R3} = cowboy_http_req:reply(404, R2),
	    {shutdown, R3, undefined}
    end.

handle(Req, State) ->
    Headers = [{'Content-Type', <<"text/event-stream">>}],
    {ok, Req2} = cowboy_http_req:chunked_reply(200, Headers, Req),
    handle_loop(Req2, State).

handle_loop(Req, #state{event_manager = EventManager, handler = Handler, timeout = Timeout} = State) ->
    receive
	shutdown ->
	    impel_hierarchy_event:delete_handler(EventManager, Handler),
	    {ok, Req, State};
	
	{cowboy_http_req, resp_sent} ->
	    handle_loop(Req, State);
	
	{event, {update, _, Value}} ->
	    case cowboy_http_req:chunk(io_lib:format("data: ~s~n~n", [jsx:to_json(Value, [space, indent])]), Req) of
		{error, closed} ->
		    impel_hierarchy_event:delete_handler(EventManager, Handler),
		    {ok, Req, State};
		
		ok ->
		    impel_monitoring:increment_counter(outbound_messages),
		    handle_loop(Req, State)
	    end

    after Timeout ->
	    case cowboy_http_req:chunk(io_lib:format("data: ~p~n~n", ["ping"]), Req) of
		{error, closed} ->
		    impel_hierarchy_event:delete_handler(EventManager, Handler),
		    {ok, Req, State};
		
		ok ->
		    handle_loop(Req, State)
	    end
    end.

terminate(_Req, _State) ->
    impel_monitoring:decrement_counter(connections),
    ok.
