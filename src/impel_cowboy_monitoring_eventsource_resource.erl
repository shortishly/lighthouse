-module(impel_cowboy_monitoring_eventsource_resource).
-behaviour(cowboy_http_handler).
-export([init/3,
	 handle/2,
	 terminate/2]).

-record(state, {timeout = 5000}).

init({tcp, http}, R, []) ->
    {ok, R, #state{}}.

handle(Req, State) ->
    Headers = [{'Content-Type', <<"text/event-stream">>}],
    {ok, Req2} = cowboy_http_req:chunked_reply(200, Headers, Req),
    handle_loop(Req2, State).

handle_loop(Req, #state{timeout = Timeout} = State) ->
    receive
	shutdown ->
	    {ok, Req, State};
	
	{cowboy_http_req, resp_sent} ->
	    handle_loop(Req, State)

    after Timeout ->
	    case cowboy_http_req:chunk(io_lib:format("data: ~s~n~n", [jsx:to_json(monitor(), [])]), Req) of
		{error, closed} ->
		    {ok, Req, State};
		
		ok ->
		    handle_loop(Req, State)
	    end
    end.

terminate(_Req, _State) ->
    ok.

monitor() ->
    [{monitoring, impel_monitoring:samples()}].

