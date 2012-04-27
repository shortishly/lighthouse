-module(impel_cowboy_http_redirect_resource).
-behaviour(cowboy_http_handler).
-export([init/3,
	 handle/2,
	 terminate/2]).

-record(state, {location, status}).

init({tcp, http} = Protocol, Req, Args) ->
    init(Protocol, Req, Args, #state{}).

init(Protocol, Req, [{location, Location} | T], State) ->
    init(Protocol, Req, T, State#state{location = Location});
init(Protocol, Req, [{status, Status} | T], State) ->
    init(Protocol, Req, T, State#state{status = Status});
init(_, Req, [], State) ->
    {ok, Req, State}.


handle(Req, #state{location = Location, status = Status} = State) ->
    {ok, Req2} = cowboy_http_req:reply(Status, [{<<"Location">>, Location}], [], Req),
    {ok, Req2, State}.


terminate(_, _) ->
    ok.


