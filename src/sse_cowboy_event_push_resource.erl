-module(sse_cowboy_event_push_resource).
-export([init/3,
	 rest_init/2,
	 allowed_methods/2,
	 content_types_provided/2,
	 post_is_create/2,
	 process_post/2,
	 malformed_request/2]).


-record(state, {path, value}).

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
    case {path(Bindings), data(Bindings), event(Bindings)} of
	{undefined, _, _} ->
	    {true, R2, State};

	{_, undefined, undefined} ->
	    {true, R2, State};

	{Path, Data, undefined} ->
	    {false, R2, State#state{path = Path, value = [{data, Data}]}};

	{Path, undefined, Event} ->
	    {false, R2, State#state{path = Path, value = [{event, Event}]}};

	{Path, Data, Event} ->
	    {false, R2, State#state{path = Path, value = [{data, Data}, {event, Event}]}}
    end.

process_post(R, #state{path = Path, value = Value} = S) ->
    sse_hierarchy:update(binary:split(Path, <<"/">>, [global]), Value),
    {true, R,S}.

path(Bindings) ->
    proplists:get_value(<<"path">>, Bindings).

data(Bindings) ->
    proplists:get_value(<<"data">>, Bindings).

event(Bindings) ->
    proplists:get_value(<<"event">>, Bindings).
    
