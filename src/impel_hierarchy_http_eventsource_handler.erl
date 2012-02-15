-module(impel_hierarchy_http_eventsource_handler).
-behaviour(gen_event).
-export([init/1,
	 terminate/2,
	 handle_info/2,
	 handle_event/2]).

-record(state, {emitter}).

init(P) ->
    init(P, #state{}).

init([{emitter, Emitter} | T], State) ->
    init(T, State#state{emitter = Emitter});
init([], State) ->
    {ok, State}.

handle_event(Event, #state{emitter = Emitter} = S) ->
    Emitter ! {event, Event},
    {ok, S}.

terminate(remove_handler, _) ->
    ok.

handle_info({'EXIT', _, shutdown}, _) ->
    remove_handler.


