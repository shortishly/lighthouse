-module(impel_monitoring).
-export([increment_counter/1,
	 decrement_counter/1,
	 samples/0]).

increment_counter(Counter) ->
    gen_server:cast(server(), {increment_counter, Counter}).

decrement_counter(Counter) ->
    gen_server:cast(server(), {decrement_counter, Counter}).

samples() ->
    gen_server:call(server(), samples).

server() ->
    impel_monitoring_server.

