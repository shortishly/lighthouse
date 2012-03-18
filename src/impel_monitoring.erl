-module(impel_monitoring).
-export([samples/0]).

samples() ->
    gen_server:call(impel_monitoring_server, samples).

