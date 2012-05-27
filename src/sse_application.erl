-module(sse_application).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
    sse_supervisor:start_link([application:get_all_env()]).

stop(_State) ->
    ok.
