-module(impel_webmachine_http_supervisor).
-behaviour(supervisor).

%% External exports
-export([start_link/0]).

%% supervisor callbacks
-export([init/1]).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
    {ok, Dispatch} = case code:priv_dir(impel) of
			 {error, _} ->
			     file:consult(filename:join(
					    [filename:dirname(code:which(?MODULE)),
					     "..", "priv", "dispatch.conf"]));
			 Directory ->
			     file:consult(filename:join([Directory,"dispatch.conf"]))
		     end,
    WebConfig = [
                 {ip, "127.0.0.1"},
                 {port, 8000},
                 {log_dir, "priv/log"},
                 {dispatch, Dispatch}],
    Web = {webmachine_mochiweb,
           {webmachine_mochiweb, start, [WebConfig]},
           permanent, 5000, worker, dynamic},
    Processes = [Web],
    {ok, { {one_for_one, 10, 10}, Processes} }.
