-module(impel_cowboy_http_static_resource).
-behaviour(cowboy_http_handler).
-export([init/3,
	 handle/2,
	 terminate/2]).

-record(state, {root}).

init({tcp, http} = Protocol, Req, Args) ->
    init(Protocol, Req, Args, #state{}).

init(Protocol, Req, [{root, Root} | T], State) ->
    init(Protocol, Req, T, State#state{root = Root});
init(_, Req, [], State) ->
    {ok, Req, State}.

handle(Req, #state{root = Root} = State) ->
    {[ _ | Paths], Req2} = cowboy_http_req:path(Req),
    Filename = filename:join([priv(), Root | [binary_to_list(Path) || Path <- Paths]]),
    error_logger:info_report([{module, ?MODULE},
			      {filename, Filename}]),
    case file:read_file(Filename) of
	{ok, Resource} ->
	    {ok, Req3} = cowboy_http_req:reply(200,
					       [{<<"Content-Type">>,
						 content_type(filename:extension(Filename))}],
					       Resource,
					       Req2),
	    {ok, Req3, State};

	{error, _} ->
	    {ok, Req3} = cowboy_http_req:reply(404, [], [], Req2),
	    {ok, Req3, State}
    end.

terminate(_, _) ->
    ok.


content_type(".html") ->
    <<"text/html">>;
content_type(".css") ->
    <<"text/css">>;
content_type(".js") ->
    <<"application/javascript">>.


priv() ->
  case code:priv_dir(impel) of
    {error,_} -> "priv";
    Priv -> Priv
  end.

    
