-module(impel_cowboy_node_resource).

-export([init/3,
	 rest_init/2,
	 resource_exists/2,
	 content_types_provided/2,
	 to_atom/2]).

-record(state, {children}).


init({tcp, http}, _, []) ->
    {upgrade, protocol, cowboy_http_rest}.

rest_init(Req, _) ->
    {ok, Req, #state{}}.

resource_exists(R1, State) ->
    {Path, R2} = cowboy_http_req:path_info(R1),
    case impel_hierarchy:children(Path) of
	{ok, Children} ->
	    {true, R2, State#state{children = Children}};

	{error, not_found} ->
	    {false, R2, State}
    end.

content_types_provided(ReqData, State) ->
    {[{{<<"application">>, <<"atom+xml">>, []}, to_atom}], ReqData, State}.

to_atom(ReqData, #state{children = Children} = State) ->
    {impel_atom:to_atom(ReqData, State, Children), ReqData, State}.
