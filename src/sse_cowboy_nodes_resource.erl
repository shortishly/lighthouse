-module(sse_cowboy_nodes_resource).

-export([init/3,
	 rest_init/2,
	 content_types_provided/2,
	 to_atom/2]).

init({tcp, http}, _, []) ->
    {upgrade, protocol, cowboy_http_rest}.

rest_init(Req, _) ->
    {ok, Req, []}.

content_types_provided(ReqData, Context) ->
    {[{{<<"application">>, <<"atom+xml">>, []}, to_atom}], ReqData, Context}.

to_atom(ReqData, State) ->
    {ok, Children} = sse_hierarchy:children(),
    {sse_atom:to_atom(ReqData, State, Children), ReqData, State}.

    
