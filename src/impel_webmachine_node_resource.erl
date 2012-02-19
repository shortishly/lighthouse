-module(impel_webmachine_node_resource).

-export([init/1,
	 resource_exists/2,
	 content_types_provided/2,
	 to_atom/2]).

-include_lib("webmachine/include/webmachine.hrl").

-record(state, {children}).

init([]) ->
    {ok, #state{}}.

resource_exists(ReqData, State) ->
    case impel_hierarchy:children(string:tokens(wrq:disp_path(ReqData), "/")) of
	{ok, Children} ->
	    {true, ReqData, State#state{children = Children}};

	{error, not_found} ->
	    {false, ReqData, State}
    end.

content_types_provided(ReqData, State) ->
    {[{"application/atom+xml", to_atom}], ReqData, State}.


to_atom(ReqData, State) ->
    {{stream, stream_atom(ReqData, State)}, ReqData, State}.

stream_atom(ReqData, State) ->
    {iolist_to_binary([<<"<?xml version=\"1.0\" encoding=\"utf-8\"?>">>,
		       <<"<feed xmlns=\"http://www.w3.org/2005/Atom\">">>,
		       io_lib:format("<title>~s</title>", [wrq:disp_path(ReqData)]),
		       io_lib:format("<link href=\"~s~s/node/~s\" rel=\"self\"/>", ph(ReqData) ++ [wrq:disp_path(ReqData)]),
		       io_lib:format("<link href=\"~s~s/\"/>", ph(ReqData))
		      ]), stream_atom_entry(ReqData, State)}.

stream_atom_entry(ReqData, #state{children = Children} = State) ->
    fun() ->
	    stream_atom_entry(ReqData, State, Children)
    end.


stream_atom_entry(ReqData, State, [H | T]) ->
    {iolist_to_binary([<<"<entry>">>,
		       title(H),
		       io_lib:format("<link href=\"~s~s/node/~s/~s\"/>", ph(ReqData) ++ [wrq:disp_path(ReqData), impel_hierarchy:key(H)]),
		       io_lib:format("<link rel=\"alternate\" type=\"text/event-stream\" href=\"~s~s/node/~s/live\"/>", ph(ReqData) ++ [wrq:disp_path(ReqData)]),
		       io_lib:format("<id>urn:tag:nodes.example.com,~s/~s/~s</id>", [ymd(impel_hierarchy:created(H)), wrq:disp_path(ReqData), impel_hierarchy:key(H)]),
		       updated(H),
		       <<"</entry>">>]),
		      fun() -> stream_atom_entry(ReqData, State, T) end};
stream_atom_entry(_, _, []) ->
    {<<"</feed>">>, done}.

ph(ReqData) ->
    [protocol(ReqData), host(ReqData)].

title(L) ->
    io_lib:format("<title>~p</title>", [impel_hierarchy:key(L)]).

updated(L) ->
    [<<"<updated>">>,
     impel_atom:rfc3339(impel_hierarchy:updated(L)),
     <<"</updated>">>].


host(ReqData) ->
     wrq:get_req_header("host", ReqData).

protocol(_ReqData) ->
    "http://".

ymd({{Year, Month, Day}, _}) ->
    io_lib:format("~4..0w-~2..0w-~2..0w", [Year, Month, Day]).
