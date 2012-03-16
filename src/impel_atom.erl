-module(impel_atom).
-export([to_atom/3]).
-export([join/1]).

to_atom(ReqData, State, Entries) ->
    to_atom(ReqData, State, [header(ReqData, State)], Entries).

to_atom(ReqData, State, A, []) ->
    lists:reverse([footer(ReqData, State) | A]);
to_atom(ReqData, State, A, [H | T]) ->
    to_atom(ReqData, State, [entry(ReqData, State, H) | A], T).

header(ReqData, _) ->
    Self = case cowboy_http_req:path_info(ReqData) of
	       {undefined, _} ->
		   <<"nodes">>;
	       {Components, _} ->
		   join(Components)
	   end,
    [<<"<?xml version=\"1.0\" encoding=\"utf-8\"?>">>,
     <<"<feed xmlns=\"http://www.w3.org/2005/Atom\">">>,
     io_lib:format("<title>~s</title>", [Self]),
     io_lib:format("<link href=\"~s~s/~s\" rel=\"self\"/>", ph(ReqData) ++ [Self]),
     io_lib:format("<link href=\"~s~s/\"/>", ph(ReqData))].

entry(ReqData, State, Entry) ->
    Path = case cowboy_http_req:path_info(ReqData) of
	       {undefined, _} ->
		   [];
	       {Components, _} ->
		   Components
	   end,
    entry(ReqData, State, Entry, Path, impel_hierarchy:type(Entry)).

entry(ReqData, _, Entry, Path, branch) ->
    Node = [join(Path ++ [impel_hierarchy:key(Entry)])],
    [<<"<entry>">>,
     title(Entry),
     io_lib:format("<link href=\"~s~s/node/~s\"/>", ph(ReqData) ++ Node),
     io_lib:format("<id>urn:tag:nodes.example.com,~s/~s</id>", [ymd(impel_hierarchy:created(Entry)), impel_hierarchy:key(Entry)]),
     updated(Entry),
     <<"</entry>">>];
entry(ReqData, _, Entry, Path, leaf) ->
    Node = [join(Path ++ [impel_hierarchy:key(Entry)])],
    [<<"<entry>">>,
     title(Entry),
     io_lib:format("<link href=\"~s~s/es/~s\"/>", ph(ReqData) ++ Node),
     io_lib:format("<id>urn:tag:nodes.example.com,~s/~s</id>", [ymd(impel_hierarchy:created(Entry)), Node]),
     updated(Entry),
     <<"</entry>">>].

footer(_, _) ->
    [<<"</feed>">>].

join(Items) ->
    join(Items, <<"/">>).

join([H1, H2 | T], Separator) ->
    [H1, Separator | join([H2 | T], Separator)];
join([H | T], Separator) ->
    [H | join(T, Separator)];
join([], _) ->
    [].



ph(ReqData) ->
    [protocol(ReqData), host(ReqData)].

title(L) ->
    io_lib:format("<title>~s</title>", [impel_hierarchy:key(L)]).

updated(L) ->
    [<<"<updated>">>,
     rfc3339(impel_hierarchy:updated(L)),
     <<"</updated>">>].


host(R1) ->
    {Host, R2} = cowboy_http_req:host(R1),
    {Raw, R3} = cowboy_http_req:raw_host(R2),
    {Port, _} = cowboy_http_req:port(R3),
    error_logger:info_report([{host, Host},
			      {raw_host, Raw},
			      {port, Port}]),
    host(Host, Port).

host(Host, 80) ->
    Host;
host(Host, Port) ->
    Host ++ [":" | integer_to_list(Port)].


protocol(_ReqData) ->
    "http://".

ymd({{Year, Month, Day}, _}) ->
    io_lib:format("~4..0w-~2..0w-~2..0w", [Year, Month, Day]).

rfc3339({{Year, Month, Day}, {Hour, Minute, Second}}) ->
    io_lib:format("~4..0w-~2..0w-~2..0wT~2..0w:~2..0w:~2..0wZ", [Year, Month, Day, Hour, Minute, Second]).
