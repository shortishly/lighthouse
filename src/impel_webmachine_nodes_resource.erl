-module(impel_webmachine_nodes_resource).

-export([init/1,
	 content_types_provided/2,
	 to_atom/2]).

-include_lib("webmachine/include/webmachine.hrl").

init([]) ->
    {ok, []}.


content_types_provided(ReqData, Context) ->
    {[{"application/atom+xml", to_atom}], ReqData, Context}.


to_atom(ReqData, State) ->
    {{stream, stream_atom(ReqData, State)}, ReqData, State}.

stream_atom(ReqData, State) ->
    {<<"<?xml version=\"1.0\" encoding=\"utf-8\"?> 
<feed xmlns=\"http://www.w3.org/2005/Atom\">
<title>Example Feed</title>
<subtitle>A subtitle.</subtitle>
<link href=\"http://example.org/feed/\" rel=\"self\"/>
<link href=\"http://example.org/\"/>
<id>urn:uuid:60a76c80-d399-11d9-b91C-0003939e0af6</id>
<updated>2003-12-13T18:30:02Z</updated>
<author>
        <name>John Doe</name>
        <email>johndoe@example.com</email>
</author>">>, fun() -> stream_atom_entry(ReqData, State, impel_hierarchy:children()) end}.


stream_atom_entry(ReqData, State, [_ | T]) ->
    {<<"<entry>
<title>Atom-Powered Robots Run Amok</title>
<link href=\"http://example.org/2003/12/13/atom03\"/>
<link rel=\"alternate\" type=\"text/html\" href=\"http://example.org/2003/12/13/atom03.html\"/>
<link rel=\"edit\" href=\"http://example.org/2003/12/13/atom03/edit\"/>
<id>urn:uuid:1225c695-cfb8-4ebb-aaaa-80da344efa6a</id>
<updated>2003-12-13T18:30:02Z</updated>
<summary>Some text.</summary>
</entry>">>, fun() -> stream_atom_entry(ReqData, State, T) end};
stream_atom_entry(_, _, []) ->
    {<<"</feed>">>, done}.

		     



		      



