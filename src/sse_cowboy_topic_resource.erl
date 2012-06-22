%% Copyright (c) 2012, Peter Morgan <peter.james.morgan@gmail.com>
%%
%% Permission to use, copy, modify, and/or distribute this software for any
%% purpose with or without fee is hereby granted, provided that the above
%% copyright notice and this permission notice appear in all copies.
%%
%% THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
%% WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
%% MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
%% ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
%% WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
%% ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
%% OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.

-module(sse_cowboy_topic_resource).

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
    case sse_hierarchy:children(Path) of
	{ok, Children} ->
	    {true, R2, State#state{children = Children}};

	{error, {is_a_leaf, _, _}} ->
	    {true, R2, State#state{children = []}};

	{error, {not_found, _, _}} ->
	    {false, R2, State}
    end.

content_types_provided(ReqData, State) ->
    {[{{<<"application">>, <<"atom+xml">>, []}, to_atom}], ReqData, State}.

to_atom(ReqData, #state{children = Children} = State) ->
    {sse_atom:to_atom(ReqData, State, Children), ReqData, State}.
