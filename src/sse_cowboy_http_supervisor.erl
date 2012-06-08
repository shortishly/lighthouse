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

-module(sse_cowboy_http_supervisor).
-behaviour(supervisor).
-export([start_link/0,
	 start_link/1,
	 init/1]).


start_link() ->
    start_link([]).

start_link(P) ->
    supervisor:start_link(?MODULE, P).

init(Args) ->
    {ok, {{one_for_one, 5, 10},
	  [cowboy:child_spec(http, 1000,
			     cowboy_tcp_transport, proplists:get_value(tcp_transport, Args),
			     cowboy_http_protocol, proplists:get_value(http_protocol, Args))]}}.
    
