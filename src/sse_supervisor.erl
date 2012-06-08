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

-module(sse_supervisor).
-behaviour(supervisor).

%% API
-export([start_link/0,
	 start_link/1]).

%% Supervisor callbacks
-export([init/1]).

%% Helper macro for declaring children of supervisor
-define(CHILD(I, Type), {I, {I, start_link, []}, permanent, 5000, Type, [I]}).
-define(CHILD(I, T, P), {I, {I, start_link, P}, permanent, 5000, T, [I]}).

%% ===================================================================
%% API functions
%% ===================================================================

start_link() ->
    start_link([]).

start_link(Parameters) ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, Parameters).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init(Parameters) ->
    {ok, { {one_for_one, 5, 10}, children(Parameters)}}.

children(Parameters) ->
    [?CHILD(sse_hierarchy_gb_trees, worker, Parameters),
     ?CHILD(sse_monitor_node_server, worker),
     ?CHILD(sse_monitoring_server, worker, Parameters),
     ?CHILD(sse_cowboy_http_supervisor, supervisor, Parameters)].
    
