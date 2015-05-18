%% Copyright (c) 2012-2015 Peter Morgan <peter.james.morgan@gmail.com>
%%
%% Licensed under the Apache License, Version 2.0 (the "License");
%% you may not use this file except in compliance with the License.
%% You may obtain a copy of the License at
%%
%% http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing, software
%% distributed under the License is distributed on an "AS IS" BASIS,
%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%% See the License for the specific language governing permissions and
%% limitations under the License.

-module(lighthouse_events_resource).
-export([
	 init/3,
	 rest_init/2,
	 allowed_methods/2,
	 content_types_accepted/2,
	 post_is_create/2,
	 from_form_urlencoded/2
	]).

init(_, _, _) ->
    {upgrade, protocol, cowboy_rest}.

rest_init(Req, _) ->
    {ok, Req, #{}}.

allowed_methods(R, S) ->
    {[<<"POST">>], R, S}.

content_types_accepted(Req, State) ->
    {[{{<<"application">>, <<"x-www-form-urlencoded">>, '*'}, from_form_urlencoded}], Req, State}.

post_is_create(R, S) ->
    {false, R, S}.

from_form_urlencoded(R1, S) ->
    {ok, Bindings, R2} = cowboy_req:body_qs(R1),
    case maps:from_list(Bindings) of
	#{<<"topic">> := Topic, <<"data">> := Data, <<"event">> := Event} ->
	    {post(Topic, Event, Data), R2, S};
	_ ->
	    {false, R2, S}
    end.


post(Topic, Event, Data) ->
    lighthouse_node:property(lighthouse_topic:ensure_present(Topic), Event, Data) == ok.
