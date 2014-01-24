%% Copyright 2009, 2010, 2011, 2012, 2013 Anton Lavrik
%%
%% Licensed under the Apache License, Version 2.0 (the "License");
%% you may not use this file except in compliance with the License.
%% You may obtain a copy of the License at
%%
%%     http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing, software
%% distributed under the License is distributed on an "AS IS" BASIS,
%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%% See the License for the specific language governing permissions and
%% limitations under the License.

%%
%% @doc OTP application behavior for Piqi-RPC 
%%
-module(piqi_rpc_app).

-behaviour(application).

-export([start/2, prep_stop/1, stop/1]).

%
% Applicaton callbacks
%

start(_Type, _StartArgs) ->
    {ok, _} = piqi_rpc_http:start_listener(),
    {ok, Pid} = piqi_rpc_sup:start_link(),
    {ok, Pid}.

prep_stop(_State) ->
    ok = piqi_rpc:remove_all_services(),
    ok = piqi_rpc_http:stop_listener().

stop(_State) ->
    ok.

