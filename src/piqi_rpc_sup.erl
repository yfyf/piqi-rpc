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
%% @doc OTP supervisor behavior for Piqi-RPC
%%
-module(piqi_rpc_sup).

-behaviour(supervisor).

-export([start_link/0]).
% OTP supervisor callbacks
-export([init/1]).


-define(SUPERVISOR, ?MODULE).
-define(PIQI_RPC_ENV_TABLE, piqi_rpc_env_table).


start_link() ->
    supervisor:start_link({local, ?SUPERVISOR}, ?MODULE, []).


%
% Supervisor callback



init(_Args) ->
    %% Table used to maintain registered services, specifically not named,
    %% to avoid handling name clashes on restarts.
    TableId = ets:new(
        ?PIQI_RPC_ENV_TABLE, [set, public]
    ),
    ChildSpecs = [
      {piqi_rpc, {piqi_rpc, start_link, [TableId]},
        permanent, 5000, worker, [piqi_rpc]}
    ],
    {ok, {{one_for_one, 10, 10}, ChildSpecs}}.

