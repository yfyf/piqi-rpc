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
%% @doc Piqi-RPC HTTP interface
%%
%% Currently it is implemented on top of Webmachine
%%

-module(piqi_rpc_http).

-export([start_link/0, cleanup/0]).
-export([add_service/1, remove_service/1, configure/0]).
%-compile(export_all).

-define(DEFAULT_PORT, 8080).
-define(DEFAULT_NAME, piqi_rpc_server).

-include("piqi_rpc.hrl").

start_link() ->
    Routes = get_cowboy_routes(),
    cowboy:start_http(get_http_env(name, ?DEFAULT_NAME), get_http_env(nb_acceptors, 100),
        [{port, get_http_env(port, ?DEFAULT_PORT)}],
        [{env, [{dispatch, Routes}]}]
    ).

configure() ->
    Routes = get_cowboy_routes(),
    cowboy:set_env(get_http_env(name, ?DEFAULT_NAME), dispatch, Routes).

get_cowboy_routes() ->
    RpcServices = piqi_rpc:get_services(),
    Routes = [ rpc_service_to_cowboy_route(X) || X <- RpcServices ],
    Dispatch = [
        {'_', lists:flatten(Routes)}
    ],
    cowboy_router:compile(Dispatch).

rpc_service_to_cowboy_route(_RpcService = {ImplMod, RpcMod, UrlPath, Options}) ->
    LAdjustedPath = case string:left(UrlPath, 1) of
        "/" -> UrlPath;
        _ -> "/" ++ UrlPath
    end,
    AdjustedPath = case string:right(LAdjustedPath, 1) of
        "/" -> LAdjustedPath;
        _ -> LAdjustedPath ++ "/"
    end,
    PathMatch = AdjustedPath ++ ":function/[...]",
    [
        {PathMatch, piqi_rpc_resource, {ImplMod, RpcMod, make_service_options(Options)}},
        {AdjustedPath, piqi_rpc_resource, {ImplMod, RpcMod, make_service_options(Options)}}
    ].

% backward compatibility
cleanup() ->
    cowboy:set_env(get_http_env(name, ?DEFAULT_NAME), dispatch, []).

-spec add_service/1 :: ( piqi_rpc_service() ) -> ok.
add_service(_RpcService = {_ImplMod, _RpcMod, _UrlPath, _Options}) ->
    configure().

-spec remove_service/1 :: ( piqi_rpc_service() ) -> ok.
remove_service(_RpcService = {_ImplMod, _RpcMod, _UrlPath, _Options}) ->
    configure().

% Utility
get_http_env(Key, Default) ->
    case proplists:get_value(Key, get_env(piqi_rpc, http_server, [])) of
        undefined -> Default;
        Value -> Value
    end.

get_env(App, Key, Default) ->
    case application:get_env(App, Key) of
        undefined -> Default;
        {ok, Value} -> Value
    end.

make_service_options(Options) ->
    DefaultOpts = get_env(piqi_rpc, 'default_service_options', []),
    ReturnDefinition = get_env(piqi_rpc, 'return_definition', false),
    Options ++ [{return_definition, ReturnDefinition}] ++ DefaultOpts.
