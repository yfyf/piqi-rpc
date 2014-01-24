-module(piqi_rpc_eunit).

-include_lib("eunit/include/eunit.hrl").

-compile(export_all).


piqi_rpc_test_() ->
    {foreach,
        fun () ->
            %% for cleaner / readable test logs, though I would recommend
            %% re-enabling this during actual testing.
            error_logger:tty(false)
        end,
        fun (_) ->
            piqi_rpc:stop_all(),
            ranch_bug_sleep()
        end,
        [
         fun start_stop/0,
         fun stop_twice/0,
         fun start_stop_start/0,
         fun clean_services_on_stop/0,
         fun clean_services/0,
         fun crash_gen_server/0
        ]
    }.

start_stop() ->
    ?assertEqual(ok, piqi_rpc:start()),
    ?assertEqual(ok, application:stop(piqi_rpc)).

stop_twice() ->
    ?assertEqual(ok, piqi_rpc:start()),
    ?assertEqual(ok, application:stop(piqi_rpc)),
    ?assertMatch({error, {not_started, _}}, application:stop(piqi_rpc)).

start_stop_start() ->
    ?assertEqual(ok, piqi_rpc:start()),
    ?assertEqual(ok, application:stop(piqi_rpc)),
    ranch_bug_sleep(),
    ?assertEqual(ok, application:start(piqi_rpc)),
    ?assertEqual(ok, application:stop(piqi_rpc)).

clean_services_on_stop() ->
    ?assertEqual(ok, piqi_rpc:start()),
    ?assertEqual([], piqi_rpc:get_services()),
    ServiceSpec = {?MODULE, ?MODULE, "eunit"},
    ?assertEqual(ok, piqi_rpc:add_service(ServiceSpec)),
    ?assertEqual([erlang:append_element(ServiceSpec, [])], piqi_rpc:get_services()),
    ok = application:stop(piqi_rpc),
    ranch_bug_sleep(),
    ok = application:start(piqi_rpc),
    ?assertEqual([], piqi_rpc:get_services()).

clean_services() ->
    ok = piqi_rpc:start(),
    [] = piqi_rpc:get_services(),
    ServiceSpec = {?MODULE, ?MODULE, "eunit"},
    ?assertEqual(ok, piqi_rpc:add_service(ServiceSpec)),
    ?assertEqual([erlang:append_element(ServiceSpec, [])], piqi_rpc:get_services()),
    ?assertEqual(ok, piqi_rpc:remove_all_services()),
    ?assertEqual([], piqi_rpc:get_services()),
    ok = application:stop(piqi_rpc).

%% crash the piqi_rpc gen server and make sure it re-inits with the same
%% services.
crash_gen_server() ->
    ok = piqi_rpc:start(),
    ServiceSpec = {?MODULE, ?MODULE, "eunit"},
    ok = piqi_rpc:add_service(ServiceSpec),
    Pid = whereis(piqi_rpc),
    exit(Pid, kill),
    timer:sleep(100),
    ?assertEqual([erlang:append_element(ServiceSpec, [])], piqi_rpc:get_services()).


%% Helpers
%%
%% Pretend this module is a Piqi-RPC impl module.

get_functions() ->
    [ rpc_method_1, rpc_method_2 ].

rpc_method_1(_) -> ok.

rpc_method_2(_) -> ok.

ranch_bug_sleep() ->
    %% there's a bug
    %%  (https://github.com/extend/ranch/issues/74)
    %% in `ranch` causing it to (in rare cases) fail in the presence of many
    %% very quick starts / stops, so this is a temporary workaround, which is
    %% needed inbetween start / stops.
    timer:sleep(10).
