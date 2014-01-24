-module(piqi_rpc_eunit).

-include_lib("eunit/include/eunit.hrl").


start_stop_test_() ->
    {foreach,
        fun piqi_rpc:stop_all/0,
        [
         fun start_stop/0,
         fun stop_twice/0,
         fun start_stop_start/0
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
    ?assertEqual(ok, application:start(piqi_rpc)),
    ?assertEqual(ok, application:stop(piqi_rpc)).
