%%%=============================================================================
%%% @copyright (C) 1999-2012, Erlang Solutions Ltd
%%% @author Diana Corbacho <diana.corbacho@erlang-solutions.com>
%%% @doc Unit tests for lhttpc_client
%%% @end
%%%=============================================================================
-module(lhttpc_client_tests).
-copyright("2012, Erlang Solutions Ltd.").

-include_lib("eunit/include/eunit.hrl").

-compile(export_all).

fail_connect_test() ->
    ?assertEqual({error, econnrefused},
         lhttpc_client:start({{"localhost", 8080, false}, []}, [])).
