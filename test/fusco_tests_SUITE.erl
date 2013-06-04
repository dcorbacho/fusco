%%%=============================================================================
%%% @copyright (C) 1999-2013, Erlang Solutions Ltd
%%% @author Diana Corbacho <diana.corbacho@erlang-solutions.com>
%%% @doc
%%% @end
%%%=============================================================================
-module(fusco_tests_SUITE).
-copyright("2013, Erlang Solutions Ltd.").

-include_lib("common_test/include/ct.hrl").

-compile(export_all).

all() ->
    [{group, ipv4}, {group, ipv6}, {group, ipv4ssl}, {group, ipv6ssl}].

init_per_group(ipv4, Config) ->
    [{fusco_parameters, {"127.0.0.1", inet, false}} | Config];
init_per_group(ipv6, Config) ->
    [{fusco_parameters, {"::1", inet6, false}} | Config];
init_per_group(ipv4ssl, Config) ->
    [ok = application:start(App) || App <- apps()],
    [{fusco_parameters, {"127.0.0.1", inet, true}} | Config];
init_per_group(ipv6ssl, Config) ->
    [ok = application:start(App) || App <- apps()],
    [{fusco_parameters, {"::1", inet6, true}} | Config].

end_per_group(ipv4, _Config) ->
    ok;
end_per_group(ipv6, _Config) ->
    ok;
end_per_group(ipv4ssl, _Config) ->
    [application:stop(App) || App <- lists:reverse(apps())],
    ok;
end_per_group(ipv6ssl, _Config) ->
    [application:stop(App) || App <- lists:reverse(apps())],
    ok.

apps() ->
    [crypto, public_key, ssl, fusco].

groups() ->
    [{ipv4, [], all_tests()},
     {ipv6, [], all_tests()},
     {ipv4ssl, [], all_tests()},
     {ipv6ssl, [], all_tests()}].

all_tests() ->
    [prop_http_request, prop_persistent_connection, prop_reconnect,
     prop_client_close_connection, prop_connection_refused].

%%==============================================================================
%% Test cases
%%==============================================================================
prop_http_request(Config) ->
    do_prop(prop_http_request_per_family, Config).

prop_persistent_connection(Config) ->
    do_prop(prop_persistent_connection_per_family, Config).

prop_reconnect(Config) ->
    do_prop(prop_reconnect_per_family, Config).

prop_client_close_connection(Config) ->
    do_prop(prop_client_close_connection_per_family, Config).

prop_connection_refused(Config) ->
    do_prop(prop_connection_refused_per_family, Config).

%%==============================================================================
%% Internal functions
%%==============================================================================
do_prop(Case, Config) ->
    {Ip, Family, Ssl} = ?config(fusco_parameters, Config),
    case eqc:counterexample(erlang:apply(fusco_tests_eqc, Case, [Ip, Family, Ssl])) of
        true ->
            true;
        Value ->
            exit(Value)
    end.
