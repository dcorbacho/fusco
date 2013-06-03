%%%=============================================================================
%%% @copyright (C) 1999-2013, Erlang Solutions Ltd
%%% @author Diana Corbacho <diana.corbacho@erlang-solutions.com>
%%% @doc
%%% @end
%%%=============================================================================
-module(fusco_tests_SUITE).
-compile(export_all).

all() ->
    [prop_http_request_ipv4, prop_http_request_ipv6,
     prop_http_request_ipv4_ssl, prop_http_request_ipv6_ssl,
     prop_persistent_connection_ipv4, prop_persistent_connection_ipv6,
     prop_persistent_connection_ipv4_ssl, prop_persistent_connection_ipv6_ssl,
     prop_reconnect_ipv4, prop_reconnect_ipv6, prop_client_close_connection_ipv4,
     prop_client_close_connection_ipv6].

init_per_suite(Config) ->
    [ok = application:start(App) || App <- apps()],
    Config.

end_per_suite(_Config) ->
    [application:stop(App) || App <- lists:reverse(apps())],
    ok.

apps() ->
    [crypto, public_key, ssl, fusco].

%%==============================================================================
%% Test cases
%%==============================================================================
prop_http_request_ipv4(_) ->
    do_prop(prop_http_request_ipv4).

prop_http_request_ipv6(_) ->
    do_prop(prop_http_request_ipv6).

prop_http_request_ipv4_ssl(_) ->
    do_prop(prop_http_request_ipv4_ssl).

prop_http_request_ipv6_ssl(_) ->
    do_prop(prop_http_request_ipv6_ssl).

prop_persistent_connection_ipv4(_) ->
    do_prop(prop_persistent_connection_ipv4).

prop_persistent_connection_ipv6(_) ->
    do_prop(prop_persistent_connection_ipv6).

prop_persistent_connection_ipv4_ssl(_) ->
    do_prop(prop_persistent_connection_ipv4_ssl).

prop_persistent_connection_ipv6_ssl(_) ->
    do_prop(prop_persistent_connection_ipv6_ssl).

prop_reconnect_ipv4(_) ->
    do_prop(prop_reconnect_ipv4).

prop_reconnect_ipv6(_) ->
    do_prop(prop_reconnect_ipv6).

prop_client_close_connection_ipv4(_) ->
    do_prop(prop_client_close_connection_ipv4).

prop_client_close_connection_ipv6(_) ->
    do_prop(prop_client_close_connection_ipv6).

%%==============================================================================
%% Internal functions
%%==============================================================================
do_prop(Case) ->
    case eqc:counterexample(erlang:apply(fusco_tests_eqc, Case, [])) of
        true ->
            true;
        Value ->
            exit(Value)
    end.
