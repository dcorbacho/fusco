%%%=============================================================================
%%% @copyright (C) 1999-2013, Erlang Solutions Ltd
%%% @author Diana Corbacho <diana.corbacho@erlang-solutions.com>
%%% @doc
%%% @end
%%%=============================================================================
-module(lhttpc_protocol_tests).

-include_lib("eunit/include/eunit.hrl").

-export([test_decode_header/0]).

lhttpc_protocol_test_() ->
    [{"HTTP version", ?_test(http_version())},
     {"Cookies", ?_test(cookies())},
     {"Decode header", ?_test(decode_header())}].

http_version() ->
    L = {_, _, Socket} = test_utils:start_listener(cookie_message()),
    test_utils:send_message(Socket),
    ?assertMatch({{1,1}, <<"200">>, <<"OK">>, _, _, <<"Great success!">>},
		 lhttpc_protocol:recv(Socket, false)),
    test_utils:stop_listener(L).

cookies() ->
    L = {_, _, Socket} = test_utils:start_listener(cookie_message()),
    test_utils:send_message(Socket),
    Recv = lhttpc_protocol:recv(Socket, false),
    test_utils:stop_listener(L),
    ?assertMatch({{1,1}, <<"200">>, <<"OK">>,
		  _,
		  [{<<"set-cookie">>,<<"name2=value2; Expires=Wed, 09 Jun 2021 10:18:14 GMT">>},
		   {<<"set-cookie">>,<<"name=value">>} | _],
		  <<"Great success!">>},
		 Recv).

decode_header() ->
    ?assertMatch({undefined, undefined, undefined,
		  _,
		  [{<<"set-cookie">>,<<"name2=value2; Expires=Wed, 09 Jun 2021 10:18:14 GMT">>},
		   {<<"set-cookie">>,<<"name=value">>},
		   {<<"content-length">>, <<"14">>},
		   {<<"content-type">>,<<"text/plain">>}],
		  <<"Great success!">>},
		 test_decode_header()).

test_decode_header() ->
    lhttpc_protocol:decode_header(header(), <<>>,
				  lhttpc_protocol:empty_state()).

header() ->
    <<"Content-type: text/plain\r\nContent-length: 14\r\nSet-Cookie: name=value\r\nSet-Cookie: name2=value2; Expires=Wed, 09 Jun 2021 10:18:14 GMT\r\n\r\nGreat success!">>.

cookie_message() ->
    [
     "HTTP/1.1 200 OK\r\n"
     "Content-type: text/plain\r\nContent-length: 14\r\n"
     "Set-Cookie: name=value\r\n"
     "Set-Cookie: name2=value2; Expires=Wed, 09 Jun 2021 10:18:14 GMT\r\n"
     "\r\n"
     "Great success!"
    ].
