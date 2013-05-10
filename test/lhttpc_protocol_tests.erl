%%%=============================================================================
%%% @copyright (C) 1999-2013, Erlang Solutions Ltd
%%% @author Diana Corbacho <diana.corbacho@erlang-solutions.com>
%%% @doc
%%% @end
%%%=============================================================================
-module(lhttpc_protocol_tests).

-include_lib("eunit/include/eunit.hrl").


lhttpc_protocol_test_() ->
    [{"HTTP version", ?_test(http_version())},
     {"Cookies", ?_test(cookies())}].

http_version() ->
    Port = webserver:start(gen_tcp, [fun webserver_utils:simple_response/5]),
    {ok, Socket} = gen_tcp:connect("127.0.0.1", Port, [binary, {packet, raw},
						       {nodelay, true},
						       {reuseaddr, true},
						       {active, false}], 5000),
    gen_tcp:send(Socket, message()),
    ?assertMatch({{1,1}, <<"200">>, <<"OK">>, _, <<"Great success!">>},
		 lhttpc_protocol:recv(Socket, false)).

cookies() ->
    Port = webserver:start(gen_tcp, [user_response(cookie_message())]),
    {ok, Socket} = gen_tcp:connect("127.0.0.1", Port, [binary, {packet, raw},
						       {nodelay, true},
						       {reuseaddr, true},
						       {active, false}], 5000),
    gen_tcp:send(Socket, message()),
    Recv = lhttpc_protocol:recv(Socket, false),
    ?assertMatch({{1,1}, <<"200">>, <<"OK">>,
		  [{<<"set-cookie">>,<<"name2=value2; Expires=Wed, 09 Jun 2021 10:18:14 GMT">>},
		   {<<"set-cookie">>,<<"name=value">>} | _],
		  <<"Great success!">>},
		 Recv).

user_response(Message) ->
    fun(Module, Socket, _, _, _) ->
	    Module:send(Socket, Message)
    end.

cookie_message() ->
    [
     "HTTP/1.1 200 OK\r\n"
     "Content-type: text/plain\r\nContent-length: 14\r\n"
     "Set-Cookie: name=value\r\n"
     "Set-Cookie: name2=value2; Expires=Wed, 09 Jun 2021 10:18:14 GMT\r\n"
     "\r\n"
     "Great success!"
    ].

message() ->
    <<"GET /blabla HTTP/1.1\r\nhost: 127.0.0.1:5050\r\nuser-agent: Cow\r\nAccept: */*\r\n\r\n">>.
