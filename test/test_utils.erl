%%%=============================================================================
%%% @copyright (C) 1999-2013, Erlang Solutions Ltd
%%% @author Diana Corbacho <diana.corbacho@erlang-solutions.com>
%%% @doc
%%% @end
%%%=============================================================================
-module(test_utils).

-export([start_listener/1,
	 send_message/1]).

start_listener(Msg) ->
    Port = webserver:start(gen_tcp, [user_response(Msg)]),
    {ok, Socket} = gen_tcp:connect("127.0.0.1", Port, [binary, {packet, raw},
						       {nodelay, true},
						       {reuseaddr, true},
						       {active, false}], 5000),
    Socket.

send_message(Socket) ->
    gen_tcp:send(Socket, message()).

user_response(Message) ->
    fun(Module, Socket, _, _, _) ->
	    Module:send(Socket, Message)
    end.

message() ->
    <<"GET /blabla HTTP/1.1\r\nhost: 127.0.0.1:5050\r\nuser-agent: Cow\r\nAccept: */*\r\n\r\n">>.

