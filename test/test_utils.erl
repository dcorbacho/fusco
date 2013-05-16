%%%=============================================================================
%%% @copyright (C) 1999-2013, Erlang Solutions Ltd
%%% @author Diana Corbacho <diana.corbacho@erlang-solutions.com>
%%% @doc
%%% @end
%%%=============================================================================
-module(test_utils).

-export([start_listener/1,
	 send_message/1,
	 stop_listener/1]).

start_listener(Msg) ->
    {ok, Listener, LS, Port} = webserver:start(gen_tcp, [user_response(Msg)]),
    {ok, Socket} = gen_tcp:connect("127.0.0.1", Port, [binary, {packet, raw},
						       {nodelay, true},
						       {reuseaddr, true},
						       {active, false}], 5000),
    {Listener, LS, Socket}.

send_message(Socket) ->
    gen_tcp:send(Socket, message()).

user_response(Message) ->
    fun(Module, Socket, _, _, _) ->
	    Module:send(Socket, Message)
    end.

message() ->
    <<"GET /blabla HTTP/1.1\r\nhost: 127.0.0.1:5050\r\nuser-agent: Cow\r\nAccept: */*\r\n\r\n">>.

stop_listener({Listener, LS, Socket}) ->
    unlink(Listener),
    (catch exit(Listener, kill)),
    gen_tcp:close(LS),
    gen_tcp:close(Socket).
