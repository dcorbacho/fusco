%%% ----------------------------------------------------------------------------
%%% Copyright (c) 2009, Erlang Training and Consulting Ltd.
%%% All rights reserved.
%%% 
%%% Redistribution and use in source and binary forms, with or without
%%% modification, are permitted provided that the following conditions are met:
%%%    * Redistributions of source code must retain the above copyright
%%%      notice, this list of conditions and the following disclaimer.
%%%    * Redistributions in binary form must reproduce the above copyright
%%%      notice, this list of conditions and the following disclaimer in the
%%%      documentation and/or other materials provided with the distribution.
%%%    * Neither the name of Erlang Training and Consulting Ltd. nor the
%%%      names of its contributors may be used to endorse or promote products
%%%      derived from this software without specific prior written permission.
%%% 
%%% THIS SOFTWARE IS PROVIDED BY Erlang Training and Consulting Ltd. ''AS IS''
%%% AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
%%% IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
%%% ARE DISCLAIMED. IN NO EVENT SHALL Erlang Training and Consulting Ltd. BE
%%% LIABLE SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR
%%% BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY,
%%% WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR
%%% OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF
%%% ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
%%% ----------------------------------------------------------------------------

%%% @author Oscar Hellström <oscar@hellstrom.st>
%%% @author Magnus Henoch <magnus@erlang-consulting.com>
%%% @doc Simple web server for testing purposes
%%% @end
-module(webserver).

-export([start/2, start/3, stop/2]).
-export([acceptor/3]).

start(Module, Responders) ->
    start(Module, Responders, inet).

start(Module, Responders, Family) ->
    case get_addr("localhost", Family) of
        {ok, Addr} ->
            LS = listen(Module, Addr, Family),
            Pid = spawn(?MODULE, acceptor, [Module, LS, Responders]),
            {ok, Pid, LS, port(Module, LS)};
        Error ->
            Error
    end.

stop(Listener, LS) ->
    (catch exit(kill, Listener)),
    gen_tcp:close(LS).

acceptor(Module, ListenSocket, Responders) ->
    case accept(Module, ListenSocket) of
	error ->
	    ok;
	Socket ->
	    spawn_link(fun() ->
			       acceptor(Module, ListenSocket, Responders)
		       end),
	    server_loop(Module, Socket, nil, [], Responders)
    end.

server_loop(Module, Socket, _, _, []) ->
    Module:close(Socket);
server_loop(Module, Socket, Request, Headers, [H | T] = Responders) ->
    receive
	stop ->
	    Module:close(Socket)
    after 0 ->
	    case Module:recv(Socket, 0, 500) of
		{ok, {http_request, _, _, _} = NewRequest} ->
		    server_loop(Module, Socket, NewRequest, Headers, Responders);
		{ok, {http_header, _, Field, _, Value}} when is_atom(Field) ->
		    NewHeaders = [{atom_to_list(Field), Value} | Headers],
		    server_loop(Module, Socket, Request, NewHeaders, Responders);
		{ok, {http_header, _, Field, _, Value}} when is_list(Field) ->
		    NewHeaders = [{Field, Value} | Headers],
		    server_loop(Module, Socket, Request, NewHeaders, Responders);
		{ok, http_eoh} ->
		    RequestBody = case proplists:get_value("Content-Length", Headers) of
				      undefined ->
					  <<>>;
				      "0" ->
					  <<>>;
				      SLength ->
					  Length = list_to_integer(SLength),
					  setopts(Module, Socket, [{packet, raw}]),
					  {ok, Body} = Module:recv(Socket, Length),
					  setopts(Module, Socket, [{packet, http}]),
					  Body
				  end,
		    H(Module, Socket, Request, Headers, RequestBody),
		    case proplists:get_value("Connection", Headers) of
			"close" ->
			    Module:close(Socket);
			_ ->
			    server_loop(Module, Socket, none, [], T)
		    end;
		{error, timeout} ->
		    server_loop(Module, Socket, Request, Headers, Responders);
		{error, closed} ->
		    Module:close(Socket)
	    end
    end.

listen(ssl, Addr, Family) ->
    Opts = [
        Family,
        {packet, http},
        binary,
        {active, false},
        {ip, Addr},
        {verify,0},
        {keyfile, "../test/key.pem"},
        {certfile, "../test/crt.pem"}
    ],
    {ok, LS} = ssl:listen(0, Opts),
    LS;
listen(Module, Addr, Family) ->
    {ok, LS} = Module:listen(0, [
            Family,
            {packet, http},
            binary,
            {active, false},
            {ip, Addr}
        ]),
    LS.

get_addr(Host, Family) ->
    case inet:getaddr(Host, Family) of
        {ok, Addr} ->
            {ok, Addr};
        _ ->
            {error, family_not_supported}
    end.

accept(ssl, ListenSocket) ->
    case ssl:transport_accept(ListenSocket, 10000) of
	{ok, Socket} ->
	    ok = ssl:ssl_accept(Socket),
	    Socket;
	{error, _} ->
	    error
    end;
accept(Module, ListenSocket) ->
    case Module:accept(ListenSocket, 1000) of
	{ok, Socket} ->
	    Socket;
	{error, _} ->
	    error
    end.

setopts(ssl, Socket, Options) ->
    ssl:setopts(Socket, Options);
setopts(_, Socket, Options) ->
    inet:setopts(Socket, Options).

port(ssl, Socket) ->
    {ok, {_, Port}} = ssl:sockname(Socket),
    Port;
port(_, Socket) ->
    {ok, Port} = inet:port(Socket),
    Port.
