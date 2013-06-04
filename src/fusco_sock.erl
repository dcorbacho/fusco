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

%%------------------------------------------------------------------------------
%%% @private
%%% @author Oscar Hellström <oscar@hellstrom.st>
%%% @author Diana Parra Corbacho <diana.corbacho@erlang-solutions.com>
%%% @doc
%%% This module implements wrappers for socket operations.
%%% Makes it possible to have the same interface to ssl and tcp sockets.
%%------------------------------------------------------------------------------
-module(fusco_sock).

-export([connect/5,
         recv/2,
	 recv/3,
	 send/3,
         close/2]).

-include("fusco_types.hrl").

%%==============================================================================
%% Exported functions
%%==============================================================================

%%------------------------------------------------------------------------------
%% @spec (Host, Port, Options, Timeout, SslFlag) -> {ok, Socket} | {error, Reason}
%%   Host = string() | ip_address()
%%   Port = integer()
%%   Options = [{atom(), term()} | atom()]
%%   Timeout = infinity | integer()
%%   SslFlag = boolean()
%%   Socket = socket()
%%   Reason = atom()
%% @doc
%% Connects to `Host' and `Port'.
%% Will use the `ssl' module if `SslFlag' is `true' and gen_tcp otherwise.
%% `Options' are the normal `gen_tcp' or `ssl' Options.
%% @end
%%------------------------------------------------------------------------------
-spec connect(host(), integer(), socket_options(), timeout(), boolean()) ->
    {ok, socket()} | {error, atom()}.
connect(Host, Port, Options, Timeout, true) ->
    ssl:connect(Host, Port, Options, Timeout);
connect(Host, Port, Options, Timeout, false) ->
    gen_tcp:connect(Host, Port, Options, Timeout).

%%------------------------------------------------------------------------------
%% @spec (Socket, SslFlag) -> {ok, Data} | {error, Reason}
%%   Socket = socket()
%%   Length = integer()
%%   SslFlag = boolean()
%%   Data = term()
%%   Reason = atom()
%% @doc
%% Reads available bytes from `Socket'.
%% Will block untill data is available on the socket and return the first
%% packet.
%% @end
%%------------------------------------------------------------------------------
-spec recv(socket(), boolean()) ->
    {ok, any()} | {error, atom()} | {error, {http_error, string()}}.
recv(Socket, true) ->
    ssl:recv(Socket, 0);
recv(Socket, false) ->
    prim_inet:recv(Socket, 0).

-spec recv(socket(), boolean(), timeout()) ->
    {ok, any()} | {error, atom()} | {error, {http_error, string()}}.
recv(Socket, true, Timeout) ->
    ssl:recv(Socket, 0, Timeout);
recv(Socket, false, Timeout) ->
    prim_inet:recv(Socket, 0, Timeout).

%%------------------------------------------------------------------------------
%% @spec (Socket, Data, SslFlag) -> ok | {error, Reason}
%%   Socket = socket()
%%   Data = iolist()
%%   SslFlag = boolean()
%%   Reason = atom()
%% @doc
%% Sends data on a socket.
%% Will use the `ssl' module if `SslFlag' is set to `true', otherwise the
%% gen_tcp module.
%% @end
%%------------------------------------------------------------------------------
-spec send(socket(), iolist() | binary(), boolean()) -> ok | {error, atom()}.
send(Socket, Request, true) ->
    ssl:send(Socket, Request);
send(Socket, Request, false) ->
    prim_inet:send(Socket, Request, []).

%%------------------------------------------------------------------------------
%% @spec (Socket, SslFlag) -> ok | {error, Reason}
%%   Socket = socket()
%%   SslFlag = boolean()
%%   Reason = atom()
%% @doc
%% Closes a socket.
%% @end
%%------------------------------------------------------------------------------
-spec close(socket(), boolean()) -> ok | {error, atom()}.
close(Socket, true) ->
    ssl:close(Socket);
close(Socket, false) ->
    gen_tcp:close(Socket).
