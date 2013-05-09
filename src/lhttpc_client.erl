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
%%% @author Ramon Lastres Guerrero <ramon.lastres@erlang-solutions.com>
%%% @doc This module implements the HTTP request handling. This should normally
%%% not be called directly since it should be spawned by the lhttpc module.
%%% @end
%%------------------------------------------------------------------------------
-module(lhttpc_client).

%exported functions
-export([start_link/2,
         start/2,
         request/7,
         send_body_part/3,
         send_trailers/3,
         get_body_part/2,
         stop/1]).

%% gen_server callbacks
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-include("lhttpc_types.hrl").
-include("lhttpc.hrl").

-define(HTTP_LINE_END, "\r\n").
-define(CONNECTION_HDR(HDRS, DEFAULT),
        lhttpc_lib:header_value(<<"connection">>, HDRS, DEFAULT)).

-record(client_state, {
        host :: string(),
        port = 80 :: port_num(),
        ssl = false :: boolean(),
        pool = undefined,
        pool_options,
        socket,
        connect_timeout = 'infinity' :: timeout(),
        connect_options = [] :: [any()],
        %% next fields are specific to particular requests
        request :: iolist() | undefined,
        method :: string(),
        request_headers :: headers(),
        requester,
        cookies = [] :: [#lhttpc_cookie{}],
        use_cookies = false :: boolean(),
        partial_upload = false :: boolean(),
        chunked_upload = false :: boolean(),
        partial_download = false :: boolean(),
        download_window = infinity :: timeout(),
        download_proc :: pid(),
        part_size :: non_neg_integer() | infinity,
        %% in case of infinity we read whatever data we can get from
        %% the wire at that point or in case of chunked one chunk
        attempts = 0 :: integer(),
        download_info :: {term(), term()},
        body_length = undefined :: {'fixed_length', non_neg_integer()} |
                                    'undefined' | 'chunked' | 'infinite',
        proxy :: undefined | #lhttpc_url{},
        proxy_ssl_options = [] :: [any()],
        proxy_setup = false :: boolean()
        }).

%%==============================================================================
%% Exported functions
%%==============================================================================
start(Args, Options) ->
    gen_server:start(?MODULE, Args, Options).

start_link(Args, Options) ->
    gen_server:start_link(?MODULE, Args, Options).

send_body_part(Client, Part, Timeout) ->
    gen_server:call(Client, {send_body_part, Part}, Timeout).

send_trailers(Client, Trailers, Timeout) ->
    gen_server:call(Client, {send_trailers, Trailers}, Timeout).

get_body_part(Client, Timeout) ->
    gen_server:call(Client, get_body_part, Timeout).

stop(Client) ->
    gen_server:cast(Client, stop).

%%------------------------------------------------------------------------------
%% @spec (Client, Path, Method, Hdrs, RequestBody, Options, Timeout) -> ok
%%    From = pid()
%%    Method = string()
%%    Hdrs = [Header]
%%    Header = {string() | atom(), string()}
%%    Body = iolist()
%%    Options = [Option]
%%    Option = {connect_timeout, Milliseconds}
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec request(pid(), string(), method(), headers(), iolist(), options(), integer()) -> result().
request(Client, Path, Method, Hdrs, Body, Options, Timeout) ->
    gen_server:call(Client,
                    {request, Path, Method, Hdrs, Body, Options}, Timeout).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================
init({Destination, Options}) ->
    PoolOptions = lhttpc_lib:get_value(pool_options, Options, []),
    Pool = lhttpc_lib:get_value(pool, PoolOptions),
    ConnectTimeout = lhttpc_lib:get_value(connect_timeout, Options, infinity),
    ConnectOptions = lhttpc_lib:get_value(connect_options, Options, []),
    UseCookies = lhttpc_lib:get_value(use_cookies, Options, false),
    {Host, Port, Ssl} = case Destination of
        {H, P, S} ->
            {H, P, S};
        URL ->
            #lhttpc_url{host = H, port = P,
                        is_ssl = S} = lhttpc_lib:parse_url(URL),
            {H, P, S}
    end,
    State = #client_state{host = Host, port = Port, ssl = Ssl, pool = Pool,
                          connect_timeout = ConnectTimeout,
                          connect_options = ConnectOptions,
                          use_cookies = UseCookies,
                          pool_options = PoolOptions},
    %% Get a socket for the pool or exit
    case connect_socket(State) of
        {ok, NewState} ->
            {ok, NewState};
        {{error, Reason}, _} ->
            {stop, Reason}
    end.

%%------------------------------------------------------------------------------
%% @doc This function fills in the Client record used in the requests and obtains
%% the socket from an existing pool or creates a new pool if needed. If the
%% socket used is new, it also makes the pool gen_server its controlling process.
%% @end
%%------------------------------------------------------------------------------
handle_call({request, Path, Method, Hdrs, Body, Options}, From,
            State = #client_state{ssl = Ssl, host = Host, port = Port,
                                  socket = Socket, cookies = Cookies,
                                  use_cookies = UseCookies}) ->
    PartialUpload = lhttpc_lib:get_value(partial_upload, Options, false),
    PartialDownload = lists:keymember(partial_download, 1, Options),
    PartialDownloadOptions = lhttpc_lib:get_value(partial_download, Options, []),
    Proxy = case lists:keyfind(proxy, 1, Options) of
		false ->
		    undefined;
		{proxy, ProxyUrl} when is_list(ProxyUrl), not Ssl ->
						% The point of HTTP CONNECT proxying is to use TLS tunneled in
						% a plain HTTP/1.1 connection to the proxy (RFC2817).
		    throw(origin_server_not_https);
		{proxy, ProxyUrl} when is_list(ProxyUrl) ->
		    lhttpc_lib:parse_url(ProxyUrl)
	    end,
    {ChunkedUpload, Request} =
	lhttpc_lib:format_request(Path, Method, Hdrs, Host, Port, Body, PartialUpload,
				  {UseCookies, Cookies}),
    NewState =
	State#client_state{
	  method = Method,
	  request = Request,
	  requester = From,
	  request_headers = Hdrs,
	  attempts = lhttpc_lib:get_value(send_retry, Options, 1),
	  partial_upload = PartialUpload,
	  chunked_upload = ChunkedUpload,
	  partial_download = PartialDownload,
	  download_window =
	      lhttpc_lib:get_value(window_size, PartialDownloadOptions,
				  infinity),
	  download_proc =
	      lhttpc_lib:get_value(recv_proc, PartialDownloadOptions,
				  infinity),
	  part_size =
	      lhttpc_lib:get_value(part_size, PartialDownloadOptions,
				  infinity),
	  proxy = Proxy,
	  proxy_setup = (Socket /= undefined),
	  proxy_ssl_options =
	      lhttpc_lib:get_value(proxy_ssl_options, Options, [])},
    send_request(NewState);
handle_call(_Msg, _From, #client_state{request = undefined} = State) ->
    {reply, {error, no_pending_request}, State};
handle_call({send_body_part, _}, _From, State = #client_state{partial_upload = false}) ->
    {reply, {error, no_partial_upload}, State};
handle_call({trailers, _}, _From, State = #client_state{partial_upload = false}) ->
    {reply, {error, no_partial_upload}, State};
handle_call(get_body_part, _From, State = #client_state{partial_download = false}) ->
    {reply, {error, no_partial_download}, State};
handle_call(_Msg, _From, #client_state{socket = undefined} = State) ->
    {reply, {error, connection_closed}, State#client_state{request = undefined}};
handle_call({send_trailers, Trailers}, _From, State) ->
    case send_trailers(State, Trailers) of
        {ok, NewState} ->
            read_response(NewState);
        {Error, NewState} ->
            {reply, Error, NewState}
    end;
handle_call({send_body_part, http_eob}, From, State) ->
    case send_body_part(State, http_eob) of
        {ok, NewState} ->
            read_response(NewState#client_state{requester = From});
        {Error, NewState} ->
            {reply, Error, NewState}
    end;
handle_call({send_body_part, Data}, From, State) ->
    gen_server:reply(From, ok),
    {_Reply, NewState} = send_body_part(State, Data),
    {noreply, NewState};
						%We send the parts to the specified Pid.
handle_call(get_body_part, From, State) ->
    gen_server:reply(From, ok),
    {noreply, read_partial_body(State)}.


%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%%
%% @spec handle_cast(Msg, State) -> {noreply, State} |
%%                                  {noreply, State, Timeout} |
%%                                  {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_cast(stop, State) ->
    {stop, normal, State};
handle_cast(_Msg, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling all non call/cast messages
%%
%% @spec handle_info(Info, State) -> {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_info(_Info, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
%%
%% @spec terminate(Reason, State) -> void()
%% @end
%%--------------------------------------------------------------------
terminate(_Reason, State = #client_state{pool = Pool, host = Host, ssl = Ssl,
                                         socket = Socket, port = Port}) ->
    case Socket of
        undefined ->
            ok;
        _ ->
            case Pool of
                undefined ->
                    close_socket(State),
                    ok;
                _ ->
                    %return the control of the socket to the pool.
                    lhttpc_manager:client_done(Pool, Host, Port, Ssl, Socket)
            end
    end.



%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @end
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%==============================================================================
%% Internal functions
%%==============================================================================
%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
send_body_part(State = #client_state{socket = Socket, ssl = Ssl}, BodyPart) ->
    Data = encode_body_part(State, BodyPart),
    check_send_result(State, lhttpc_sock:send(Socket, Data, Ssl)).

%%------------------------------------------------------------------------------
%% @private
%% @doc This function creates a new socket connection if needed, and it also
%% handles the proxy connection.
%% @end
%%------------------------------------------------------------------------------
send_request(#client_state{attempts = 0} = State) ->
    {reply, {error, connection_closed}, State#client_state{request = undefined}};
send_request(#client_state{socket = undefined} = State) ->
    % if we dont get a keep alive from the previous request, the socket is undefined.
    case connect_socket(State) of
        {ok, NewState} ->
            send_request(NewState);
        {Error, NewState} ->
            {reply, Error, NewState}
    end;
send_request(#client_state{proxy = #lhttpc_url{}, proxy_setup = false,
                           host = DestHost, port = Port, socket = Socket} = State) ->
    %% use a proxy.
    #lhttpc_url{user = User, password = Passwd, is_ssl = Ssl} = State#client_state.proxy,
    Host = case inet_parse:address(DestHost) of
        {ok, {_, _, _, _, _, _, _, _}} ->
            %% IPv6 address literals are enclosed by square brackets (RFC2732)
            [$[, DestHost, $], $:, integer_to_list(Port)];
        _ ->
            [DestHost, $:, integer_to_list(Port)]
    end,
    ConnectRequest = [
            "CONNECT ", Host, " HTTP/1.1", ?HTTP_LINE_END,
            "Host: ", Host, ?HTTP_LINE_END,
            case User of
                [] ->
                    [];
                _ ->
                    ["Proxy-Authorization: Basic ",
                     base64:encode(User ++ ":" ++ Passwd), ?HTTP_LINE_END]
            end,
            ?HTTP_LINE_END],
    case lhttpc_sock:send(Socket, ConnectRequest, Ssl) of
        ok ->
            {Reply, NewState} = read_proxy_connect_response(State, nil, nil),
            {reply, Reply, NewState};
        {error, closed} ->
            close_socket(State),
            {reply, {error, proxy_connection_closed},
             State#client_state{socket = undefined, request = undefined}};
        {error, _Reason} ->
            close_socket(State),
            {reply, {error, proxy_connection_closed},
             State#client_state{socket = undefined, request = undefined}}
    end;
send_request(#client_state{socket = Socket, ssl = Ssl, request = Request,
                           attempts = Attempts} = State) ->
    %% no proxy
    case lhttpc_sock:send(Socket, Request, Ssl) of
        ok ->
            if
                %% {partial_upload, WindowSize} is used.
                State#client_state.partial_upload     ->
                    {reply, {ok, partial_upload}, State#client_state{attempts = 0}};
                not State#client_state.partial_upload ->
                    read_response(State)
            end;
        {error, closed} ->
            close_socket(State),
            send_request(State#client_state{socket = undefined, attempts = Attempts - 1});
        {error, _Reason} ->
            close_socket(State),
            {reply, {error, connection_closed}, State#client_state{socket = undefined,
                                                                   request = undefined}}
    end.

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
request_first_destination(#client_state{proxy = #lhttpc_url{} = Proxy}) ->
    {Proxy#lhttpc_url.host, Proxy#lhttpc_url.port, Proxy#lhttpc_url.is_ssl};
request_first_destination(#client_state{host = Host, port = Port, ssl = Ssl}) ->
    {Host, Port, Ssl}.

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
read_proxy_connect_response(State, StatusCode, StatusText) ->
    Socket = State#client_state.socket,
    ProxyIsSsl = (State#client_state.proxy)#lhttpc_url.is_ssl,
    case lhttpc_sock:recv(Socket, ProxyIsSsl) of
        {ok, {http_response, _Vsn, Code, Reason}} ->
            read_proxy_connect_response(State, Code, Reason);
        {ok, {http_header, _, _Name, _, _Value}} ->
            read_proxy_connect_response(State, StatusCode, StatusText);
        {ok, http_eoh} when StatusCode >= 100, StatusCode =< 199 ->
            %% RFC 2616, section 10.1:
            %% A client MUST be prepared to accept one or more
            %% 1xx status responses prior to a regular
            %% response, even if the client does not expect a
            %% 100 (Continue) status message. Unexpected 1xx
            %% status responses MAY be ignored by a user agent.
            read_proxy_connect_response(State, nil, nil);
        {ok, http_eoh} when StatusCode >= 200, StatusCode < 300 ->
            %% RFC2817, any 2xx code means success.
            ConnectOptions = State#client_state.connect_options,
            SslOptions = State#client_state.proxy_ssl_options,
            Timeout = State#client_state.connect_timeout,
            State2 = case ssl:connect(Socket, SslOptions ++ ConnectOptions, Timeout) of
                {ok, SslSocket} ->
                    State#client_state{socket = SslSocket, proxy_setup = true};
                {error, Reason} ->
                    close_socket(State),
                    {{error, {proxy_connection_failed, Reason}}, State}
            end,
            send_request(State2);
        {ok, http_eoh} ->
            {{error, {proxy_connection_refused, StatusCode, StatusText}}, State};
        {error, closed} ->
            close_socket(State),
            {{error, proxy_connection_closed},
             State#client_state{socket = undefined, request = undefined}};
        {error, Reason} ->
            {{error, {proxy_connection_failed, Reason}},
             State#client_state{request = undefined}}
    end.

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
send_trailers(#client_state{chunked_upload = true, socket = Socket, ssl= Ssl} = State, Trailers) ->
    Data = list_to_binary("0" ++ ?HTTP_LINE_END),
    Data2 = [Data, lhttpc_lib:format_hdrs(Trailers)],
    check_send_result(State, lhttpc_sock:send(Socket, Data2, Ssl));
send_trailers(#client_state{chunked_upload = false} = State, _Trailers) ->
    {{error, trailers_not_allowed}, State}.

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
encode_body_part(#client_state{chunked_upload = true}, http_eob) ->
    list_to_binary("0" ++ ?HTTP_LINE_END ++ ?HTTP_LINE_END);
encode_body_part(#client_state{chunked_upload = false}, http_eob) ->
    <<>>;
encode_body_part(#client_state{chunked_upload = true}, Data) ->
    Size = list_to_binary(erlang:integer_to_list(iolist_size(Data), 16)),
    [Size, <<?HTTP_LINE_END>>, Data, <<?HTTP_LINE_END>>];
encode_body_part(#client_state{chunked_upload = false}, Data) ->
    Data.

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
check_send_result(State, ok) ->
    {ok, State};
check_send_result(State, Error) ->
    close_socket(State),
    {Error, State#client_state{socket = undefined, request = undefined}}.

%%------------------------------------------------------------------------------
%% @private
%% @doc @TODO This does not handle redirects at the moment.
%% @end
%%------------------------------------------------------------------------------
-spec read_response(#client_state{}) -> {any(), socket()} | no_return().
read_response(#client_state{socket = Socket, ssl = Ssl, use_cookies = UseCookies,
                            request_headers = ReqHdrs, cookies = Cookies} = State) ->
    case lhttpc_protocol:recv(Socket, Ssl) of
	{_Vsn, <<$1,_,_>>, _Reason, _Hdrs, _Body} ->
	    %% RFC 2616, section 10.1:
            %% A client MUST be prepared to accept one or more
            %% 1xx status responses prior to a regular
            %% response, even if the client does not expect a
            %% 100 (Continue) status message. Unexpected 1xx
            %% status responses MAY be ignored by a user agent.
            read_response(State);
	{Vsn, Status, Reason, NewHdrs, Body} ->
	    FinalCookies =
		case UseCookies of
		    true ->
			lhttpc_lib:update_cookies(NewHdrs, Cookies);
		    _ ->
			[]
		end,
	    NewSocket = maybe_close_socket(State, Vsn, ReqHdrs, NewHdrs),
	    {reply, {ok, {{Status, Reason}, NewHdrs, Body}},
	     State#client_state{socket = NewSocket,
				   request = undefined,
				   cookies = FinalCookies}};
	{error, closed} ->
            %% TODO does it work for partial uploads? I think should return an error

            % Either we only noticed that the socket was closed after we
            % sent the request, the server closed it just after we put
            % the request on the wire or the server has some isses and is
            % closing connections without sending responses.
            % If this the first attempt to send the request, we will try again.
            close_socket(State),
            send_request(State#client_state{socket = undefined});
	{error, Reason} ->
	    close_socket(State),
	    {reply, {error, Reason}, State#client_state{socket = undefined}}
    end.

%%------------------------------------------------------------------------------
%%% @private
%%% @doc Called when {partial_download, PartialDownloadOptions} option is used.
%%% @end
%%------------------------------------------------------------------------------
read_partial_body(State=#client_state{body_length = chunked}) ->
    Window = State#client_state.download_window,
    read_partial_chunked_body(State, Window, 0, [], 0);
read_partial_body(State=#client_state{download_info = {Vsn, Hdrs}, body_length = infinite}) ->
    check_infinite_response(Vsn, Hdrs),
    read_partial_infinite_body(State, State#client_state.download_window);
read_partial_body(State=#client_state{body_length = {fixed_length, ContentLength}}) ->
    read_partial_finite_body(State, ContentLength, State#client_state.download_window).

%%------------------------------------------------------------------------------
%%% @private
%%------------------------------------------------------------------------------
read_partial_finite_body(State , 0, _Window) ->
    reply_end_of_body(State, []),
    State#client_state{request = undefined};
read_partial_finite_body(#client_state{download_proc = To} = State, ContentLength, 0) ->
    %finished the window, reply to ask for another call to get_body_part
    To ! {body_part, window_finished},
    State#client_state{body_length = {fixed_length, ContentLength}};
read_partial_finite_body(State, ContentLength, Window) when Window > 0->
    case read_body_part(State, ContentLength) of
        {ok, Bin} ->
            State#client_state.download_proc ! {body_part, Bin},
            Length = ContentLength - iolist_size(Bin),
            read_partial_finite_body(State, Length, lhttpc_lib:dec(Window));
        {error, Reason} ->
            State#client_state.download_proc ! {body_part_error, Reason},
            close_socket(State),
            State#client_state{request = undefined, socket = undefined}
    end.

%%------------------------------------------------------------------------------
%%% @private
%%------------------------------------------------------------------------------
read_body_part(#client_state{part_size = infinity, socket = Socket, ssl = Ssl}, _ContentLength) ->
    lhttpc_sock:recv(Socket, Ssl);
read_body_part(#client_state{socket = Socket, ssl = Ssl, part_size = PartSize},
               ContentLength) when PartSize =< ContentLength ->
    lhttpc_sock:recv(Socket, PartSize, Ssl);
read_body_part(#client_state{socket = Socket, ssl = Ssl, part_size = PartSize},
               ContentLength) when PartSize > ContentLength ->
    lhttpc_sock:recv(Socket, ContentLength, Ssl).

%%------------------------------------------------------------------------------
%%% @private
%%------------------------------------------------------------------------------
read_partial_chunked_body(#client_state{download_proc = Pid} = State, 0, 0, _Buffer, 0) ->
    %we ask for another call to get_body_part
    Pid ! {body_part, http_eob},
    State;
read_partial_chunked_body(#client_state{download_info = {_Vsn, Hdrs}, socket = Socket,
                                        ssl = Ssl, part_size = PartSize} = State,
                          Window, BufferSize, Buffer, 0) ->
    case read_chunk_size(Socket, Ssl) of
        0 ->
            reply_chunked_part(State, Buffer, Window),
            {Trailers, _NewHdrs} = read_trailers(Socket, Ssl, [], Hdrs),
            reply_end_of_body(State, Trailers),
            State#client_state{request = undefined};
        ChunkSize when PartSize =:= infinity ->
            Chunk = read_chunk(Socket, Ssl, ChunkSize),
            NewWindow = reply_chunked_part(State, [Chunk | Buffer], Window),
            read_partial_chunked_body(State, NewWindow, 0, [], 0);
        ChunkSize when BufferSize + ChunkSize >= PartSize ->
            {Chunk, RemSize} = read_partial_chunk(Socket, Ssl,
                                                  PartSize - BufferSize, ChunkSize),
            NewWindow = reply_chunked_part(State, [Chunk | Buffer], Window),
            read_partial_chunked_body(State, NewWindow, 0, [], RemSize);
        ChunkSize ->
            Chunk = read_chunk(Socket, Ssl, ChunkSize),
            read_partial_chunked_body(State, Window,
                                      BufferSize + ChunkSize, [Chunk | Buffer], 0)
    end;
read_partial_chunked_body(#client_state{socket = Socket, ssl = Ssl, part_size = PartSize} = State,
                          Window, BufferSize, Buffer, RemSize) ->
    if
        BufferSize + RemSize >= PartSize ->
            {Chunk, NewRemSize} = read_partial_chunk(Socket, Ssl,
                                                     PartSize - BufferSize, RemSize),
            NewWindow = reply_chunked_part(State, [Chunk | Buffer], Window),
            read_partial_chunked_body(State, NewWindow, 0, [], NewRemSize);
        BufferSize + RemSize < PartSize ->
            Chunk = read_chunk(Socket, Ssl, RemSize),
            read_partial_chunked_body(State, Window, BufferSize + RemSize, [Chunk | Buffer], 0)
    end.

%%------------------------------------------------------------------------------
%%% @private
%%------------------------------------------------------------------------------
read_chunk_size(Socket, Ssl) ->
    lhttpc_sock:setopts(Socket, [{packet, line}], Ssl),
    case lhttpc_sock:recv(Socket, Ssl) of
        {ok, ChunkSizeExt} ->
            chunk_size(ChunkSizeExt);
        {error, Reason} ->
            erlang:error(Reason)
    end.

%%------------------------------------------------------------------------------
%%% @private
%%------------------------------------------------------------------------------
reply_chunked_part(_State, [], Window) ->
    Window;
reply_chunked_part(#client_state{download_proc = Pid}, Buffer, Window) ->
    Pid ! {body_part, list_to_binary(lists:reverse(Buffer))},
    lhttpc_lib:dec(Window).

%%------------------------------------------------------------------------------
%%% @private
%%------------------------------------------------------------------------------
chunk_size(Bin) ->
    erlang:list_to_integer(lists:reverse(chunk_size(Bin, [])), 16).

%%------------------------------------------------------------------------------
%%% @private
%%------------------------------------------------------------------------------
chunk_size(<<$;, _/binary>>, Chars) ->
    Chars;
chunk_size(<<?HTTP_LINE_END, _/binary>>, Chars) ->
    Chars;
chunk_size(<<$\s, Binary/binary>>, Chars) ->
    %% Facebook's HTTP server returns a chunk size like "6  \r\n"
    chunk_size(Binary, Chars);
chunk_size(<<Char, Binary/binary>>, Chars) ->
    chunk_size(Binary, [Char | Chars]).

%%------------------------------------------------------------------------------
%%% @private
%%------------------------------------------------------------------------------
read_partial_chunk(Socket, Ssl, ChunkSize, ChunkSize) ->
    {read_chunk(Socket, Ssl, ChunkSize), 0};
read_partial_chunk(Socket, Ssl, Size, ChunkSize) ->
    case lhttpc_sock:recv(Socket, Size, Ssl) of
        {ok, Chunk} ->
            {Chunk, ChunkSize - Size};
        {error, Reason} ->
            {error, Reason}
    end.

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
read_chunk(Socket, Ssl, Size) ->
    case lhttpc_sock:recv(Socket, Size + 2, Ssl) of
        {ok, <<Chunk:Size/binary, ?HTTP_LINE_END>>} ->
            Chunk;
        {ok, Data} ->
            {error, {invalid_chunk, Data}};
        {error, Reason} ->
            {error, Reason}
    end.

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
-spec read_trailers(socket(), boolean(), any(), any()) ->
    {any(), any()} | no_return().
read_trailers(Socket, Ssl, Trailers, Hdrs) ->
    lhttpc_sock:setopts(Socket, [{packet, httph}], Ssl),
    case lhttpc_sock:recv(Socket, Ssl) of
        {ok, http_eoh} ->
            {Trailers, Hdrs};
        {ok, {http_header, _, Name, _, Value}} ->
            Header = {lhttpc_lib:maybe_atom_to_list(Name), Value},
            read_trailers(Socket, Ssl, [Header | Trailers], [Header | Hdrs]);
        {error, {http_error, Data}} ->
            erlang:error({bad_trailer, Data})
    end.

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
%-spec reply_end_of_body(#client_state{}, any(), any()) -> 'no_return'.
reply_end_of_body(#client_state{download_proc = To}, Trailers) ->
    To ! {http_eob, Trailers}.

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
read_partial_infinite_body(#client_state{download_proc = Pid} = State, 0) ->
    Pid ! {body_part, window_finished},
    State;
read_partial_infinite_body(#client_state{download_proc = Pid} = State, Window)
        when Window >= 0 ->
    case read_infinite_body_part(State) of
        http_eob ->
            reply_end_of_body(State, []),
            State#client_state{request = undefined};
        Bin ->
            Pid ! {body_part, Bin},
            read_partial_infinite_body(State, lhttpc_lib:dec(Window))
    end.

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
-spec read_infinite_body_part(#client_state{}) -> bodypart() | no_return().
read_infinite_body_part(#client_state{socket = Socket, ssl = Ssl}) ->
    case lhttpc_sock:recv(Socket, Ssl) of
        {ok, Data} ->
            Data;
        {error, closed} ->
            http_eob;
        {error, Reason} ->
            erlang:error(Reason)
    end.

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
check_infinite_response({1, Minor}, Hdrs) when Minor >= 1 ->
    HdrValue = lhttpc_lib:header_value(<<"connection">>, Hdrs, <<"keep-alive">>),
    case lhttpc_lib:compare_strings(HdrValue, <<"close">>) of
        true -> ok;
        _       -> erlang:error(no_content_length)
    end;
check_infinite_response(_, Hdrs) ->
    HdrValue = lhttpc_lib:header_value(<<"connection">>, Hdrs, <<"close">>),
    case lhttpc_lib:compare_strings(HdrValue, <<"keep-alive">>) of
        true -> erlang:error(no_content_length);
        _          -> ok
    end.

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
maybe_close_socket(#client_state{socket = Socket} = State, {1, Minor},
                   ReqHdrs, RespHdrs) when Minor >= 1->
    ClientConnection = lhttpc_lib:compare_strings(?CONNECTION_HDR(ReqHdrs, <<"keep-alive">>), <<"close">>),
    ServerConnection = lhttpc_lib:compare_strings(?CONNECTION_HDR(RespHdrs, <<"keep-alive">>), <<"close">>),
    if
        ClientConnection orelse ServerConnection ->
            close_socket(State),
            undefined;
        (not ClientConnection) andalso (not ServerConnection) ->
            Socket
    end;
maybe_close_socket(#client_state{socket = Socket} = State, _, ReqHdrs, RespHdrs) ->
    ClientConnection = lhttpc_lib:compare_strings(?CONNECTION_HDR(ReqHdrs, <<"keep-alive">>), <<"close">>),
    ServerConnection = lhttpc_lib:compare_strings(?CONNECTION_HDR(RespHdrs, <<"close">>), <<"keep-alive">>),
    if
        ClientConnection orelse (not ServerConnection) ->
            close_socket(State),
            undefined;
        (not ClientConnection) andalso ServerConnection ->
            Socket
    end.

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
-spec is_ipv6_host(host()) -> boolean().
is_ipv6_host(Host) ->
    case inet_parse:address(Host) of
        {ok, {_, _, _, _, _, _, _, _}} ->
            true;
        {ok, {_, _, _, _}} ->
            false;
        _ ->
            % Prefer IPv4 over IPv6.
            case inet:getaddr(Host, inet) of
                {ok, _} ->
                    false;
                _ ->
                    case inet:getaddr(Host, inet6) of
                        {ok, _} ->
                            true;
                        _ ->
                            false
                    end
            end
    end.

% What about the timeout?
%%------------------------------------------------------------------------------
%% @private
%% @doc If we are using a pool, it takes the socket from the pool if it exist,
%% otherwise it creates a new socket. If we are not using pool, it just creates
%% a new socket. If the option pool_ensure its set to true, it creates a
%% pool dinamically.
%% @end
%%------------------------------------------------------------------------------
connect_socket(#client_state{pool = undefined} = State) ->
    connect_socket_return(new_socket(State), State);
connect_socket(#client_state{pool = _Pool} = State) ->
    connect_socket_return(connect_pool(State), State).

connect_socket_return({ok, Socket}, State) ->
    {ok, State#client_state{socket = Socket}};
connect_socket_return(Error, State) ->
    {Error, State}.

-spec connect_pool(#client_state{}) -> {ok, socket()} | {error, atom()}.
connect_pool(State = #client_state{pool_options = Options,
                                   pool = Pool}) ->
    {Host, Port, Ssl} = request_first_destination(State),
    case lhttpc_manager:ensure_call(Pool, self(), Host, Port, Ssl, Options) of
        {ok, no_socket} ->
            %% ensure_call does not open a socket if the pool doesnt have one
            new_socket(State);
        Reply ->
            Reply
    end.

%%------------------------------------------------------------------------------
%% @private
%% @doc Creates a new socket using the options included in the client state.
%% end
%%------------------------------------------------------------------------------
new_socket(#client_state{connect_timeout = Timeout, connect_options = ConnectOptions} = State) ->
    {Host, Port, Ssl} = request_first_destination(State),
    ConnectOptions2 = case (not lists:member(inet, ConnectOptions)) andalso
        (not lists:member(inet6, ConnectOptions)) andalso
        is_ipv6_host(Host) of
        true ->
            [inet6 | ConnectOptions];
        false ->
            ConnectOptions
    end,
    SocketOptions = [binary, {packet, raw}, {nodelay, true}, {reuseaddr, true},
                     {active, false} | ConnectOptions2],
    try lhttpc_sock:connect(Host, Port, SocketOptions, Timeout, Ssl) of
        {ok, Socket} ->
            {ok, Socket};
        {error, etimedout} ->
            %% TCP stack decided to give up
            {error, connect_timeout};
        {error, timeout} ->
            {error, connect_timeout};
        {error, 'record overflow'} ->
            {error, ssl_error};
        {error, _} = Error ->
            Error
    catch
        exit:{{{badmatch, {error, {asn1, _}}}, _}, _} ->
            {error, ssl_decode_error};
        Type:Error ->
            error_logger:error_msg("Socket connection error: ~p ~p, ~p",
                                   [Type, Error, erlang:get_stacktrace()]),
            {error, connection_error}
    end.

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
close_socket(_State = #client_state{socket = Socket, pool = Pool, ssl = Ssl}) ->
    case Pool of
        undefined ->
            lhttpc_sock:close(Socket, Ssl);
        _ ->
            lhttpc_manager:close_socket(Pool, Socket)
    end.
