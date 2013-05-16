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
-module(lhttpc).

%exported functions
-export([connect/2,
	 request/6,
	 request/7,
	 request/10,
         send_body_part/2,
         send_body_part/3,
         send_trailers/2,
	 send_trailers/3,
         disconnect/1]).

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

-record(client_state, {
        host :: string(),
        port = 80 :: port_num(),
        ssl = false :: boolean(),
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
        %% in case of infinity we read whatever data we can get from
        %% the wire at that point or in case of chunked one chunk
        attempts = 0 :: integer(),
        proxy :: undefined | #lhttpc_url{},
        proxy_ssl_options = [] :: [any()],
        proxy_setup = false :: boolean(),
	host_header
        }).

%%==============================================================================
%% Exported functions
%%==============================================================================
%%------------------------------------------------------------------------------
%% @doc Starts a Client.
%% @end
%%------------------------------------------------------------------------------
connect(Destination, Options) ->
    verify_options(Options),
    gen_server:start(?MODULE, {Destination, Options}, []).

%%------------------------------------------------------------------------------
%% @spec (UploadState :: UploadState, BodyPart :: BodyPart) -> Result
%%   BodyPart = iodata() | binary()
%%   Timeout = integer() | infinity
%%   Result = {error, Reason} | UploadState
%%   Reason = connection_closed | connect_timeout | timeout
%% @doc Sends a body part to an ongoing request when
%% `{partial_upload, WindowSize}' is used. The default timeout, `infinity'
%% will be used. Notice that if `WindowSize' is infinity, this call will never
%% block.
%% Would be the same as calling
%% `send_body_part(UploadState, BodyPart, infinity)'.
%% @end
%%------------------------------------------------------------------------------
-spec send_body_part(upload_state(), bodypart()) -> result().
send_body_part({Pid, Window}, IoList) ->
    send_body_part({Pid, Window}, IoList, infinity).

%%------------------------------------------------------------------------------
%% @spec (UploadState :: UploadState, BodyPart :: BodyPart, Timeout) -> Result
%%   BodyPart = iodata() | binary()
%%   Timeout = integer() | infinity
%%   Result = {error, Reason} | UploadState
%%   Reason = connection_closed | connect_timeout | timeout
%% @doc Sends a body part to an ongoing request when
%% `{partial_upload, WindowSize}' is used.
%% `Timeout' is the timeout for the request in milliseconds.
%%
%% If the window size reaches 0 the call will block for at maximum Timeout
%% milliseconds. If there is no acknowledgement received during that time the
%% the request is cancelled and `{error, timeout}' is returned.
%%
%% As long as the window size is larger than 0 the function will return
%% immediately after sending the body part to the request handling process.
%%
%% The `BodyPart' `http_eob' signals an end of the entity body, the request
%% is considered sent and the response will be read from the socket. If
%% there is no response within `Timeout' milliseconds, the request is
%% canceled and `{error, timeout}' is returned.
%% @end
%%------------------------------------------------------------------------------
-spec send_body_part(upload_state(), bodypart(), timeout()) -> result().
send_body_part(Client, Part, Timeout) ->
    gen_server:call(Client, {send_body_part, Part}, Timeout).

%%------------------------------------------------------------------------------
%% @spec (UploadState :: UploadState, Trailers) -> Result
%%   Header = string() | binary() | atom()
%%   Value = string() | binary()
%%   Result = {ok, {{StatusCode, ReasonPhrase}, Hdrs, ResponseBody}}
%%            | {error, Reason}
%%   Reason = connection_closed | connect_timeout | timeout
%% @doc Sends trailers to an ongoing request when `{partial_upload,
%% WindowSize}' is used and no `Content-Length' was specified. The default
%% timout `infinity' will be used. Plase note that after this the request is
%% considered complete and the response will be read from the socket.
%% Would be the same as calling
%% `send_trailers(UploadState, BodyPart, infinity)'.
%% @end
%%------------------------------------------------------------------------------
-spec send_trailers({pid(), window_size()}, headers()) -> result().
send_trailers(Client, Trailers) ->
    send_trailers(Client, Trailers, infinity).

%%------------------------------------------------------------------------------
%% @spec (UploadState :: UploadState, Trailers, Timeout) -> Result
%%   Trailers = [{Header, Value}]
%%   Header = string() | binary() | atom()
%%   Value = string() | binary()
%%   Timeout = integer() | infinity
%%   Result = {ok, {{StatusCode, ReasonPhrase}, Hdrs, ResponseBody}}
%%            | {error, Reason}
%%   Reason = connection_closed | connect_timeout | timeout
%% @doc Sends trailers to an ongoing request when
%% `{partial_upload, WindowSize}' is used and no `Content-Length' was
%% specified.
%% `Timeout' is the timeout for sending the trailers and reading the
%% response in milliseconds.
%%
%% Sending trailers also signals the end of the entity body, which means
%% that no more body parts, or trailers can be sent and the response to the
%% request will be read from the socket. If no response is received within
%% `Timeout' milliseconds the request is canceled and `{error, timeout}' is
%% returned.
%% @end
%%------------------------------------------------------------------------------
-spec send_trailers({pid(), window_size()}, headers(), timeout()) -> result().
send_trailers(Client, Trailers, Timeout) when is_list(Trailers), is_pid(Client)->
    gen_server:call(Client, {send_trailers, Trailers}, Timeout).

%%------------------------------------------------------------------------------
%% @doc Stops a Client.
%% @end
%%------------------------------------------------------------------------------
-spec disconnect(pid()) -> ok.
disconnect(Client) ->
    gen_server:cast(Client, stop).

%%------------------------------------------------------------------------------
%% @doc Makes a request using a client already connected.
%% @end
%%------------------------------------------------------------------------------
-spec request(pid(), string(), method(), headers(), iodata(), pos_timeout()) -> result().
request(Client, Path, Method, Hdrs, Body, Timeout) ->
    request(Client, Path, Method, Hdrs, Body, Timeout, []).

%%------------------------------------------------------------------------------
%% @spec (Client, Path, Method, Hdrs, RequestBody, Timeout, Options) -> ok
%%    From = pid()
%%    Method = string()
%%    Hdrs = [Header]
%%    Header = {string() | atom(), string()}
%%    Body = iolist()
%%    Options = [Option]
%%    Option = {connect_timeout, Milliseconds}
%%
%% Authorization must be part of the headers
%%
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec request(pid(), string(), method(), headers(), iolist(), integer(), options()) -> result().
request(Client, Path, Method, Hdrs, Body, Timeout, []) ->
    request(Client, Path, Method, Hdrs, Body, false, false, 1, [], Timeout);
request(Client, Path, Method, Hdrs, Body, Timeout, Options) ->
    verify_options(Options),
    PartialUpload = lhttpc_lib:get_value(partial_upload, Options, false),
    ProxyInfo = lhttpc_lib:get_value(proxy, Options, false),
    SendRetry = lhttpc_lib:get_value(send_retry, Options, 1),
    ProxySsl = lhttpc_lib:get_value(proxy_ssl_options, Options, []),
    request(Client, Path, Method, Hdrs, Body, PartialUpload, ProxyInfo, SendRetry,
	    ProxySsl, Timeout).

%%------------------------------------------------------------------------------
%% @spec (Host, Port, Ssl, Path, Method, Hdrs, RequestBody, Timeout, Options) ->
%%                                                                        Result
%%   Host = string()
%%   Port = integer()
%%   Ssl = boolean()
%%   Path = string()
%%   Method = string() | atom()
%%   Hdrs = [{Header, Value}]
%%   Header = string() | binary() | atom()
%%   Value = string() | binary()
%%   RequestBody = iodata()
%%   Timeout = integer() | infinity
%%   Options = [Option]
%%   Option = {connect_timeout, Milliseconds | infinity} |
%%            {connect_options, [ConnectOptions]} |
%%            {send_retry, integer()} |
%%            {partial_upload, WindowSize} |
%%            {proxy, ProxyUrl} |
%%            {proxy_ssl_options, SslOptions}
%%   Milliseconds = integer()
%%   WindowSize = integer()
%%   ProxyUrl = string()
%%   SslOptions = [any()]
%%   PartSize = integer() | infinity
%%   Result = {ok, {{StatusCode, ReasonPhrase}, Hdrs, ResponseBody}}
%%          | {error, Reason}
%%   StatusCode = integer()
%%   ReasonPhrase = string()
%%   ResponseBody = binary() | pid() | undefined
%%   Reason = connection_closed | connect_timeout | timeout
%% @doc Sends a request with a body.
%%
%% Instead of building and parsing URLs the target server is specified with
%% a host, port, weither SSL should be used or not and a path on the server.
%% For instance, if you want to request http://example.com/foobar you would
%% use the following:<br/>
%% `Host' = `"example.com"'<br/>
%% `Port' = `80'<br/>
%% `Ssl' = `false'<br/>
%% `Path' = `"/foobar"'<br/>
%% `Path' must begin with a forward slash `/'.
%%
%% `Method' is either a string, stating the HTTP method exactly as in the
%% protocol, i.e: `"POST"' or `"GET"'. It could also be an atom, which is
%% then coverted to an uppercase (if it isn't already) string.
%%
%% `Hdrs' is a list of headers to send. Mandatory headers such as
%% `Host', `Content-Length' or `Transfer-Encoding' (for some requests)
%% are added automatically.
%%
%% `Body' is the entity to send in the request. Please don't include entity
%% bodies where there shouldn't be any (such as for `GET').
%%
%% `Timeout' is the timeout for the request in milliseconds.
%%
%% `Options' is a list of options.
%%
%% Options:
%%
%% `{connect_timeout, Milliseconds}' specifies how many milliseconds the
%% client can spend trying to establish a connection to the server. This
%% doesn't affect the overall request timeout. However, if it's longer than
%% the overall timeout it will be ignored. Also note that the TCP layer my
%% choose to give up earlier than the connect timeout, in which case the
%% client will also give up. The default value is infinity, which means that
%% it will either give up when the TCP stack gives up, or when the overall
%% request timeout is reached.
%%
%% `{connect_options, Options}' specifies options to pass to the socket at
%% connect time. This makes it possible to specify both SSL options and
%% regular socket options, such as which IP/Port to connect from etc.
%% Some options must not be included here, namely the mode, `binary'
%% or `list', `{active, boolean()}', `{active, once}' or `{packet, Packet}'.
%% These options would confuse the client if they are included.
%% Please note that these options will only have an effect on *new*
%% connections, and it isn't possible for different requests
%% to the same host uses different options unless the connection is closed
%% between the requests. Using HTTP/1.0 or including the "Connection: close"
%% header would make the client close the connection after the first
%% response is received.
%%
%% `{send_retry, N}' specifies how many times the client should retry
%% sending a request if the connection is closed after the data has been
%% sent. The default value is `1'. If `{partial_upload, WindowSize}'
%% (see below) is specified, the client cannot retry after the first part
%% of the body has been sent since it doesn't keep the whole entitity body
%% in memory.
%%
%% `{partial_upload, WindowSize}' means that the request entity body will be
%% supplied in parts to the client by the calling process. The `WindowSize'
%% specifies how many parts can be sent to the process controlling the socket
%% before waiting for an acknowledgement. This is to create a kind of
%% internal flow control if the network is slow and the client process is
%% blocked by the TCP stack. Flow control is disabled if `WindowSize' is
%% `infinity'. If `WindowSize' is an integer, it must be >= 0. If partial
%% upload is specified and no `Content-Length' is specified in `Hdrs' the
%% client will use chunked transfer encoding to send the entity body.
%% If a content length is specified, this must be the total size of the entity
%% body.
%% The call to {@link request/6} will return `{ok, UploadState}'. The
%% `UploadState' is supposed to be used as the first argument to the {@link
%% send_body_part/2} or {@link send_body_part/3} functions to send body parts.
%% Partial upload is intended to avoid keeping large request bodies in
%% memory but can also be used when the complete size of the body isn't known
%% when the request is started.
%%
%% `{proxy, ProxyUrl}' if this option is specified, a proxy server is used as
%% an intermediary for all communication with the destination server. The link
%% to the proxy server is established with the HTTP CONNECT method (RFC2817).
%% Example value: {proxy, "http://john:doe@myproxy.com:3128"}
%%
%% `{proxy_ssl_options, SslOptions}' this is a list of SSL options to use for
%% the SSL session created after the proxy connection is established. For a
%% list of all available options, please check OTP's ssl module manpage.
%% @end
%%------------------------------------------------------------------------------
request(Client, Path, Method, Hdrs, Body, PartialUpload, ProxyInfo, SendRetry,
	ProxySsl, Timeout) ->
    try
	gen_server:call(Client, {request, Path, Method, Hdrs, Body, PartialUpload,
				 ProxyInfo, SendRetry, ProxySsl}, Timeout)
    catch
	exit:{timeout, _} ->
	    {error, timeout}
    end.

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================
init({Destination, Options}) ->
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
    State = #client_state{host = Host, port = Port, ssl = Ssl,
                          connect_timeout = ConnectTimeout,
                          connect_options = ConnectOptions,
                          use_cookies = UseCookies,
			  host_header = lhttpc_lib:host_header(Host, Port)},
    %% Get a socket or exit
    case connect_socket(State) of
        {ok, NewState} ->
            {ok, NewState};
        {{error, Reason}, _} ->
            {stop, Reason}
    end.

%%------------------------------------------------------------------------------
%% @doc This function fills in the Client record used in the requests and obtains
%% the socket.
%% @end
%%------------------------------------------------------------------------------
handle_call({request, Path, Method, Hdrs, Body, PartialUpload, ProxyInfo, SendRetry,
	     ProxySsl}, From,
            State = #client_state{ssl = Ssl, host_header = Host,
                                  socket = Socket, cookies = Cookies,
                                  use_cookies = UseCookies}) ->
    Proxy = case ProxyInfo of
		false ->
		    undefined;
		{proxy, ProxyUrl} when is_list(ProxyUrl), not Ssl ->
		    %% The point of HTTP CONNECT proxying is to use TLS tunneled in
		    %% a plain HTTP/1.1 connection to the proxy (RFC2817).
		    throw(origin_server_not_https);
		{proxy, ProxyUrl} when is_list(ProxyUrl) ->
		    lhttpc_lib:parse_url(ProxyUrl)
	    end,
    {ChunkedUpload, Request} =
	lhttpc_lib:format_request(Path, Method, Hdrs, Host, Body, PartialUpload,
				  {UseCookies, Cookies}),
    NewState =
	State#client_state{
	  method = Method,
	  request = Request,
	  requester = From,
	  request_headers = Hdrs,
	  attempts = SendRetry,
	  partial_upload = PartialUpload,
	  chunked_upload = ChunkedUpload,
	  proxy = Proxy,
	  proxy_setup = (Socket /= undefined),
	  proxy_ssl_options = ProxySsl},
    send_request(NewState);
handle_call(_Msg, _From, #client_state{request = undefined} = State) ->
    {reply, {error, no_pending_request}, State};
handle_call({send_body_part, _}, _From, State = #client_state{partial_upload = false}) ->
    {reply, {error, no_partial_upload}, State};
handle_call(_Msg, _From, #client_state{socket = undefined} = State) ->
    {reply, {error, connection_closed}, State#client_state{request = undefined}};
handle_call({send_trailers, Trailers}, _From, State) ->
    case int_send_trailers(State, Trailers) of
        {ok, NewState} ->
            read_response(NewState);
        {Error, NewState} ->
            {reply, Error, NewState}
    end;
handle_call({send_body_part, http_eob}, From, State) ->
    case int_send_body_part(State, http_eob) of
        {ok, NewState} ->
            read_response(NewState#client_state{requester = From});
        {Error, NewState} ->
            {reply, Error, NewState}
    end;
handle_call({send_body_part, Data}, From, State) ->
    gen_server:reply(From, ok),
    {_Reply, NewState} = int_send_body_part(State, Data),
    {noreply, NewState}.


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
terminate(_Reason, #client_state{socket = Socket, ssl = Ssl}) ->
    case Socket of
        undefined ->
            ok;
        _ ->
	    lhttpc_sock:close(Socket, Ssl),
	    ok
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
int_send_body_part(State = #client_state{socket = Socket, ssl = Ssl}, BodyPart) ->
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
            lhttpc_sock:close(Socket, Ssl),
            {reply, {error, proxy_connection_closed},
             State#client_state{socket = undefined, request = undefined}};
        {error, _Reason} ->
            lhttpc_sock:close(Socket, Ssl),
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
            lhttpc_sock:close(Socket, Ssl),
            send_request(State#client_state{socket = undefined, attempts = Attempts - 1});
        {error, _Reason} ->
            lhttpc_sock:close(Socket, Ssl),
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
                    lhttpc_sock:close(State#client_state.socket, State#client_state.ssl),
                    {{error, {proxy_connection_failed, Reason}}, State}
            end,
            send_request(State2);
        {ok, http_eoh} ->
            {{error, {proxy_connection_refused, StatusCode, StatusText}}, State};
        {error, closed} ->
            lhttpc_sock:close(Socket, ProxyIsSsl),
            {{error, proxy_connection_closed},
             State#client_state{socket = undefined, request = undefined}};
        {error, Reason} ->
            {{error, {proxy_connection_failed, Reason}},
             State#client_state{request = undefined}}
    end.

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
int_send_trailers(#client_state{chunked_upload = true, socket = Socket, ssl= Ssl} = State, Trailers) ->
    Data = list_to_binary("0" ++ ?HTTP_LINE_END),
    Data2 = [Data, lhttpc_lib:format_hdrs(Trailers)],
    check_send_result(State, lhttpc_sock:send(Socket, Data2, Ssl));
int_send_trailers(#client_state{chunked_upload = false} = State, _Trailers) ->
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
    lhttpc_sock:close(State#client_state.socket, State#client_state.ssl),
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
	{_Vsn, <<$1,_,_>>, _Reason, _Cookies, _Hdrs, _Body} ->
	    %% RFC 2616, section 10.1:
            %% A client MUST be prepared to accept one or more
            %% 1xx status responses prior to a regular
            %% response, even if the client does not expect a
            %% 100 (Continue) status message. Unexpected 1xx
            %% status responses MAY be ignored by a user agent.
            read_response(State);
	{Vsn, Status, Reason, NewCookies, NewHdrs, Body} ->
	    FinalCookies =
		case UseCookies of
		    true ->
			lhttpc_lib:update_cookies(NewCookies, Cookies);
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
            lhttpc_sock:close(Socket, Ssl),
            send_request(State#client_state{socket = undefined});
	{error, Reason} ->
	    lhttpc_sock:close(Socket, Ssl),
	    {reply, {error, Reason}, State#client_state{socket = undefined}}
    end.

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
maybe_close_socket(#client_state{socket = Socket} = State, {1, Minor},
                   ReqHdrs, RespHdrs) when Minor >= 1->
    ClientConnection = case lists:keyfind(<<"connection">>, 1, ReqHdrs) of
			   false ->
			       false;
			   {_, Value} ->
			       lhttpc_lib:is_close(Value)
		       end,
    ServerConnection = case lists:keyfind(<<"connection">>, 1, RespHdrs) of
			   false ->
			       false;
			   {_, Value2} ->
			       lhttpc_lib:is_close(Value2)
		       end,
    if
        ClientConnection orelse ServerConnection ->
            lhttpc_sock:close(Socket, State#client_state.ssl),
            undefined;
        (not ClientConnection) andalso (not ServerConnection) ->
            Socket
    end;
maybe_close_socket(#client_state{socket = Socket} = State, _, ReqHdrs, RespHdrs) ->
    ClientConnection = case lists:keyfind(<<"connection">>, 1, ReqHdrs) of
			   false ->
			       false;
			   {_, Value} ->
			       lhttpc_lib:is_close(Value)
		       end,
    ServerConnection = case lists:keyfind(<<"connection">>, 1, RespHdrs) of
			   false ->
			       false;
			   {_, Value2} ->
			       lhttpc_lib:is_keep_alive(Value2)
		       end,
    if
        ClientConnection orelse (not ServerConnection) ->
            lhttpc_sock:close(Socket, State#client_state.ssl),
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
%% Creates a new socket.
%% @end
%%------------------------------------------------------------------------------
connect_socket(State) ->
    case new_socket(State) of
	{ok, Socket} ->
	    {ok, State#client_state{socket = Socket}};
	Error ->
	    {Error, State}
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
-spec verify_options(options()) -> ok | any().
verify_options([{send_retry, N} | Options]) when is_integer(N), N >= 0 ->
    verify_options(Options);
verify_options([{connect_timeout, infinity} | Options]) ->
    verify_options(Options);
verify_options([{connect_timeout, MS} | Options])
        when is_integer(MS), MS >= 0 ->
    verify_options(Options);
verify_options([{partial_upload, Bool} | Options])
        when is_boolean(Bool) ->
    verify_options(Options);
verify_options([{partial_upload, infinity} | Options])  ->
    verify_options(Options);
verify_options([{connect_options, List} | Options]) when is_list(List) ->
    verify_options(Options);
verify_options([{proxy, List} | Options]) when is_list(List) ->
    verify_options(Options);
verify_options([{proxy_ssl_options, List} | Options]) when is_list(List) ->
    verify_options(Options);
verify_options([Option | _Rest]) ->
    erlang:error({bad_option, Option});
verify_options([]) ->
    ok.
