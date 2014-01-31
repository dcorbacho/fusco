%%%=============================================================================
%%% @copyright (C) 1999-2013, Erlang Solutions Ltd
%%% @author Diana Corbacho <diana.corbacho@erlang-solutions.com>
%%% @doc
%%% @end
%%%=============================================================================
-module(fusco_tests_eqc).
-copyright("2013, Erlang Solutions Ltd.").

-include_lib("eqc/include/eqc.hrl").
-include("fusco.hrl").

-define(TWO_OK, <<"HTTP/1.1 200 OK\r\n\r\n">>).
-define(FOUR_BAD_REQUEST, <<"HTTP/1.1 400 Bad Request\r\n">>).
-define(TWO_OK(V), case V of
		       undefined ->
			   ?TWO_OK;
		       _ ->
			   <<"HTTP/1.1 200 OK\r\nConnection: ",
			     V/binary,"\r\n\r\n">>
		   end).

-export([prop_http_request_per_family/3,
         prop_persistent_connection_per_family/3,
         prop_reconnect_per_family/3,
         prop_client_close_connection_per_family/3,
         prop_connection_refused_per_family/3,
         prop_http_request_cookie_path/3]).

%%==============================================================================
%% Quickcheck generators
%%==============================================================================
valid_http_request() ->
    ?LET({RequestLine, Headers},
	 {http_eqc_gen:request_line(), http_eqc_gen:request_headers()},
	 ?LET(Body, http_eqc_encoding:body(any),
	      {RequestLine, http_eqc_encoding:add_content_length(Headers, Body),
	       Body})).

valid_http_response() ->
    ?LET({StatusLine, Headers},
         {status_line(), http_eqc_gen:headers()},
         ?LET(Body, http_eqc_encoding:body(StatusLine),
              {StatusLine, Headers, Body}
             )
        ).

status_line() ->
    %% Discard CONTINUE for cookie testing, client waits for next messages
    ?SUCHTHAT({_, S, _}, http_eqc_gen:status_line(),
              not lists:member(S, [<<"100">>, <<"101">>])).

token() ->
    non_empty(list(choose($A, $z))).

path() ->
    non_empty(list(token())).

subpath(Path, true) ->
    ?LET(Length, choose(1, length(Path)),
         begin
             {H, _} = lists:split(Length, Path),
             H
         end);
subpath(Path, false) ->
    ?SUCHTHAT(SubPath, path(), hd(SubPath) =/= hd(Path)).

set_cookie(Path) ->
    {<<"Set-Cookie">>, {http_eqc_gen:cookie_pair(),
                        [{<<"Path">>, encode_path(Path)}]}}.

path_cookie() ->
    ?LET({Path, IsSubPath}, {path(), bool()},
         ?LET(SubPath, subpath(Path, IsSubPath),
              ?LET(Cookie, set_cookie(SubPath),
                   {Cookie, encode_path(Path), IsSubPath}
                  )
             )
        ).

%%==============================================================================
%% Quickcheck properties
%%==============================================================================
prop_http_request_per_family(Host, Family, Ssl) ->
    eqc:numtests(
      500,
      ?FORALL({{Method, Uri, _Version}, Headers, Body} = Msg,
	      valid_http_request(),
	      begin
		  Module = select_module(Ssl),
		  {ok, Listener, LS, Port} =
		      webserver:start(Module, [validate_msg(Msg)], Family),
		  {ok, Client} = fusco:start({Host, Port, Ssl}, []),
		  {ok, {Status, _, _, _, _}}
		      = fusco:request(Client, Uri, Method, Headers, Body, 10000), 
		  ok = fusco:disconnect(Client),
		  webserver:stop(Module, Listener, LS),
		  Expected = {<<"200">>, <<"OK">>},
		  ?WHENFAIL(io:format("Status: ~p~nExpected: ~p~n",
				      [Status, Expected]),
			    case Status of
				Expected ->
				    true;
				_ ->
				    false
			    end)
	      end)).

prop_persistent_connection_per_family(Host, Family, Ssl) ->
    %% Fusco must keep the connection alive and be able to reconnect
    %% Individual properties defined for reconnect and keep-alive
    ?FORALL(
       Msgs,
       non_empty(list({valid_http_request(), http_eqc_gen:connection_header()})),
       begin
	   Module = select_module(Ssl),
	   {ok, Listener, LS, Port} =
	       webserver:start(Module,
			       [reply_msg(?TWO_OK(ConHeader)) || {_, ConHeader} <- Msgs],
			       Family),
	   {ok, Client} = fusco:start({Host, Port, Ssl}, []),
	   Replies = lists:map(fun({{{Method, Uri, _Version}, Headers, Body}, _}) ->
				       fusco:request(Client, Uri, Method, Headers, Body, 10000)
			       end, Msgs),
	   ok = fusco:disconnect(Client),
	   webserver:stop(Module, Listener, LS),
	   ?WHENFAIL(io:format("Replies: ~p~nExpected: 200 OK~n", [Replies]),
		     lists:all(fun({ok, {{<<"200">>, <<"OK">>}, _, _, _, _}}) ->
				       true;
				  (_) ->
				       false
			       end, Replies))
       end).

prop_reconnect_per_family(Host, Family, Ssl) ->
    %% Connection is always closed in the server and fusco must reconnect
    eqc:numtests(
      50,
      ?FORALL(
	 Msgs,
	 non_empty(list(valid_http_request())),
	 begin
	     Module = select_module(Ssl),
	     {ok, Listener, LS, Port} =
		 webserver:start(Module,
				 [reply_and_close_msg(?TWO_OK) || _ <- Msgs],
				 Family),
	     {ok, Client} = fusco:start({Host, Port, Ssl}, []),
	     Replies = lists:map(fun({{Method, Uri, _Version}, Headers, Body}) ->
					 Hdrs = lists:keydelete(<<"Connection">>, 1, Headers),
					 fusco:request(Client, Uri, Method, Hdrs, Body, 10000)
				 end, Msgs),
	     ok = fusco:disconnect(Client),
	     webserver:stop(Module, Listener, LS),
	     ?WHENFAIL(io:format("Replies: ~p~nExpected: 200 OK~n", [Replies]),
		       lists:all(fun({ok, {{<<"200">>, <<"OK">>}, _, _, _, _}}) ->
					 true;
				    (_) ->
					 false
				 end, Replies))
	 end)).

prop_client_close_connection_per_family(Host, Family, Ssl) ->
    %% Fusco must close the connection if requested by the server
    eqc:numtests(
      25,
      ?FORALL({{{Method, Uri, _Version}, Headers, Body}, Connection},
	      {valid_http_request(), http_eqc_gen:connection_header()},
	      begin
		  Id = erlang:now(),
		  Module = select_module(Ssl),
		  {ok, Listener, LS, Port} =
		      webserver:start(Module,
				      [reply_msg_and_check(Id, ?TWO_OK(Connection))],
				      Family),
		  {ok, Client} = fusco:start({Host, Port, Ssl}, []),
		  {ok, {Status, _, _, _, _}}
		      = fusco:request(Client, Uri, Method, Headers, Body, 10000), 
		  Closed = receive
			       {Id, closed} ->
				  true
			   after 1000 ->
				   false
			   end,
		  ok = fusco:disconnect(Client),
		  webserver:stop(Module, Listener, LS),
		  Expected = {<<"200">>, <<"OK">>},
		  MustClose = must_close(Headers, Connection),
		  ?WHENFAIL(io:format("Connection: ~p~nStatus: ~p~nExpected:"
				      " ~p~nMust close: ~p~nClosed: ~p~n",
				      [Connection, Status, Expected, MustClose, Closed]),
			    case Status of
				Expected ->
				    MustClose == Closed;
				_ ->
				    false
			    end)
	      end)).

prop_connection_refused_per_family(Host, Family, Ssl) ->
    eqc:numtests(1,
      begin
	  Module = select_module(Ssl),
	  {ok, Listener, LS, Port} =
	      webserver:start(Module, [reply_msg(<<>>)], Family),
	  webserver:stop(Module, Listener, LS),
	  {ok, Client} = fusco:start({Host, Port, Ssl}, []),
	  Reply = fusco:connect(Client),
	  Expected = {error, econnrefused},
	  ?WHENFAIL(io:format("Reply: ~p~nExpected: ~p~n",
			      [Reply, Expected]),
		    case Reply of
			Expected ->
			    true;
			_ ->
			    false
		    end)
      end).

prop_http_request_cookie_path(Host, Family, Ssl) ->
    ?FORALL(
       {Request, {Cookie, Path, IsSubPath},
        {{_, Status, Reason}, _, _} = Response},
       {valid_http_request(), path_cookie(), valid_http_response()},
       begin
           ResponseBin = build_response(Response, [Cookie]),
           ValidationFun = validate_cookie_path_msg(ResponseBin, Path,
                                                    IsSubPath, Cookie),
           {FirstResponse, SecondResponse} =
               send_cookie_requests(Host, Ssl, Family, ValidationFun, Path, Request),
           Expected = {Status, Reason},
           ?WHENFAIL(io:format("FirstResponse: ~p~nExpected:"
                               " ~p~nSecondResponse ~p~n",
                               [FirstResponse, Expected, SecondResponse]),
                     (FirstResponse == Expected)
                     and (SecondResponse == {<<"200">>, <<"OK">>})
                    )
       end
      ).

%%==============================================================================
%% Internal functions
%%==============================================================================
validate_msg({{_Method, _Uri, _Version}, SentHeaders, SentBody}) ->
    fun(Module, Socket, _Request, GotHeaders, GotBody) when SentBody == GotBody ->
	    case validate_headers(SentBody, GotHeaders, SentHeaders) of
		true ->
		    Module:send(Socket, ?TWO_OK);
		false ->
		    Module:send(Socket, ?FOUR_BAD_REQUEST)
	    end;
       (Module, Socket, _Request, _GotHeaders, _) ->
	    Module:send(Socket, ?FOUR_BAD_REQUEST) 
    end.

validate_cookie_path_msg(Response, Path, IsSubPath, Cookie) ->
    SPath = binary_to_list(Path),
    fun(Module, Socket, {http_request, _, "original", _}, _Headers, _Body) ->
            Module:send(Socket, Response);
       (Module, Socket, {http_request, _, {abs_path, RPath}, _}, Headers, _Body)
          when SPath == RPath ->
            case check_cookie_path(Headers, IsSubPath, Cookie) of
                true ->
                    Module:send(Socket, ?TWO_OK);
                false ->
                    Module:send(Socket, ?FOUR_BAD_REQUEST)
            end
    end.

check_cookie_path(Headers, true, {_, {{A, B}, _}}) ->
    binary_to_list(<<A/binary,"=",B/binary>>)
        == proplists:get_value("Cookie", Headers);
check_cookie_path(Headers, false, _) ->
    undefined == proplists:get_value("Cookie", Headers).

verify_host(GotHeaders, SentHeaders) ->
    %% Host must be added by the client if it is not part of the headers list
    %% http://tools.ietf.org/html/rfc2616#section-14.23
    Key = "host",
    case lists:keytake(Key, 1, GotHeaders) of
	{value, {_, Value}, NewGotHeaders} ->
	    case lists:keytake(Key, 1, SentHeaders) of
		%% The user sent the 'Host' header, value must match
		{value, {_, Value}, NewSentHeaders} ->
		    {NewGotHeaders, NewSentHeaders};
		false ->
		    {NewGotHeaders, SentHeaders};
		_  ->
		    false
	    end;
	false ->
	    false
    end.

verify_content_length(Body, GotHeaders, SentHeaders) ->
    %% Must be updated when the code supports transfer-encoding
    %% http://tools.ietf.org/html/rfc2616#section-14.13
    Key = "content-length",
    ContentLength = iolist_size(Body),
    {NewGotHeaders, GotContentLength}
	= case lists:keytake(Key, 1, GotHeaders) of
	      {value, {_, Value}, H} ->
		  {H, list_to_integer(Value)};
	      false ->
		  {GotHeaders, 0}
	  end,
    case ContentLength == GotContentLength of
	true ->
	    {NewGotHeaders, lists:keydelete(Key, 1, SentHeaders)};
	false ->
	    false
    end.

validate_headers(Body, GotHeaders, SentHeaders) ->
    CleanGotHeaders = lists:keysort(1, [{string:to_lower(K), V}
					|| {K, V} <- GotHeaders]),
    CleanSentHeaders = lists:keysort(1, [{string:to_lower(binary_to_list(K)),
					  binary_to_list(V)}
					 || {K, V} <- SentHeaders]),
    case verify_host(CleanGotHeaders, CleanSentHeaders) of
	false ->
	    false;
	{GotHeaders1, Headers1} ->
	    case verify_content_length(Body, GotHeaders1, Headers1) of
		false ->
		    false;
		{GotHeaders2, Headers2} ->
		    GotHeaders2 == Headers2
	    end
    end.

reply_msg(Msg) ->
    fun(Module, Socket, _Request, _Headers, _Body) ->
	    Module:send(Socket, Msg)
    end.

reply_msg_and_check(Id, Msg) ->
    Parent = self(),
    fun(Module, Socket, _Request, _Headers, _Body) ->
	    Module:send(Socket, Msg),
	    case Module:recv(Socket, 0) of
		{error, closed} ->
		    Parent ! {Id, closed};
		_ ->
		    ok
	    end
    end.

reply_and_close_msg(Msg) ->
    fun(Module, Socket, _Request, _Headers, _Body) ->
	    Module:send(Socket, Msg),
	    Module:close(Socket)
    end.

must_close(Headers, Connection) ->
    case proplists:get_value(<<"Connection">>, Headers) of
	<<"close">> ->
	    true;
	_ ->
	    case Connection of
		<<"close">> ->
		    true;
		_ ->
		    false
	    end
    end.

select_module(Ssl) ->
    case Ssl of
	true ->
	    ssl;
	false ->
	    gen_tcp
    end.

encode_path(Path) ->
    list_to_binary(["/", string:join(Path, "/")]).

build_response({StatusLine, Headers, Body}, Cookies) ->
    http_eqc_encoding:build_valid_response(
      StatusLine,
      http_eqc_encoding:add_content_length(Headers, Body),
      Cookies, Body).

send_cookie_requests(Host, Ssl, Family, ValidationFun, Path,
                     {{Method, _Uri, _Version}, Headers, Body}) ->
    Module = select_module(Ssl),
    {ok, Listener, LS, Port} =
        webserver:start(Module, [ValidationFun], Family),
    {ok, Client} = fusco:start({Host, Port, Ssl}, [{use_cookies, true}]),
    {ok, {FirstResponse, _, _, _, _}}
        = fusco:request(Client, <<"original">>, Method, Headers, Body, 10000), 
    {ok, {SecondResponse, _, _, _, _}}
        = fusco:request(Client, Path, Method, Headers, Body, 10000), 
    ok = fusco:disconnect(Client),
    webserver:stop(Module, Listener, LS),
    {FirstResponse, SecondResponse}.
