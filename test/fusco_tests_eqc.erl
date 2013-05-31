%%%=============================================================================
%%% @copyright (C) 1999-2013, Erlang Solutions Ltd
%%% @author Diana Corbacho <diana.corbacho@erlang-solutions.com>
%%% @doc
%%% @end
%%%=============================================================================
-module(fusco_tests_eqc).

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

-export([prop_http_request_ipv4/0, prop_http_request_ipv6/0,
	 prop_http_request_ipv4_ssl/0, prop_http_request_ipv6_ssl/0]).
-export([prop_persistent_connection_ipv4/0, prop_persistent_connection_ipv6/0,
	 prop_persistent_connection_ipv4_ssl/0, prop_persistent_connection_ipv6_ssl/0]).
-export([prop_reconnect_ipv4/0, prop_reconnect_ipv6/0]).
-export([prop_client_close_connection_ipv4/0, prop_client_close_connection_ipv6/0]).

%%==============================================================================
%% Quickcheck generators
%%==============================================================================
valid_http_request() ->
    ?LET({RequestLine, Headers},
	 {http_eqc_gen:request_line(), http_eqc_gen:request_headers()},
	 ?LET(Body, http_eqc_encoding:body(any),
	      {RequestLine, http_eqc_encoding:add_content_length(Headers, Body),
	       Body})).

%%==============================================================================
%% Quickcheck properties
%%==============================================================================
prop_http_request_ipv4() ->
    prop_http_request_per_family("127.0.0.1", inet, false).

prop_http_request_ipv6() ->
    prop_http_request_per_family("::1", inet6, false).

prop_http_request_ipv4_ssl() ->
    %% TODO Use CT for setup/cleanup
    application:start(crypto),
    application:start(public_key),
    application:start(ssl),
    application:start(fusco),
    prop_http_request_per_family("127.0.0.1", inet, true).

prop_http_request_ipv6_ssl() ->
    %% TODO Use CT for setup/cleanup
    application:start(crypto),
    application:start(public_key),
    application:start(ssl),
    application:start(fusco),
    prop_http_request_per_family("::1", inet6, true).

prop_http_request_per_family(Host, Family, Ssl) ->
    eqc:numtests(
      500,
      ?FORALL({{Method, Uri, _Version}, Headers, Body} = Msg,
	      valid_http_request(),
	      begin
		  Module = select_module(Ssl),
		  {ok, Listener, LS, Port} =
		      webserver:start(Module, [validate_msg(Msg)], Family),
		  {ok, Client} = fusco:connect({Host, Port, Ssl}, []),
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

prop_persistent_connection_ipv4() ->
    prop_persistent_connection_per_family("127.0.0.1", inet, false).

prop_persistent_connection_ipv6() ->
    prop_persistent_connection_per_family("::1", inet6, false).

prop_persistent_connection_ipv4_ssl() ->
    prop_persistent_connection_per_family("127.0.0.1", inet, true).

prop_persistent_connection_ipv6_ssl() ->
    prop_persistent_connection_per_family("::1", inet6, true).

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
	   {ok, Client} = fusco:connect({Host, Port, Ssl}, []),
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

prop_reconnect_ipv4() ->
    prop_reconnect_per_family("127.0.0.1", inet).

prop_reconnect_ipv6() ->
    prop_reconnect_per_family("::1", inet6).

prop_reconnect_per_family(Host, Family) ->
    %% Connection is always closed in the server and fusco must reconnect
    eqc:numtests(
      50,
      ?FORALL(
	 Msgs,
	 non_empty(list(valid_http_request())),
	 begin
	     {ok, Listener, LS, Port} =
		 webserver:start(gen_tcp,
				 [reply_and_close_msg(?TWO_OK) || _ <- Msgs],
				 Family),
	     {ok, Client} = fusco:connect({Host, Port, false}, []),
	     Replies = lists:map(fun({{Method, Uri, _Version}, Headers, Body}) ->
					 Hdrs = lists:keydelete(<<"Connection">>, 1, Headers),
					 fusco:request(Client, Uri, Method, Hdrs, Body, 10000)
				 end, Msgs),
	     ok = fusco:disconnect(Client),
	     webserver:stop(Listener, LS),
	     ?WHENFAIL(io:format("Replies: ~p~nExpected: 200 OK~n", [Replies]),
		       lists:all(fun({ok, {{<<"200">>, <<"OK">>}, _, _, _, _}}) ->
					 true;
				    (_) ->
					 false
				 end, Replies))
	 end)).

prop_client_close_connection_ipv4() ->
    prop_client_close_connection_per_family("127.0.0.1", inet).

prop_client_close_connection_ipv6() ->
    prop_client_close_connection_per_family("::1", inet6).

prop_client_close_connection_per_family(Host, Family) ->
    %% Fusco must close the connection if requested by the server
    eqc:numtests(
      25,
      ?FORALL({{{Method, Uri, _Version}, Headers, Body}, Connection},
	      {valid_http_request(), http_eqc_gen:connection_header()},
	      begin
		  Id = erlang:now(),
		  {ok, Listener, LS, Port} =
		      webserver:start(gen_tcp,
				      [reply_msg_and_check(Id, ?TWO_OK(Connection))],
				      Family),
		  {ok, Client} = fusco:connect({Host, Port, false}, []),
		  {ok, {Status, _, _, _, _}}
		      = fusco:request(Client, Uri, Method, Headers, Body, 10000), 
		  Closed = receive
			       {Id, closed} ->
				  true
			   after 1000 ->
				   false
			   end,
		  ok = fusco:disconnect(Client),
		  webserver:stop(Listener, LS),
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

%%==============================================================================
%% Internal functions
%%==============================================================================
validate_msg({{_Method, _Uri, _Version}, SentHeaders, SentBody}) ->
    fun(Module, Socket, _Request, GotHeaders, GotBody) when SentBody == GotBody ->
	    case validate_headers(SentBody, GotHeaders, SentHeaders) of
		true ->
		    Module:send(Socket, ?TWO_OK);
		false ->
		    io:format(user, "~nHeaders expected ~p~n Headers got ~p~n",
			      [SentHeaders, GotHeaders]),
		    Module:send(Socket, ?FOUR_BAD_REQUEST)
	    end;
       (Module, Socket, _Request, _GotHeaders, _) ->
	    Module:send(Socket, ?FOUR_BAD_REQUEST) 
    end.

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
