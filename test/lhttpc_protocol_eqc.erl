%%%=============================================================================
%%% @copyright (C) 1999-2013, Erlang Solutions Ltd
%%% @author Diana Corbacho <diana.corbacho@erlang-solutions.com>
%%% @doc
%%% @end
%%%=============================================================================
-module(lhttpc_protocol_eqc).

-include_lib("eqc/include/eqc.hrl").

-export([prop_http_response/0]).

prop_http_response() ->
    ?FORALL({StatusLine, Headers, Body},
	    {http_eqc_gen:status_line(), http_eqc_gen:headers(), binary()},
	    begin
		Msg = build_valid_message(StatusLine, Headers, Body),
		Socket = test_utils:start_listener(Msg),
		test_utils:send_message(Socket),
		Recv = lhttpc_protocol:recv(Socket, false),
		Expected = expected_output(StatusLine, Headers, Body),
		?WHENFAIL(io:format("Message:~n=======~n~s~n=======~nResponse:"
				    " ~p~nExpected: ~p~n",
				    [binary:list_to_bin(Msg), Recv, Expected]),
			  case Recv of
			      Expected ->
				  true;
			      _ ->
				  false
			  end)
	    end).

build_valid_message({HttpVersion, StatusCode, Reason}, Headers, Body) ->
    SL = [HttpVersion, sp(), StatusCode, sp(), Reason, crlf()],
    HS = [[Name, colon(), Value, crlf()] || {Name, Value} <- Headers],
    [SL, HS, crlf(), Body]. 

expected_output({HttpVersion, StatusCode, Reason}, Headers, Body) ->
    Version = http_version(HttpVersion),
    LowerHeaders = lists:reverse(headers_to_lower(Headers)),
    {Version, StatusCode, Reason, [], LowerHeaders, Body}.

colon() ->
    <<$:>>.

sp() ->
    <<$\s>>.

crlf() ->
    <<$\r,$\n>>.

http_version(<<"HTTP/1.1">>) ->
    {1, 1};
http_version(<<"HTTP/1.0">>) ->
    {1, 0}.

headers_to_lower(Headers) ->
    [{to_lower(H), V} || {H, V} <- Headers].

to_lower(Bin) ->
    list_to_binary(string:to_lower(binary_to_list(Bin))).
