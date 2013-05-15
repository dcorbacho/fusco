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
		?WHENFAIL(io:format("Response ~p ~s~n", [Recv, binary:list_to_bin(Msg)]),
			  case Recv of
			      {_, _, _, _, _, _} ->
				  true;
			      _ ->
				  false
			  end)
	    end).

build_valid_message({HttpVersion, StatusCode, Reason}, Headers, Body) ->
    SL = [HttpVersion, sp(), StatusCode, sp(), Reason, crlf()],
    HS = [[Name, colon(), Value, crlf()] || {Name, Value} <- Headers],
    [SL, HS, crlf(), Body]. 

colon() ->
    <<$:>>.

sp() ->
    <<$\s>>.

crlf() ->
    <<$\r,$\n>>.
