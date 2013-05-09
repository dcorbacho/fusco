%%%=============================================================================
%%% @copyright (C) 1999-2013, Erlang Solutions Ltd
%%% @author Diana Corbacho <diana.corbacho@erlang-solutions.com>
%%% @doc
%%% @end
%%%=============================================================================
-module(lhttpc_protocol).
-copyright("2013, Erlang Solutions Ltd.").

-record(state, {socket, ssl, version, status_code, reason, headers = []}).

-export([recv/2]).

%% TODO handle partial downloads

recv(Socket, Ssl) ->
    recv(Socket, Ssl, <<>>).

recv(Socket, Ssl, Buffer) ->
    case lhttpc_sock:recv(Socket, Ssl) of
	{ok, Data} ->
	    decode_status_line(<< Buffer/binary, Data/binary >>, #state{socket = Socket, ssl = Ssl});
	{error, Reason} ->
	    {error, Reason}
    end.

decode_status_line(<<"HTTP/1.0\s", Rest/bits>>, State) ->
    decode_status_code(Rest, State#state{version = {1,0}});
decode_status_line(<<"HTTP/1.1\s", Rest/bits>>, State) ->
    decode_status_code(Rest, State#state{version = {1,1}});
decode_status_line(_, _) ->
    {error, 400}.

decode_status_code(<<C1,C2,C3,$\s,Rest/bits>>, State)
  when C1 >= $1, C1 =< $5 ->
    decode_reason_phrase(Rest, <<>>, State#state{status_code = <<C1,C2,C3>>});
decode_status_code(_, _) ->
    {error, 400}.

decode_reason_phrase(<<"\n", Rest/bits>>, Acc, State) ->
    decode_header(Rest, <<>>, State#state{reason = Acc});
decode_reason_phrase(<<"\r\n", Rest/bits>>, Acc, State) ->
    decode_header(Rest, <<>>, State#state{reason = Acc});
decode_reason_phrase(<<C, Rest/bits>>, Acc, State) ->
    decode_reason_phrase(Rest, <<Acc/binary, C>>, State).

decode_header(<<>>, Acc, State) ->
    case lhttpc_sock:recv(State#state.socket, State#state.ssl) of
	{ok, Data} ->
	    decode_header(Data, Acc, State);
	{error, Reason} ->
	    {error, Reason}
    end;
decode_header(<<$\s, Rest/bits>>, Acc, State) ->
    decode_header(Rest, Acc, State);
decode_header(<<$:, Rest/bits>>, Header, State) ->
    decode_header_value(Rest, {Header, <<>>}, State);
decode_header(<<$\n, Rest/bits>>, <<>>, State) ->
    decode_body(Rest, State#state{headers = State#state.headers});
decode_header(<<$\r, $\n, Rest/bits>>, <<>>, State) ->
    decode_body(Rest, State#state{headers = State#state.headers});
decode_header(<<$\r, $\n, _Rest/bits>>, _, _State) ->
    {error, 400};
decode_header(<<$A, Rest/bits>>, Header, State) ->
    decode_header(Rest, <<Header/binary, $a>>, State);
decode_header(<<$B, Rest/bits>>, Header, State) ->
    decode_header(Rest, <<Header/binary, $b>>, State);
decode_header(<<$C, Rest/bits>>, Header, State) ->
    decode_header(Rest, <<Header/binary, $c>>, State);
decode_header(<<$D, Rest/bits>>, Header, State) ->
    decode_header(Rest, <<Header/binary, $d>>, State);
decode_header(<<$E, Rest/bits>>, Header, State) ->
    decode_header(Rest, <<Header/binary, $e>>, State);
decode_header(<<$F, Rest/bits>>, Header, State) ->
    decode_header(Rest, <<Header/binary, $f>>, State);
decode_header(<<$G, Rest/bits>>, Header, State) ->
    decode_header(Rest, <<Header/binary, $g>>, State);
decode_header(<<$H, Rest/bits>>, Header, State) ->
    decode_header(Rest, <<Header/binary, $h>>, State);
decode_header(<<$I, Rest/bits>>, Header, State) ->
    decode_header(Rest, <<Header/binary, $i>>, State);
decode_header(<<$J, Rest/bits>>, Header, State) ->
    decode_header(Rest, <<Header/binary, $j>>, State);
decode_header(<<$K, Rest/bits>>, Header, State) ->
    decode_header(Rest, <<Header/binary, $k>>, State);
decode_header(<<$L, Rest/bits>>, Header, State) ->
    decode_header(Rest, <<Header/binary, $l>>, State);
decode_header(<<$M, Rest/bits>>, Header, State) ->
    decode_header(Rest, <<Header/binary, $m>>, State);
decode_header(<<$N, Rest/bits>>, Header, State) ->
    decode_header(Rest, <<Header/binary, $n>>, State);
decode_header(<<$O, Rest/bits>>, Header, State) ->
    decode_header(Rest, <<Header/binary, $o>>, State);
decode_header(<<$P, Rest/bits>>, Header, State) ->
    decode_header(Rest, <<Header/binary, $p>>, State);
decode_header(<<$Q, Rest/bits>>, Header, State) ->
    decode_header(Rest, <<Header/binary, $q>>, State);
decode_header(<<$R, Rest/bits>>, Header, State) ->
    decode_header(Rest, <<Header/binary, $r>>, State);
decode_header(<<$S, Rest/bits>>, Header, State) ->
    decode_header(Rest, <<Header/binary, $s>>, State);
decode_header(<<$T, Rest/bits>>, Header, State) ->
    decode_header(Rest, <<Header/binary, $t>>, State);
decode_header(<<$U, Rest/bits>>, Header, State) ->
    decode_header(Rest, <<Header/binary, $u>>, State);
decode_header(<<$V, Rest/bits>>, Header, State) ->
    decode_header(Rest, <<Header/binary, $v>>, State);
decode_header(<<$W, Rest/bits>>, Header, State) ->
    decode_header(Rest, <<Header/binary, $w>>, State);
decode_header(<<$X, Rest/bits>>, Header, State) ->
    decode_header(Rest, <<Header/binary, $x>>, State);
decode_header(<<$Y, Rest/bits>>, Header, State) ->
    decode_header(Rest, <<Header/binary, $y>>, State);
decode_header(<<$Z, Rest/bits>>, Header, State) ->
    decode_header(Rest, <<Header/binary, $z>>, State);
decode_header(<<C, Rest/bits>>, Header, State) ->
    decode_header(Rest, <<Header/binary, C>>, State).

decode_header_value(<<>>, Acc, State) ->
    case lhttpc_sock:recv(State#state.socket, State#state.ssl) of
	{ok, Data} ->
	    decode_header_value(Data, Acc, State);
	{error, Reason} ->
	    {error, Reason}
    end;
decode_header_value(<<$\s, Rest/bits>>, Acc, State) ->
    decode_header_value(Rest, Acc, State);
decode_header_value(<<$\t, Rest/bits>>, Acc, State) ->
    decode_header_value(Rest, Acc, State);
decode_header_value(<<$\n, Rest/bits>>, Acc, State) ->
    decode_header(Rest, <<>>, State#state{headers = [Acc | State#state.headers]});
decode_header_value(<<$\r, $\n, Rest/bits>>, Acc, State) ->
    decode_header(Rest, <<>>, State#state{headers = [Acc | State#state.headers]});
decode_header_value(<<C, Rest/bits>>, {Header, Value}, State) ->
    decode_header_value(Rest, {Header, <<Value/binary, C>>}, State).

decode_body(Rest, State) ->
    {State#state.version, State#state.status_code, State#state.reason, State#state.headers, Rest}.

match_eol(<< $\n, _/bits >>) ->
    match;
match_eol(<< _, Rest/bits >>) ->
    match_eol(Rest);
match_eol(_) ->
    nomatch.
