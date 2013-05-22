%%%=============================================================================
%%% @copyright (C) 1999-2013, Erlang Solutions Ltd
%%% @author Diana Corbacho <diana.corbacho@erlang-solutions.com>
%%% @doc
%%% @end
%%%=============================================================================
-module(lhttpc_protocol).
-copyright("2013, Erlang Solutions Ltd.").

-include("lhttpc.hrl").

-record(state, {socket, ssl, version, status_code, reason, headers = [],
		connection, cookies = [], content_length = 0}).

%% API
-export([recv/2,
	 decode_cookie/1]).

%% TEST
-export([decode_header_value/5,
	 decode_header/3,
	 empty_state/0]).

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
    {error, status_line}.

decode_status_code(<<C1,C2,C3,$\s,Rest/bits>>, State) ->
    decode_reason_phrase(Rest, <<>>, State#state{status_code = <<C1,C2,C3>>});
decode_status_code(Bin, State) when byte_size(Bin) < 3 ->
    case lhttpc_sock:recv(State#state.socket, State#state.ssl) of
	{ok, Data} ->
	    decode_status_code(<<Bin/binary, Data/binary>>, State);
	{error, Reason} ->
	    {error, Reason}
    end;    
decode_status_code(_, _) ->
    {error, status_code}.

decode_reason_phrase(<<>>, Acc, State) ->
    case lhttpc_sock:recv(State#state.socket, State#state.ssl) of
	{ok, Data} ->
	    decode_reason_phrase(Data, Acc, State);
	{error, Reason} ->
	    {error, Reason}
    end;
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
    decode_header_value_ws(Rest, Header, State);
decode_header(<<$\n, Rest/bits>>, <<>>, State) ->
    decode_body(Rest, State);
decode_header(<<$\r,$\n>>, <<>>, State = #state{status_code = <<$1, _, _>>}) ->
    decode_body(<<>>, State);
decode_header(<<$\r,$\n, Rest/bits>>, <<>>,
	      State = #state{status_code = <<$1, _, _>>,
			     headers = []}) ->
    decode_status_line(Rest, #state{socket = State#state.socket,
				    ssl = State#state.ssl});
decode_header(<<$\r, $\n, Rest/bits>>, <<>>, State) ->
    decode_body(Rest, State);
decode_header(<<$\r, $\n, _Rest/bits>>, _, _State) ->
    {error, header};
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

decode_header_value_ws(<<$\s, Rest/bits>>, H, S) ->
    decode_header_value_ws(Rest, H, S);
decode_header_value_ws(<<$\t, Rest/bits>>, H, S) ->
    decode_header_value_ws(Rest, H, S);
decode_header_value_ws(Rest, <<"connection">> = H, S) ->
    decode_header_value_lc(Rest, H, <<>>, <<>>, S);
decode_header_value_ws(Rest, H, S) ->
    decode_header_value(Rest, H, <<>>, <<>>, S).

decode_header_value(<<>>, H, V, T, State) ->
    case lhttpc_sock:recv(State#state.socket, State#state.ssl) of
	{ok, Data} ->
	    decode_header_value(Data, H, V, T, State);
	{error, Reason} ->
	    {error, Reason}
    end;
decode_header_value(<<$\r>>, H, V, T, State) ->
    case lhttpc_sock:recv(State#state.socket, State#state.ssl) of
	{ok, Data} ->
	    decode_header_value(<<$\r, Data/binary>>, H, V, T, State);
	{error, Reason} ->
	    {error, Reason}
    end;
decode_header_value(<<$\n, Rest/bits>>, <<"content-length">> = H, V, _T, State) ->
    decode_header(Rest, <<>>, State#state{headers = [{H, V} | State#state.headers],
					  content_length = list_to_integer(binary_to_list(V))});
decode_header_value(<<$\n, Rest/bits>>, <<"set-cookie">> = H, V, _T, State) ->
    decode_header(Rest, <<>>, State#state{cookies = [decode_cookie(V)
						     | State#state.cookies],
					  headers = [{H, V} | State#state.headers]});
decode_header_value(<<$\n, Rest/bits>>, H, V, _T, State) ->
    decode_header(Rest, <<>>, State#state{headers = [{H, V} | State#state.headers]});
decode_header_value(<<$\r, $\n, Rest/bits>>, <<"set-cookie">> = H, V, _T, State) ->
    decode_header(Rest, <<>>, State#state{cookies = [decode_cookie(V)
						     | State#state.cookies],
					  headers = [{H, V} | State#state.headers]});
decode_header_value(<<$\r,$\n, Rest/bits>>, <<"content-length">> = H, V, _T, State) ->
    decode_header(Rest, <<>>, State#state{headers = [{H, V} | State#state.headers],
					  content_length = list_to_integer(binary_to_list(V))});
decode_header_value(<<$\r, $\n, Rest/bits>>, H, V, _T, State) ->
    decode_header(Rest, <<>>, State#state{headers = [{H, V} | State#state.headers]});
decode_header_value(<<$\s, Rest/bits>>, H, V, T, State) ->
    decode_header_value(Rest, H, V, <<T/binary, $\s>>, State);
decode_header_value(<<$\t, Rest/bits>>, H, V, T, State) ->
    decode_header_value(Rest, H, V, <<T/binary, $\t>>, State);
decode_header_value(<<C, Rest/bits>>, H, V, <<>>, State) ->
    decode_header_value(Rest, H, <<V/binary, C>>, <<>>, State);
decode_header_value(<<C, Rest/bits>>, H, V, T, State) ->
    decode_header_value(Rest, H, <<V/binary, T/binary, C>>, <<>>, State).

decode_header_value_lc(<<>>, H, V, T, State) ->
    case lhttpc_sock:recv(State#state.socket, State#state.ssl) of
	{ok, Data} ->
	    decode_header_value_lc(Data, H, V, T, State);
	{error, Reason} ->
	    {error, Reason}
    end;
decode_header_value_lc(<<$\n, Rest/bits>>, H, V, _T, State) ->
    decode_header(Rest, <<>>, State#state{headers = [{H, V} | State#state.headers],
					  connection = V});
decode_header_value_lc(<<$\r, $\n, Rest/bits>>, H, V, _T, State) ->
    decode_header(Rest, <<>>, State#state{headers = [{H, V} | State#state.headers],
					  connection = V});
decode_header_value_lc(<<$\s, Rest/bits>>, H, V, T, State) ->
    decode_header_value_lc(Rest, H, V, <<T/binary, $\s>>, State);
decode_header_value_lc(<<$\t, Rest/bits>>, H, V, T, State) ->
    decode_header_value_lc(Rest, H, V, <<T/binary, $\t>>, State);
decode_header_value_lc(<<$A, Rest/bits>>, H, V, T, State) ->
    decode_header_value_lc(Rest, H, <<V/binary, T/binary, $a>>, <<>>, State);
decode_header_value_lc(<<$B, Rest/bits>>, H, V, T, State) ->
    decode_header_value_lc(Rest, H, <<V/binary, T/binary, $b>>, <<>>, State);
decode_header_value_lc(<<$C, Rest/bits>>, H, V, T, State) ->
    decode_header_value_lc(Rest, H, <<V/binary, T/binary, $c>>, <<>>, State);
decode_header_value_lc(<<$D, Rest/bits>>, H, V, T, State) ->
    decode_header_value_lc(Rest, H, <<V/binary, T/binary, $d>>, <<>>, State);
decode_header_value_lc(<<$E, Rest/bits>>, H, V, T, State) ->
    decode_header_value_lc(Rest, H, <<V/binary, T/binary, $e>>, <<>>, State);
decode_header_value_lc(<<$F, Rest/bits>>, H, V, T, State) ->
    decode_header_value_lc(Rest, H, <<V/binary, T/binary, $f>>, <<>>, State);
decode_header_value_lc(<<$G, Rest/bits>>, H, V, T, State) ->
    decode_header_value_lc(Rest, H, <<V/binary, T/binary, $g>>, <<>>, State);
decode_header_value_lc(<<$H, Rest/bits>>, H, V, T, State) ->
    decode_header_value_lc(Rest, H, <<V/binary, T/binary, $h>>, <<>>, State);
decode_header_value_lc(<<$I, Rest/bits>>, H, V, T, State) ->
    decode_header_value_lc(Rest, H, <<V/binary, T/binary, $i>>, <<>>, State);
decode_header_value_lc(<<$J, Rest/bits>>, H, V, T, State) ->
    decode_header_value_lc(Rest, H, <<V/binary, T/binary, $j>>, <<>>, State);
decode_header_value_lc(<<$K, Rest/bits>>, H, V, T, State) ->
    decode_header_value_lc(Rest, H, <<V/binary, T/binary, $k>>, <<>>, State);
decode_header_value_lc(<<$L, Rest/bits>>, H, V, T, State) ->
    decode_header_value_lc(Rest, H, <<V/binary, T/binary, $l>>, <<>>, State);
decode_header_value_lc(<<$M, Rest/bits>>, H, V, T, State) ->
    decode_header_value_lc(Rest, H, <<V/binary, T/binary, $m>>, <<>>, State);
decode_header_value_lc(<<$N, Rest/bits>>, H, V, T, State) ->
    decode_header_value_lc(Rest, H, <<V/binary, T/binary, $n>>, <<>>, State);
decode_header_value_lc(<<$O, Rest/bits>>, H, V, T, State) ->
    decode_header_value_lc(Rest, H, <<V/binary, T/binary, $o>>, <<>>, State);
decode_header_value_lc(<<$P, Rest/bits>>, H, V, T, State) ->
    decode_header_value_lc(Rest, H, <<V/binary, T/binary, $p>>, <<>>, State);
decode_header_value_lc(<<$Q, Rest/bits>>, H, V, T, State) ->
    decode_header_value_lc(Rest, H, <<V/binary, T/binary, $q>>, <<>>, State);
decode_header_value_lc(<<$R, Rest/bits>>, H, V, T, State) ->
    decode_header_value_lc(Rest, H, <<V/binary, T/binary, $r>>, <<>>, State);
decode_header_value_lc(<<$S, Rest/bits>>, H, V, T, State) ->
    decode_header_value_lc(Rest, H, <<V/binary, T/binary, $s>>, <<>>, State);
decode_header_value_lc(<<$T, Rest/bits>>, H, V, T, State) ->
    decode_header_value_lc(Rest, H, <<V/binary, T/binary, $t>>, <<>>, State);
decode_header_value_lc(<<$U, Rest/bits>>, H, V, T, State) ->
    decode_header_value_lc(Rest, H, <<V/binary, T/binary, $u>>, <<>>, State);
decode_header_value_lc(<<$V, Rest/bits>>, H, V, T, State) ->
    decode_header_value_lc(Rest, H, <<V/binary, T/binary, $v>>, <<>>, State);
decode_header_value_lc(<<$W, Rest/bits>>, H, V, T, State) ->
    decode_header_value_lc(Rest, H, <<V/binary, T/binary, $w>>, <<>>, State);
decode_header_value_lc(<<$X, Rest/bits>>, H, V, T, State) ->
    decode_header_value_lc(Rest, H, <<V/binary, T/binary, $x>>, <<>>, State);
decode_header_value_lc(<<$Y, Rest/bits>>, H, V, T, State) ->
    decode_header_value_lc(Rest, H, <<V/binary, T/binary, $y>>, <<>>, State);
decode_header_value_lc(<<$Z, Rest/bits>>, H, V, T, State) ->
    decode_header_value_lc(Rest, H, <<V/binary, T/binary, $z>>, <<>>, State);
decode_header_value_lc(<<C, Rest/bits>>, H, V, T, State) ->
    decode_header_value_lc(Rest, H, <<V/binary, T/binary, C>>, <<>>, State).

%% RFC 6265
%% TODO decode cookie values, this only accepts 'a=b'
decode_cookie(Cookie) ->
    decode_cookie_name(Cookie, <<>>).

decode_cookie_name(<<$\s, Rest/bits>>, N) ->
    decode_cookie_name(Rest, N);
decode_cookie_name(<<$\t, Rest/bits>>, N) ->
    decode_cookie_name(Rest, N);
decode_cookie_name(<<$=, Rest/bits>>, N) ->
    decode_cookie_value(Rest, N, <<>>);
decode_cookie_name(<<C, Rest/bits>>, N) ->
    decode_cookie_name(Rest, <<N/binary, C>>).

decode_cookie_value(<<$\s, Rest/bits>>, N, V) ->
    decode_cookie_value(Rest, N, V);
decode_cookie_value(<<$\t, Rest/bits>>, N, V) ->
    decode_cookie_value(Rest, N, V);
decode_cookie_value(<<$;, Rest/bits>>, N, V) ->
    decode_cookie_av_ws(Rest, #lhttpc_cookie{name = N, value = V});
decode_cookie_value(<<C, Rest/bits>>, N, V) ->
    decode_cookie_value(Rest, N, <<V/binary, C>>);
decode_cookie_value(<<>>, N, V) ->
    #lhttpc_cookie{name = N, value = V}.

decode_cookie_av_ws(<<$\s, Rest/bits>>, C) ->
    decode_cookie_av_ws(Rest, C);
decode_cookie_av_ws(<<$\t, Rest/bits>>, C) ->
    decode_cookie_av_ws(Rest, C);
%% We are only interested on Expires, Max-Age, Path
decode_cookie_av_ws(<<$e, Rest/bits>>, C) ->
    decode_cookie_av(Rest, C, <<$e>>);
decode_cookie_av_ws(<<$E, Rest/bits>>, C) ->
    decode_cookie_av(Rest, C, <<$e>>);
decode_cookie_av_ws(<<$m, Rest/bits>>, C) ->
    decode_cookie_av(Rest, C, <<$m>>);
decode_cookie_av_ws(<<$M, Rest/bits>>, C) ->
    decode_cookie_av(Rest, C, <<$m>>);
decode_cookie_av_ws(<<$p, Rest/bits>>, C) ->
    decode_cookie_av(Rest, C, <<$p>>);
decode_cookie_av_ws(<<$P, Rest/bits>>, C) ->
    decode_cookie_av(Rest, C, <<$p>>);
decode_cookie_av_ws(Rest, C) ->
    ignore_cookie_av(Rest, C).

ignore_cookie_av(<<$;, Rest/bits>>, Co) ->
    decode_cookie_av_ws(Rest, Co);
ignore_cookie_av(<<_, Rest/bits>>, Co) ->
    ignore_cookie_av(Rest, Co);
ignore_cookie_av(<<>>, Co) ->
    Co.

%% Match only uppercase chars on Expires, Max-Age, Path
decode_cookie_av(<<$=, Rest/bits>>, Co, AV) ->
    decode_cookie_av_value(Rest, Co, AV, <<>>);
decode_cookie_av(<<$E, Rest/bits>>, Co, AV) ->
    decode_cookie_av(Rest, Co, <<AV/binary, $e>>);
decode_cookie_av(<<$X, Rest/bits>>, Co, AV) ->
    decode_cookie_av(Rest, Co, <<AV/binary, $x>>);
decode_cookie_av(<<$P, Rest/bits>>, Co, AV) ->
    decode_cookie_av(Rest, Co, <<AV/binary, $p>>);
decode_cookie_av(<<$I, Rest/bits>>, Co, AV) ->
    decode_cookie_av(Rest, Co, <<AV/binary, $i>>);
decode_cookie_av(<<$R, Rest/bits>>, Co, AV) ->
    decode_cookie_av(Rest, Co, <<AV/binary, $r>>);
decode_cookie_av(<<$S, Rest/bits>>, Co, AV) ->
    decode_cookie_av(Rest, Co, <<AV/binary, $s>>);
decode_cookie_av(<<$M, Rest/bits>>, Co, AV) ->
    decode_cookie_av(Rest, Co, <<AV/binary, $m>>);
decode_cookie_av(<<$A, Rest/bits>>, Co, AV) ->
    decode_cookie_av(Rest, Co, <<AV/binary, $a>>);
decode_cookie_av(<<$G, Rest/bits>>, Co, AV) ->
    decode_cookie_av(Rest, Co, <<AV/binary, $g>>);
decode_cookie_av(<<$T, Rest/bits>>, Co, AV) ->
    decode_cookie_av(Rest, Co, <<AV/binary, $t>>);
decode_cookie_av(<<$H, Rest/bits>>, Co, AV) ->
    decode_cookie_av(Rest, Co, <<AV/binary, $h>>);
decode_cookie_av(<<$;, Rest/bits>>, Co, _AV) ->
    decode_cookie_av_ws(Rest, Co);
decode_cookie_av(<<C, Rest/bits>>, Co, AV) ->
    decode_cookie_av(Rest, Co, <<AV/binary, C>>);
decode_cookie_av(<<>>, Co, _AV) ->
    ignore_cookie_av(<<>>, Co).

decode_cookie_av_value(<<>>, Co, <<"path">>, Value) ->
    Co#lhttpc_cookie{path = Value};
decode_cookie_av_value(<<>>, Co, <<"max-age">>, Value) ->
    Co#lhttpc_cookie{max_age = max_age(Value),
		     timestamp = calendar:universal_time()};
decode_cookie_av_value(<<>>, Co, <<"expires">>, Value) ->
    Co#lhttpc_cookie{expires = expires(Value)};
decode_cookie_av_value(<<$;, Rest/bits>>, Co, <<"path">>, Value) ->
    decode_cookie_av_ws(Rest, Co#lhttpc_cookie{path = Value});
decode_cookie_av_value(<<$;, Rest/bits>>, Co, <<"max-age">>, Value) ->
    decode_cookie_av_ws(Rest, Co#lhttpc_cookie{
				max_age = max_age(Value),
				timestamp = calendar:universal_time()});
decode_cookie_av_value(<<$;, Rest/bits>>, Co, <<"expires">>, Value) ->
    %% TODO parse expires
    decode_cookie_av_ws(Rest, Co#lhttpc_cookie{expires = expires(Value)});
decode_cookie_av_value(<<$;, Rest/bits>>, Co, _, _) ->
    decode_cookie_av_ws(Rest, Co);
decode_cookie_av_value(<<C, Rest/bits>>, Co, AV, Value) ->
    decode_cookie_av_value(Rest, Co, AV, <<Value/binary, C>>).

decode_body(Rest, State) ->
    case byte_size(Rest) >= State#state.content_length of
	true ->
	    {State#state.version, State#state.status_code, State#state.reason, State#state.cookies, State#state.headers, State#state.connection, Rest};
	false ->
	    case lhttpc_sock:recv(State#state.socket, State#state.ssl) of
		{ok, Data} ->
		    decode_body(<<Rest/binary, Data/binary>>, State);
		{error, Reason} ->
		    {error, Reason}
	    end
    end.

max_age(Value) ->
    list_to_integer(binary_to_list(Value)) * 1000000.

%% http://tools.ietf.org/html/rfc2616#section-3.3.1
expires(<<_,_,_,$,,$\s,D1,D2,$\s,M1,M2,M3,$\s,Y1,Y2,Y3,Y4,$\s,Rest/bits>>) ->
    expires(Rest, {list_to_integer([Y1,Y2,Y3,Y4]),month(<<M1,M2,M3>>),list_to_integer([D1,D2])});
expires(<<_,_,_,$\s,Mo1,Mo2,Mo3,$\s,D1,D2,$\s,H1,H2,$:,M1,M2,$:,S1,S2,$\s,Y1,Y2,Y3,Y4,_Rest/bits>>) ->
    {{list_to_integer([Y1,Y2,Y3,Y4]),month(<<Mo1,Mo2,Mo3>>),list_to_integer([D1,D2])},
     {list_to_integer([H1,H2]), list_to_integer([M1,M2]), list_to_integer([S1,S2])}};
expires(<<"Monday",$,,$\s,Rest/bits>>) ->
    expires(Rest);
expires(<<"Tuesday",$,,$\s,Rest/bits>>) ->
    expires(Rest);
expires(<<"Wednesday",$,,$\s,Rest/bits>>) ->
    expires(Rest);
expires(<<"Thursday",$,,$\s,Rest/bits>>) ->
    expires(Rest);
expires(<<"Friday",$,,$\s,Rest/bits>>) ->
    expires(Rest);
expires(<<"Saturday",$,,$\s,Rest/bits>>) ->
    expires(Rest);
expires(<<"Sunday",$,,$\s,Rest/bits>>) ->
    expires(Rest);
expires(<<D1,D2,$\-,M1,M2,M3,$\-,Y3,Y4,$\s,Rest/bits>>) ->
    expires(Rest, {list_to_integer([$2,$0,Y3,Y4]),month(<<M1,M2,M3>>),list_to_integer([D1,D2])}).

expires(<<H1,H2,$:,M1,M2,$:,S1,S2,_Rest/bits>>, Date) ->
    {Date, {list_to_integer([H1,H2]), list_to_integer([M1,M2]), list_to_integer([S1,S2])}}.

month(<<$J,$a,$n>>) ->
    1;
month(<<$F,$e,$b>>) ->
    2;
month(<<$M,$a,$r>>) ->
    3;
month(<<$A,$p,$r>>) ->
    4;
month(<<$M,$a,$y>>) ->
    5;
month(<<$J,$u,$n>>) ->
    6;
month(<<$J,$u,$l>>) ->
    7;
month(<<$A,$u,$g>>) ->
    8;
month(<<$S,$e,$p>>) ->
    9;
month(<<$O,$c,$t>>) ->
    10;
month(<<$N,$o,$v>>) ->
    11;
month(<<$D,$e,$c>>) ->
    12.

empty_state() ->
    #state{}.
