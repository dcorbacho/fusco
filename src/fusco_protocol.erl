%%%=============================================================================
%%% @copyright (C) 2013, Erlang Solutions Ltd
%%% @author Diana Corbacho <diana.corbacho@erlang-solutions.com>
%%% @doc
%%%
%%% @end
%%%=============================================================================
-module(fusco_protocol).
-copyright("2013, Erlang Solutions Ltd.").

-include("fusco.hrl").

-define(SIZE(Data, Response), Response#response{size = Response#response.size + byte_size(Data)}).
-define(RECEPTION(Data, Response), Response#response{size = byte_size(Data),
						     in_timestamp = os:timestamp()}).
-define(TOUT, 1000).
%% Latency is here defined as the time from the start of packet transmission to the start of packet reception

%% API
-export([recv/2,
	 decode_cookie/1]).

%% TEST
-export([decode_header_value/5,
	 decode_header/3]).

%% TODO handle partial downloads

recv(Socket, Ssl) ->
    case fusco_sock:recv(Socket, Ssl) of
	{ok, Data} ->
	    decode_status_line(<< Data/binary >>,
			       ?RECEPTION(Data, #response{socket = Socket, ssl = Ssl}));
	{error, Reason} ->
	    {error, Reason}
    end.

decode_status_line(<<"HTTP/1.0\s",C1,C2,C3,$\s,Rest/bits>>, Response) ->
    decode_reason_phrase(Rest, <<>>, Response#response{version = {1,0},
						       status_code = <<C1,C2,C3>>});
decode_status_line(<<"HTTP/1.1\s",C1,C2,C3,$\s,Rest/bits>>, Response) ->
    decode_reason_phrase(Rest, <<>>, Response#response{version = {1,1},
						       status_code = <<C1,C2,C3>>});
decode_status_line(Bin, Response = #response{size = Size}) when Size < 10 ->
    case fusco_sock:recv(Response#response.socket, Response#response.ssl) of
	{ok, Data} ->
	    decode_status_line(<<Bin/binary, Data/binary>>, ?SIZE(Data, Response));
	{error, Reason} ->
	    {error, Reason}
    end;    
decode_status_line(_, _) ->
    {error, status_line}.

decode_reason_phrase(<<>>, Acc, Response) ->
    case fusco_sock:recv(Response#response.socket, Response#response.ssl) of
	{ok, Data} ->
	    decode_reason_phrase(Data, Acc, ?SIZE(Data, Response));
	{error, Reason} ->
	    {error, Reason}
    end;
decode_reason_phrase(<<$\r>>, Acc, Response) ->
    case fusco_sock:recv(Response#response.socket, Response#response.ssl) of
	{ok, Data} ->
	    decode_reason_phrase(<<$\r, Data/binary>>, Acc, ?SIZE(Data, Response));
	{error, Reason} ->
	    {error, Reason}
    end;
decode_reason_phrase(<<$\n, Rest/bits>>, Acc, Response) ->
    decode_header(Rest, <<>>, Response#response{reason = Acc});
decode_reason_phrase(<<$\r,$\n, Rest/bits>>, Acc, Response) ->
    decode_header(Rest, <<>>, Response#response{reason = Acc});
decode_reason_phrase(<<C, Rest/bits>>, Acc, Response) ->
    decode_reason_phrase(Rest, <<Acc/binary, C>>, Response).

decode_header(<<>>, Acc, Response) ->
    case fusco_sock:recv(Response#response.socket, Response#response.ssl) of
	{ok, Data} ->
	    decode_header(Data, Acc, ?SIZE(Data, Response));
	{error, closed} ->
	    case Acc of
		<<>> ->
		    decode_body(<<>>, Response);
		_ ->
		    {error, closed}
	    end;
	{error, Reason} ->
	    {error, Reason}
    end;
decode_header(<<$\r>>, Acc, Response) ->
    case fusco_sock:recv(Response#response.socket, Response#response.ssl) of
	{ok, Data} ->
	    decode_header(<<$\r, Data/binary>>, Acc, ?SIZE(Data, Response));
	{error, Reason} ->
	    {error, Reason}
    end;
decode_header(<<$\s, Rest/bits>>, Acc, Response) ->
    decode_header(Rest, Acc, Response);
decode_header(<<$:, Rest/bits>>, Header, Response) ->
    decode_header_value_ws(Rest, Header, Response);
decode_header(<<$\n, Rest/bits>>, <<>>, Response) ->
    decode_body(Rest, Response);
decode_header(<<$\r, $\n, Rest/bits>>, <<>>, Response) ->
    decode_body(Rest, Response);
decode_header(<<$\r, $\n, _Rest/bits>>, _, _Response) ->
    {error, header};
decode_header(<<$A, Rest/bits>>, Header, Response) ->
    decode_header(Rest, <<Header/binary, $a>>, Response);
decode_header(<<$B, Rest/bits>>, Header, Response) ->
    decode_header(Rest, <<Header/binary, $b>>, Response);
decode_header(<<$C, Rest/bits>>, Header, Response) ->
    decode_header(Rest, <<Header/binary, $c>>, Response);
decode_header(<<$D, Rest/bits>>, Header, Response) ->
    decode_header(Rest, <<Header/binary, $d>>, Response);
decode_header(<<$E, Rest/bits>>, Header, Response) ->
    decode_header(Rest, <<Header/binary, $e>>, Response);
decode_header(<<$F, Rest/bits>>, Header, Response) ->
    decode_header(Rest, <<Header/binary, $f>>, Response);
decode_header(<<$G, Rest/bits>>, Header, Response) ->
    decode_header(Rest, <<Header/binary, $g>>, Response);
decode_header(<<$H, Rest/bits>>, Header, Response) ->
    decode_header(Rest, <<Header/binary, $h>>, Response);
decode_header(<<$I, Rest/bits>>, Header, Response) ->
    decode_header(Rest, <<Header/binary, $i>>, Response);
decode_header(<<$J, Rest/bits>>, Header, Response) ->
    decode_header(Rest, <<Header/binary, $j>>, Response);
decode_header(<<$K, Rest/bits>>, Header, Response) ->
    decode_header(Rest, <<Header/binary, $k>>, Response);
decode_header(<<$L, Rest/bits>>, Header, Response) ->
    decode_header(Rest, <<Header/binary, $l>>, Response);
decode_header(<<$M, Rest/bits>>, Header, Response) ->
    decode_header(Rest, <<Header/binary, $m>>, Response);
decode_header(<<$N, Rest/bits>>, Header, Response) ->
    decode_header(Rest, <<Header/binary, $n>>, Response);
decode_header(<<$O, Rest/bits>>, Header, Response) ->
    decode_header(Rest, <<Header/binary, $o>>, Response);
decode_header(<<$P, Rest/bits>>, Header, Response) ->
    decode_header(Rest, <<Header/binary, $p>>, Response);
decode_header(<<$Q, Rest/bits>>, Header, Response) ->
    decode_header(Rest, <<Header/binary, $q>>, Response);
decode_header(<<$R, Rest/bits>>, Header, Response) ->
    decode_header(Rest, <<Header/binary, $r>>, Response);
decode_header(<<$S, Rest/bits>>, Header, Response) ->
    decode_header(Rest, <<Header/binary, $s>>, Response);
decode_header(<<$T, Rest/bits>>, Header, Response) ->
    decode_header(Rest, <<Header/binary, $t>>, Response);
decode_header(<<$U, Rest/bits>>, Header, Response) ->
    decode_header(Rest, <<Header/binary, $u>>, Response);
decode_header(<<$V, Rest/bits>>, Header, Response) ->
    decode_header(Rest, <<Header/binary, $v>>, Response);
decode_header(<<$W, Rest/bits>>, Header, Response) ->
    decode_header(Rest, <<Header/binary, $w>>, Response);
decode_header(<<$X, Rest/bits>>, Header, Response) ->
    decode_header(Rest, <<Header/binary, $x>>, Response);
decode_header(<<$Y, Rest/bits>>, Header, Response) ->
    decode_header(Rest, <<Header/binary, $y>>, Response);
decode_header(<<$Z, Rest/bits>>, Header, Response) ->
    decode_header(Rest, <<Header/binary, $z>>, Response);
decode_header(<<C, Rest/bits>>, Header, Response) ->
    decode_header(Rest, <<Header/binary, C>>, Response).

decode_header_value_ws(<<$\s, Rest/bits>>, H, S) ->
    decode_header_value_ws(Rest, H, S);
decode_header_value_ws(<<$\t, Rest/bits>>, H, S) ->
    decode_header_value_ws(Rest, H, S);
decode_header_value_ws(Rest, <<"connection">> = H, S) ->
    decode_header_value_lc(Rest, H, <<>>, <<>>, S);
decode_header_value_ws(Rest, <<"transfer-encoding">> = H, S) ->
    decode_header_value_lc(Rest, H, <<>>, <<>>, S);
decode_header_value_ws(Rest, H, S) ->
    decode_header_value(Rest, H, <<>>, <<>>, S).

decode_header_value(<<>>, H, V, T, Response) ->
    case fusco_sock:recv(Response#response.socket, Response#response.ssl) of
	{ok, Data} ->
	    decode_header_value(Data, H, V, T, ?SIZE(Data, Response));
	{error, Reason} ->
	    {error, Reason}
    end;
decode_header_value(<<$\r>>, H, V, T, Response) ->
    case fusco_sock:recv(Response#response.socket, Response#response.ssl) of
	{ok, Data} ->
	    decode_header_value(<<$\r, Data/binary>>, H, V, T, ?SIZE(Data, Response));
	{error, Reason} ->
	    {error, Reason}
    end;
decode_header_value(<<$\n, Rest/bits>>, <<"content-length">> = H, V, _T, Response) ->
    decode_header(Rest, <<>>, Response#response{headers = [{H, V} | Response#response.headers],
						content_length = binary_to_integer(V)});
decode_header_value(<<$\n, Rest/bits>>, <<"set-cookie">> = H, V, _T, Response) ->
    decode_header(Rest, <<>>, Response#response{cookies = [decode_cookie(V)
							   | Response#response.cookies],
					  headers = [{H, V} | Response#response.headers]});
decode_header_value(<<$\n, Rest/bits>>, H, V, _T, Response) ->
    decode_header(Rest, <<>>, Response#response{headers = [{H, V} | Response#response.headers]});
decode_header_value(<<$\r, $\n, Rest/bits>>, <<"set-cookie">> = H, V, _T, Response) ->
    decode_header(Rest, <<>>, Response#response{cookies = [decode_cookie(V)
						     | Response#response.cookies],
					  headers = [{H, V} | Response#response.headers]});
decode_header_value(<<$\r,$\n, Rest/bits>>, <<"content-length">> = H, V, _T, Response) ->
    decode_header(Rest, <<>>, Response#response{headers = [{H, V} | Response#response.headers],
					  content_length = binary_to_integer(V)});
decode_header_value(<<$\r, $\n, Rest/bits>>, H, V, _T, Response) ->
    decode_header(Rest, <<>>, Response#response{headers = [{H, V} | Response#response.headers]});
decode_header_value(<<$\s, Rest/bits>>, H, V, T, Response) ->
    decode_header_value(Rest, H, V, <<T/binary, $\s>>, Response);
decode_header_value(<<$\t, Rest/bits>>, H, V, T, Response) ->
    decode_header_value(Rest, H, V, <<T/binary, $\t>>, Response);
decode_header_value(<<C, Rest/bits>>, H, V, <<>>, Response) ->
    decode_header_value(Rest, H, <<V/binary, C>>, <<>>, Response);
decode_header_value(<<C, Rest/bits>>, H, V, T, Response) ->
    decode_header_value(Rest, H, <<V/binary, T/binary, C>>, <<>>, Response).

decode_header_value_lc(<<>>, H, V, T, Response) ->
    case fusco_sock:recv(Response#response.socket, Response#response.ssl) of
	{ok, Data} ->
	    decode_header_value_lc(Data, H, V, T, ?SIZE(Data, Response));
	{error, Reason} ->
	    {error, Reason}
    end;
decode_header_value_lc(<<$\r>>, H, V, T, Response) ->
    case fusco_sock:recv(Response#response.socket, Response#response.ssl) of
	{ok, Data} ->
	    decode_header_value_lc(<<$\r, Data/binary>>, H, V, T, ?SIZE(Data, Response));
	{error, Reason} ->
	    {error, Reason}
    end;
decode_header_value_lc(<<$\n, Rest/bits>>, <<"transfer-encoding">> = H, V, _T, Response) ->
    decode_header(Rest, <<>>, Response#response{headers = [{H, V} | Response#response.headers],
						transfer_encoding = V});
decode_header_value_lc(<<$\n, Rest/bits>>, H, V, _T, Response) ->
    decode_header(Rest, <<>>, Response#response{headers = [{H, V} | Response#response.headers],
						connection = V});
decode_header_value_lc(<<$\r, $\n, Rest/bits>>, <<"transfer-encoding">> = H, V, _T, Response) ->
    decode_header(Rest, <<>>, Response#response{headers = [{H, V} | Response#response.headers],
						transfer_encoding = V});
decode_header_value_lc(<<$\r, $\n, Rest/bits>>, H, V, _T, Response) ->
    decode_header(Rest, <<>>, Response#response{headers = [{H, V} | Response#response.headers],
						connection = V});
decode_header_value_lc(<<$\s, Rest/bits>>, H, V, T, Response) ->
    decode_header_value_lc(Rest, H, V, <<T/binary, $\s>>, Response);
decode_header_value_lc(<<$\t, Rest/bits>>, H, V, T, Response) ->
    decode_header_value_lc(Rest, H, V, <<T/binary, $\t>>, Response);
decode_header_value_lc(<<$A, Rest/bits>>, H, V, T, Response) ->
    decode_header_value_lc(Rest, H, <<V/binary, T/binary, $a>>, <<>>, Response);
decode_header_value_lc(<<$B, Rest/bits>>, H, V, T, Response) ->
    decode_header_value_lc(Rest, H, <<V/binary, T/binary, $b>>, <<>>, Response);
decode_header_value_lc(<<$C, Rest/bits>>, H, V, T, Response) ->
    decode_header_value_lc(Rest, H, <<V/binary, T/binary, $c>>, <<>>, Response);
decode_header_value_lc(<<$D, Rest/bits>>, H, V, T, Response) ->
    decode_header_value_lc(Rest, H, <<V/binary, T/binary, $d>>, <<>>, Response);
decode_header_value_lc(<<$E, Rest/bits>>, H, V, T, Response) ->
    decode_header_value_lc(Rest, H, <<V/binary, T/binary, $e>>, <<>>, Response);
decode_header_value_lc(<<$F, Rest/bits>>, H, V, T, Response) ->
    decode_header_value_lc(Rest, H, <<V/binary, T/binary, $f>>, <<>>, Response);
decode_header_value_lc(<<$G, Rest/bits>>, H, V, T, Response) ->
    decode_header_value_lc(Rest, H, <<V/binary, T/binary, $g>>, <<>>, Response);
decode_header_value_lc(<<$H, Rest/bits>>, H, V, T, Response) ->
    decode_header_value_lc(Rest, H, <<V/binary, T/binary, $h>>, <<>>, Response);
decode_header_value_lc(<<$I, Rest/bits>>, H, V, T, Response) ->
    decode_header_value_lc(Rest, H, <<V/binary, T/binary, $i>>, <<>>, Response);
decode_header_value_lc(<<$J, Rest/bits>>, H, V, T, Response) ->
    decode_header_value_lc(Rest, H, <<V/binary, T/binary, $j>>, <<>>, Response);
decode_header_value_lc(<<$K, Rest/bits>>, H, V, T, Response) ->
    decode_header_value_lc(Rest, H, <<V/binary, T/binary, $k>>, <<>>, Response);
decode_header_value_lc(<<$L, Rest/bits>>, H, V, T, Response) ->
    decode_header_value_lc(Rest, H, <<V/binary, T/binary, $l>>, <<>>, Response);
decode_header_value_lc(<<$M, Rest/bits>>, H, V, T, Response) ->
    decode_header_value_lc(Rest, H, <<V/binary, T/binary, $m>>, <<>>, Response);
decode_header_value_lc(<<$N, Rest/bits>>, H, V, T, Response) ->
    decode_header_value_lc(Rest, H, <<V/binary, T/binary, $n>>, <<>>, Response);
decode_header_value_lc(<<$O, Rest/bits>>, H, V, T, Response) ->
    decode_header_value_lc(Rest, H, <<V/binary, T/binary, $o>>, <<>>, Response);
decode_header_value_lc(<<$P, Rest/bits>>, H, V, T, Response) ->
    decode_header_value_lc(Rest, H, <<V/binary, T/binary, $p>>, <<>>, Response);
decode_header_value_lc(<<$Q, Rest/bits>>, H, V, T, Response) ->
    decode_header_value_lc(Rest, H, <<V/binary, T/binary, $q>>, <<>>, Response);
decode_header_value_lc(<<$R, Rest/bits>>, H, V, T, Response) ->
    decode_header_value_lc(Rest, H, <<V/binary, T/binary, $r>>, <<>>, Response);
decode_header_value_lc(<<$S, Rest/bits>>, H, V, T, Response) ->
    decode_header_value_lc(Rest, H, <<V/binary, T/binary, $s>>, <<>>, Response);
decode_header_value_lc(<<$T, Rest/bits>>, H, V, T, Response) ->
    decode_header_value_lc(Rest, H, <<V/binary, T/binary, $t>>, <<>>, Response);
decode_header_value_lc(<<$U, Rest/bits>>, H, V, T, Response) ->
    decode_header_value_lc(Rest, H, <<V/binary, T/binary, $u>>, <<>>, Response);
decode_header_value_lc(<<$V, Rest/bits>>, H, V, T, Response) ->
    decode_header_value_lc(Rest, H, <<V/binary, T/binary, $v>>, <<>>, Response);
decode_header_value_lc(<<$W, Rest/bits>>, H, V, T, Response) ->
    decode_header_value_lc(Rest, H, <<V/binary, T/binary, $w>>, <<>>, Response);
decode_header_value_lc(<<$X, Rest/bits>>, H, V, T, Response) ->
    decode_header_value_lc(Rest, H, <<V/binary, T/binary, $x>>, <<>>, Response);
decode_header_value_lc(<<$Y, Rest/bits>>, H, V, T, Response) ->
    decode_header_value_lc(Rest, H, <<V/binary, T/binary, $y>>, <<>>, Response);
decode_header_value_lc(<<$Z, Rest/bits>>, H, V, T, Response) ->
    decode_header_value_lc(Rest, H, <<V/binary, T/binary, $z>>, <<>>, Response);
decode_header_value_lc(<<C, Rest/bits>>, H, V, T, Response) ->
    decode_header_value_lc(Rest, H, <<V/binary, T/binary, C>>, <<>>, Response).

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
    decode_cookie_av_ws(Rest, #fusco_cookie{name = N, value = V});
decode_cookie_value(<<C, Rest/bits>>, N, V) ->
    decode_cookie_value(Rest, N, <<V/binary, C>>);
decode_cookie_value(<<>>, N, V) ->
    #fusco_cookie{name = N, value = V}.

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
    Co#fusco_cookie{path = Value};
decode_cookie_av_value(<<>>, Co, <<"max-age">>, Value) ->
    Co#fusco_cookie{max_age = max_age(Value)};
decode_cookie_av_value(<<>>, Co, <<"expires">>, Value) ->
    Co#fusco_cookie{expires = expires(Value)};
decode_cookie_av_value(<<$;, Rest/bits>>, Co, <<"path">>, Value) ->
    decode_cookie_av_ws(Rest, Co#fusco_cookie{path = Value});
decode_cookie_av_value(<<$;, Rest/bits>>, Co, <<"max-age">>, Value) ->
    decode_cookie_av_ws(Rest, Co#fusco_cookie{
				max_age = max_age(Value)});
decode_cookie_av_value(<<$;, Rest/bits>>, Co, <<"expires">>, Value) ->
    %% TODO parse expires
    decode_cookie_av_ws(Rest, Co#fusco_cookie{expires = expires(Value)});
decode_cookie_av_value(<<$;, Rest/bits>>, Co, _, _) ->
    decode_cookie_av_ws(Rest, Co);
decode_cookie_av_value(<<C, Rest/bits>>, Co, AV, Value) ->
    decode_cookie_av_value(Rest, Co, AV, <<Value/binary, C>>).


decode_body(<<>>, Response = #response{status_code = <<$1, _, _>>}) ->
    return(<<>>, Response);
decode_body(<<$\r, $\n, Rest/bits>>, Response) ->
    decode_body(Rest, Response);
decode_body(Rest, Response = #response{status_code = <<$1, _, _>>}) ->
    decode_status_line(Rest, #response{socket = Response#response.socket,
				       ssl = Response#response.ssl,
				       in_timestamp = Response#response.in_timestamp});
decode_body(Rest, Response = #response{transfer_encoding = <<"chunked">>}) ->
    download_chunked_body(Rest, Response);
decode_body(Rest, Response) ->
    case byte_size(Rest) >= Response#response.content_length of
	true ->
	    return(Rest, Response);
	false ->
	    case fusco_sock:recv(Response#response.socket, Response#response.ssl) of
		{ok, Data} ->
		    decode_body(<<Rest/binary, Data/binary>>, ?SIZE(Data, Response));
		_ ->
		    %% NOTE: Return what we have so far
		    return(Rest, Response)
	    end
    end.

download_chunked_body(<<>>, Response) ->
    case fusco_sock:recv(Response#response.socket, Response#response.ssl) of
	{ok, Data} ->
	    decode_chunked_body(Data, <<>>, <<>>, ?SIZE(Data, Response));
	_ ->
	    return(<<>>, Response)
    end;
download_chunked_body(Rest, Response) ->
    decode_chunked_body(Rest, <<>>, <<>>, Response).

decode_chunked_body(<<$0,$\r,$\n,$\r,$\n>>, Acc, _, Response) ->
    return(Acc, Response);
decode_chunked_body(<<$0, Rest/bits>>, Acc, Size, Response) when is_binary(Size) ->
    decode_chunked_body(Rest, Acc, <<Size/bits, $0>>, Response);
decode_chunked_body(<<$1, Rest/bits>>, Acc, Size, Response) when is_binary(Size) ->
    decode_chunked_body(Rest, Acc, <<Size/bits, $1>>, Response);
decode_chunked_body(<<$2, Rest/bits>>, Acc, Size, Response) when is_binary(Size) ->
    decode_chunked_body(Rest, Acc, <<Size/bits, $2>>, Response);
decode_chunked_body(<<$3, Rest/bits>>, Acc, Size, Response) when is_binary(Size) ->
    decode_chunked_body(Rest, Acc, <<Size/bits, $3>>, Response);
decode_chunked_body(<<$4, Rest/bits>>, Acc, Size, Response) when is_binary(Size) ->
    decode_chunked_body(Rest, Acc, <<Size/bits, $4>>, Response);
decode_chunked_body(<<$5, Rest/bits>>, Acc, Size, Response) when is_binary(Size) ->
    decode_chunked_body(Rest, Acc, <<Size/bits, $5>>, Response);
decode_chunked_body(<<$6, Rest/bits>>, Acc, Size, Response) when is_binary(Size) ->
    decode_chunked_body(Rest, Acc, <<Size/bits, $6>>, Response);
decode_chunked_body(<<$7, Rest/bits>>, Acc, Size, Response) when is_binary(Size) ->
    decode_chunked_body(Rest, Acc, <<Size/bits, $7>>, Response);
decode_chunked_body(<<$8, Rest/bits>>, Acc, Size, Response) when is_binary(Size) ->
    decode_chunked_body(Rest, Acc, <<Size/bits, $8>>, Response);
decode_chunked_body(<<$9, Rest/bits>>, Acc, Size, Response) when is_binary(Size) ->
    decode_chunked_body(Rest, Acc, <<Size/bits, $9>>, Response);
decode_chunked_body(<<$\r,$\n, Rest/bits>>, Acc, <<>>, Response) ->
    decode_chunked_body(Rest, Acc, <<>>, Response);
decode_chunked_body(<<$\r,$\n, Rest/bits>>, Acc, Size, Response) when is_binary(Size) ->
    IntSize = erlang:binary_to_integer(Size, 16),
    decode_chunked_body(Rest, Acc, IntSize, Response);
decode_chunked_body(Rest, Acc, Size, Response) ->
    case byte_size(Rest) of
	S when S == Size ->
	    decode_chunked_body(<<>>, <<Acc/bits, Rest/bits>>, <<>>, Response);
	S when S < Size ->
	    case fusco_sock:recv(Response#response.socket, Response#response.ssl) of
		{ok, Data} ->
		    decode_chunked_body(<<Rest/bits, Data/bits>>, Acc, Size, ?SIZE(Data, Response));
		_ ->
		    return(Acc, Response)
	    end;
	S when S > Size ->
	    Current = binary:part(Rest, 0, Size),
	    Next = binary:part(Rest, Size, S - Size),
	    decode_chunked_body(Next, <<Acc/bits, Current/bits>>, <<>>, Response)
    end.

return(Body, Response) ->
    Response#response{body = Body}.

max_age(Value) ->
    list_to_integer(binary_to_list(Value)) * 1000000.

%% http://tools.ietf.org/html/rfc2616#section-3.3.1
%% Supports some non-standard datetime (Tomcat) Tue, 06-Nov-1994 08:49:37 GMT
expires(<<_,_,_,$,,$\s,D1,D2,$\s,M1,M2,M3,$\s,Y1,Y2,Y3,Y4,$\s,Rest/bits>>) ->
    expires(Rest, {list_to_integer([Y1,Y2,Y3,Y4]),month(<<M1,M2,M3>>),list_to_integer([D1,D2])});
expires(<<_,_,_,$\s,Mo1,Mo2,Mo3,$\s,D1,D2,$\s,H1,H2,$:,M1,M2,$:,S1,S2,$\s,Y1,Y2,Y3,Y4,_Rest/bits>>) ->
    {{list_to_integer([Y1,Y2,Y3,Y4]),month(<<Mo1,Mo2,Mo3>>),list_to_integer([D1,D2])},
     {list_to_integer([H1,H2]), list_to_integer([M1,M2]), list_to_integer([S1,S2])}};
expires(<<_,_,_,$,,$\s,Rest/bits>>) ->
    expires(Rest);
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
expires(<<D1,D2,$\-,M1,M2,M3,$\-,Y1,Y2,Y3,Y4,$\s,Rest/bits>>) ->
    expires(Rest, {list_to_integer([Y1,Y2,Y3,Y4]),month(<<M1,M2,M3>>),list_to_integer([D1,D2])});
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
