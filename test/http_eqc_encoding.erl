%%%=============================================================================
%%% @copyright (C) 1999-2014, Erlang Solutions Ltd
%%% @author Diana Corbacho <diana.corbacho@erlang-solutions.com>
%%% @doc
%%% @end
%%%=============================================================================
-module(http_eqc_encoding).
-copyright("2014, Erlang Solutions Ltd.").

-export([add_content_length/2,
         add_transfer_encoding/2,
         body/1]).

-export([build_valid_response/4,
         build_cookie/1]).

%%==============================================================================
%% API
%%==============================================================================
add_content_length(Headers, <<>>) ->
    Headers;
add_content_length(Headers, Body) ->
    ContentLength = list_to_binary(integer_to_list(byte_size(Body))),
    [{<<"Content-Length">>, ContentLength} | Headers].

body({_, <<$1, _, _>>, _}) ->
    <<>>;
body({_, <<$2,$0,$4>>, _}) ->
    <<>>;
body({_, <<$3,$0,$4>>, _}) ->
    <<>>;
body(_) ->
    http_eqc_gen:body().

add_transfer_encoding(Headers, <<>>) ->
    Headers;
add_transfer_encoding(Headers, Encoding) ->
    lists:keystore(<<"Transfer-Encoding">>, 1,
		   remove_transfer_encoding(Headers),
		   {<<"Transfer-Encoding">>, Encoding}).

build_valid_response({HttpVersion, StatusCode, Reason}, Headers, Cookies, Body) ->
    SL = [HttpVersion, sp(), StatusCode, sp(), Reason, crlf()],
    HS = [[Name, colon(), Value, crlf()] || {Name, Value} <- Headers],
    CS = [[Name, colon(), build_cookie(Cookie), crlf()] || {Name, Cookie} <- Cookies],
    [SL, HS ++ CS, crlf(), build_body(Body)].

build_cookie({{K, V}, Avs}) ->
    CookiePair = [K, eq(), V],
    CookieAvs = build_cookie_avs(Avs),
    CookiePair ++ CookieAvs.
%%==============================================================================
%% Internal functions
%%==============================================================================    
remove_transfer_encoding(Headers) ->
    lists:filter(fun({H, _}) -> H =/= <<"Transfer-Encoding">> end, Headers).
    
build_body(Body) when is_binary(Body) ->
    Body;
build_body(List) ->
    list_to_binary(
      io_lib:format("~s0\r\n\r\n",
		    [[io_lib:format("~s\r\n~s\r\n",
				    [erlang:integer_to_list(Nat, 16), Body])
		      || {Nat, Body} <- List]])).

build_cookie_avs(Avs) ->
    build_cookie_avs(Avs, []).
    
build_cookie_avs([{<<"Expires">> = K, {rfc1123date, {Wkday, Date1, Time}}} | Rest], Acc) ->
    Date = build_date(Date1),
    BTime = build_time(Time),
    V = [Wkday, $,, $\s, Date, $\s, BTime, $\s, "GMT"],
    build_cookie_avs(Rest, [[semicolon(), sp(), K, eq(), V] | Acc]);
build_cookie_avs([{<<"Expires">> = K, {rfc850date, {Weekday, Date2, Time}}} | Rest], Acc) ->
    Date = build_date(Date2),
    BTime = build_time(Time),
    V = [Weekday, $,, $\s, Date, $\s, BTime, $\s, "GMT"],
    build_cookie_avs(Rest, [[semicolon(), sp(), K, eq(), V] | Acc]);
build_cookie_avs([{<<"Expires">> = K, {asctimedate, {Wkday, Date3, Time, Year}}} | Rest], Acc) ->
    BTime = build_time(Time),
    Date = build_date(Date3),
    V = [Wkday, $\s, Date, $\s, BTime, $\s, Year],
    build_cookie_avs(Rest, [[semicolon(), sp(), K, eq(), V] | Acc]);
build_cookie_avs([{K, V} | Rest], Acc) ->
    build_cookie_avs(Rest, [[semicolon(), sp(), K, eq(), V] | Acc]);
build_cookie_avs([K | Rest], Acc) ->
    build_cookie_avs(Rest, [[semicolon(), sp(), K] | Acc]);
build_cookie_avs([], Acc) ->
    Acc.

build_date({date1, {Day, Month, Year}}) ->
    [Day, $\s, Month, $\s, Year];
build_date({date2, {Day, Month, Year}}) ->
    [Day, $-, Month, $-, Year];
build_date({date3, {Day, Month}}) ->
    [Month, $\s, Day].

build_time({H, M, S}) ->
    [H, $:, M, $:, S].

colon() ->
    <<$:>>.

semicolon() ->
    <<$;>>.

sp() ->
    <<$\s>>.

crlf() ->
    <<$\r,$\n>>.

eq() ->
    <<$=>>.
