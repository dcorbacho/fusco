%%%=============================================================================
%%% @copyright (C) 1999-2013, Erlang Solutions Ltd
%%% @author Diana Corbacho <diana.corbacho@erlang-solutions.com>
%%% @doc
%%% @end
%%%=============================================================================
-module(lhttpc_protocol_eqc).

-include_lib("eqc/include/eqc.hrl").
-include("lhttpc.hrl").

-export([prop_http_response/0]).

prop_http_response() ->
    ?FORALL({StatusLine, Headers, Cookies, Body},
	    {http_eqc_gen:status_line(), http_eqc_gen:headers(),
	     list(http_eqc_gen:set_cookie()), http_eqc_gen:body()},
	    begin
		ContentLength = list_to_binary(integer_to_list(byte_size(Body))),
		FinalHeaders = [{<<"Content-Length">>, ContentLength}
				| Headers],
		Msg = build_valid_message(StatusLine, FinalHeaders, Cookies, Body),
		L = {_, _, Socket} = test_utils:start_listener({fragmented, Msg}),
		test_utils:send_message(Socket),
		Recv = lhttpc_protocol:recv(Socket, false),
		test_utils:stop_listener(L),
		Expected = expected_output(StatusLine, FinalHeaders, Cookies, Body),
		Cleared = clear_connection(clear_timestamps(Recv)),
		?WHENFAIL(io:format("Message:~n=======~n~s~n=======~nResponse:"
				    " ~p~nCleared: ~p~nExpected: ~p~n",
				    [binary:list_to_bin(Msg), Recv, Cleared, Expected]),
			  case Cleared of
			      Expected ->
				  true;
			      _ ->
				  false
			  end)
	    end).

build_valid_message({HttpVersion, StatusCode, Reason}, Headers, Cookies, Body) ->
    SL = [HttpVersion, sp(), StatusCode, sp(), Reason, crlf()],
    HS = [[Name, colon(), Value, crlf()] || {Name, Value} <- Headers],
    CS = [[Name, colon(), build_cookie(Cookie), crlf()] || {Name, Cookie} <- Cookies],
    [SL, HS ++ CS, crlf(), Body]. 

expected_output({HttpVersion, StatusCode, Reason}, Headers, Cookies, Body) ->
    Version = http_version(HttpVersion),
    OCookies = [{Name, list_to_binary(build_cookie(Cookie))} || {Name, Cookie} <- Cookies],
    LowerHeaders = lists:reverse(headers_to_lower(Headers ++ OCookies)),
    CookiesRec = output_cookies(Cookies),
    {Version, StatusCode, Reason, CookiesRec, LowerHeaders,
     to_lower(proplists:get_value(<<"connection">>, LowerHeaders)), Body}.

output_cookies(Cookies) ->
    output_cookies(Cookies, []).

output_cookies([{_SetCookie, {{K, V}, Avs}} | Rest], Acc) ->
    MaxAge = output_max_age(proplists:get_value(<<"Max-Age">>, Avs)),
    Path = proplists:get_value(<<"Path">>, Avs),
    Expires = output_expires(proplists:get_value(<<"Expires">>, Avs)),
    Cookie = #lhttpc_cookie{name = K, value = V, max_age = MaxAge, path = Path,
			    expires = Expires},
    output_cookies(Rest, [Cookie | Acc]);
output_cookies([], Acc) ->
    Acc.

output_max_age(undefined) ->
    undefined;
output_max_age(Age) ->
    list_to_integer(binary_to_list(Age)) * 1000000.

output_expires({rfc1123date, {_, {date1, {Day, Month, Year}}, {H, M, S}}}) ->
    {{st_to_int(Year), month(Month), st_to_int(Day)},
     {st_to_int(H), st_to_int(M), st_to_int(S)}};
output_expires({rfc850date, {_, {date2, {Day, Month, Year}}, {H, M, S}}}) ->
    {{st_to_int(Year) + 2000, month(Month), st_to_int(Day)},
     {st_to_int(H), st_to_int(M), st_to_int(S)}};
output_expires({asctimedate, {_, {date3, {Day, Month}}, {H, M, S}, Year}}) ->
    {{st_to_int(Year), month(Month), st_to_int(Day)},
     {st_to_int(H), st_to_int(M), st_to_int(S)}};
output_expires(undefined) ->
    undefined.

st_to_int(L) ->
    list_to_integer(L).

month(Month) ->
    proplists:get_value(Month, months()).

months() ->
    [{"Jan", 1}, {"Feb", 2}, {"Mar", 3}, {"Apr", 4},
     {"May", 5}, {"Jun", 6}, {"Jul", 7}, {"Aug", 8},
     {"Sep", 9}, {"Oct", 10}, {"Nov", 11}, {"Dec", 12}].

clear_timestamps({V, S, R, C, H, Co, B}) ->
    {V, S, R, [Co#lhttpc_cookie{timestamp=undefined} || Co <- C], H, to_lower(Co), B}.

clear_connection({V, S, R, C, H, Co, B}) ->
    {V, S, R, C, H, to_lower(Co), B}.

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

http_version(<<"HTTP/1.1">>) ->
    {1, 1};
http_version(<<"HTTP/1.0">>) ->
    {1, 0}.

headers_to_lower(Headers) ->
    [begin
	 He = to_lower(H),
	 case He of
	     <<"connection">> ->
		 {He, to_lower(V)};
	     _ ->
		 {He, V}
	 end
     end || {H, V} <- Headers].

to_lower(undefined) ->
    undefined;
to_lower(Bin) ->
    list_to_binary(string:to_lower(binary_to_list(Bin))).

build_cookie({{K, V}, Avs}) ->
    CookiePair = [K, eq(), V],
    CookieAvs = build_cookie_avs(Avs),
    CookiePair ++ CookieAvs.

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
