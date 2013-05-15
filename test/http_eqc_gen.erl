%%%=============================================================================
%%% @copyright (C) 1999-2013, Erlang Solutions Ltd
%%% @author Diana Corbacho <diana.corbacho@erlang-solutions.com>
%%% @doc Quickcheck generators for HTTP messages
%%% @end
%%%=============================================================================
-module(http_eqc_gen).

-include_lib("eqc/include/eqc.hrl").

-compile(export_all).

%% RFC 2616
general_header() ->
    [{<<"Cache-Control">>, small_valid_bin()},
     {<<"Connection">>, small_valid_bin()},
     {<<"Date">>, small_valid_bin()},
     {<<"Pragma">>, small_valid_bin()},
     {<<"Trailer">>, small_valid_bin()},
     {<<"Transfer-Encoding">>, small_valid_bin()},
     {<<"Upgrade">>, small_valid_bin()},
     {<<"Via">>, small_valid_bin()},
     {<<"Warning">>, small_valid_bin()}].

%% RFC 2616
entity_header() ->
    [{<<"Allow">>, small_valid_bin()},
     {<<"Content-Encoding">>, small_valid_bin()},
     {<<"Content-Language">>, small_valid_bin()},
     {<<"Content-Length">>, small_valid_bin()},
     {<<"Content-Location">>, small_valid_bin()},
     {<<"Content-MD5">>, small_valid_bin()},
     {<<"Content-Range">>, small_valid_bin()},
     {<<"Content-Type">>, small_valid_bin()},
     {<<"Expires">>, small_valid_bin()},
     {<<"Last-Modified">>, small_valid_bin()}].

%% RFC 2616
response_header() ->
    [{<<"Accept-Ranges">>, small_valid_bin()},
     {<<"Age">>, small_valid_bin()},
     {<<"ETag">>, small_valid_bin()},
     {<<"Location">>, small_valid_bin()},
     {<<"Proxy-Authenticate">>, small_valid_bin()},
     {<<"Retry-After">>, small_valid_bin()},
     {<<"Server">>, small_valid_bin()},
     {<<"Vary">>, small_valid_bin()},
     {<<"WWW-Authenticate">>, small_valid_bin()}].

header() ->
    lists:append([general_header(), entity_header(), response_header()]).

headers() ->
    ?LET(Headers, list(oneof(header())), Headers).

http_version() ->
    [<<"HTTP/1.0">>, <<"HTTP/1.1">>].

informational_code() ->
    [{<<"100">>, <<"Continue">>},
     {<<"101">>, <<"Switching protocols">>}].

success_code() ->
    [{<<"200">>, <<"OK">>},
     {<<"201">>, <<"Created">>},
     {<<"202">>, <<"Accepted">>},
     {<<"203">>, <<"Non-Authoritative Information">>},
     {<<"204">>, <<"No Content">>},
     {<<"205">>, <<"Reset Content">>},
     {<<"206">>, <<"Partial Content">>}].

redirection_code() ->
    [{<<"300">>, <<"Multiple Choices">>},
     {<<"301">>, <<"Moved Permanently">>},
     {<<"302">>, <<"Found">>},
     {<<"303">>, <<"See Other">>},
     {<<"304">>, <<"Not Modified">>},
     {<<"305">>, <<"Use Proxy">>},
     {<<"307">>, <<"Temporary Redirect">>}].

client_error_code() ->
    [{<<"400">>, <<"Bad Request">>},
     {<<"401">>, <<"Unauthorized">>},
     {<<"402">>, <<"Payment Required">>},
     {<<"403">>, <<"Forbidden">>},
     {<<"404">>, <<"Not Found">>},
     {<<"405">>, <<"Method Not Allowed">>},
     {<<"406">>, <<"Not Acceptable">>},
     {<<"407">>, <<"Proxy Authentication Required">>},
     {<<"408">>, <<"Request Time-out">>},
     {<<"409">>, <<"Conflict">>},
     {<<"410">>, <<"Gone">>},
     {<<"411">>, <<"Length Required">>},
     {<<"412">>, <<"Precondition Failed">>},
     {<<"413">>, <<"Request Entity Too Large">>},
     {<<"414">>, <<"Request-URI Too Large">>},
     {<<"415">>, <<"Unsupported Media Type">>},
     {<<"416">>, <<"Requested range not satisfiable">>},
     {<<"417">>, <<"Expectation Failed">>}].

server_error_code() ->
    [{<<"500">>, <<"Internal Server Error">>},
     {<<"501">>, <<"Not Implemented">>},
     {<<"502">>, <<"Bad Gateway">>},
     {<<"503">>, <<"Service Unavailable">>},
     {<<"504">>, <<"Gateway Time-out">>},
     {<<"505">>, <<"HTTP Version not supported">>}].

status_code() ->
    lists:append([informational_code(), success_code(), redirection_code(),
		  client_error_code(), server_error_code()]).

status_line() ->
    ?LET({HttpVersion, {StatusCode, Reason}},
	 {oneof(http_version()), oneof(status_code())},
	 {HttpVersion, StatusCode, Reason}).

small_valid_bin() ->
    ?LET(String, vector(5, choose($A, $z)),
	 list_to_binary(String)).
