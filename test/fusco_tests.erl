%%% ----------------------------------------------------------------------------
%%% Copyright (c) 2009, Erlang Training and Consulting Ltd.
%%% All rights reserved.
%%%
%%% Redistribution and use in source and binary forms, with or without
%%% modification, are permitted provided that the following conditions are met:
%%%    * Redistributions of source code must retain the above copyright
%%%      notice, this list of conditions and the following disclaimer.
%%%    * Redistributions in binary form must reproduce the above copyright
%%%      notice, this list of conditions and the following disclaimer in the
%%%      documentation and/or other materials provided with the distribution.
%%%    * Neither the name of Erlang Training and Consulting Ltd. nor the
%%%      names of its contributors may be used to endorse or promote products
%%%      derived from this software without specific prior written permission.
%%%
%%% THIS SOFTWARE IS PROVIDED BY Erlang Training and Consulting Ltd. ''AS IS''
%%% AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
%%% IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
%%% ARE DISCLAIMED. IN NO EVENT SHALL Erlang Training and Consulting Ltd. BE
%%% LIABLE SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR
%%% BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY,
%%% WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR
%%% OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF
%%% ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
%%% ----------------------------------------------------------------------------

%%% @author Oscar Hellström <oscar@hellstrom.st>
-module(fusco_tests).

-export([test_no/2]).

-include_lib("eunit/include/eunit.hrl").

test_no(N, Tests) ->
    setelement(2, Tests,
        setelement(4, element(2, Tests),
            lists:nth(N, element(4, element(2, Tests))))).

%%% Eunit setup stuff

start_app() ->
    application:start(crypto),
    application:start(public_key),
    ok = application:start(ssl).

stop_app(_) ->
    ok = application:stop(ssl).

tcp_test_() ->
    {inorder,
        {setup, fun start_app/0, fun stop_app/1, [
                ?_test(connection_refused()),
                ?_test(basic_auth()),
                ?_test(missing_basic_auth()),
                ?_test(wrong_basic_auth()),
                ?_test(get_with_connect_options()),
                ?_test(no_content_length()),
                ?_test(no_content_length_1_0()),
                ?_test(options_content()),
                ?_test(server_connection_close()),
                ?_test(client_connection_close()),
                ?_test(pre_1_1_server_connection()),
                ?_test(pre_1_1_server_keep_alive()),
                ?_test(post_100_continue()),
                ?_test(persistent_connection()),
                ?_test(request_timeout()),
                ?_test(close_connection()),
                ?_test(trailing_space_header())
            ]}
    }.

ssl_test_() ->
    {inorder,
        {setup, fun start_app/0, fun stop_app/1, [
                ?_test(ssl_get()),
                ?_test(ssl_get_ipv6()),
                ?_test(ssl_post())
            ]}
    }.

options_test() ->
    invalid_options().

cookies_test() ->
    cookies().

connection_refused() ->
    Response = fusco:connect("http://127.0.0.1:50234/none", []),
    ?assertEqual({error, econnrefused}, Response).

basic_auth() ->
    User = "foo",
    Passwd = "bar",
    {ok, _, _, Port} = webserver:start(gen_tcp, [webserver_utils:basic_auth_responder(User, Passwd)]),
    URL = url(Port),
    {ok, Client} = fusco:connect(URL, []),
    {ok, Response} = fusco:request(Client, "/empty", "GET",
				    [{<<"Authorization">>,
					     ["Basic ", base64:encode(User ++ ":" ++ Passwd)]}],
				    [], 1000),
    ?assertEqual({<<"200">>, <<"OK">>}, status(Response)),
    ?assertEqual(<<"OK">>, body(Response)).

missing_basic_auth() ->
    User = "foo",
    Passwd = "bar",
    {ok, _, _, Port} = webserver:start(gen_tcp, [webserver_utils:basic_auth_responder(User, Passwd)]),
    URL = url(Port),
    {ok, Client} = fusco:connect(URL, []),
    {ok, Response} = fusco:request(Client, "/empty", "GET", [], [], 1000),
    ?assertEqual({<<"401">>, <<"Unauthorized">>}, status(Response)),
    ?assertEqual(<<"missing_auth">>, body(Response)).

wrong_basic_auth() ->
    User = "foo",
    Passwd = "bar",
    {ok, _, _, Port} = webserver:start(gen_tcp, [webserver_utils:basic_auth_responder(User, Passwd)]),
    URL = url(Port),
    {ok, Client} = fusco:connect(URL, []),
    {ok, Response} = fusco:request(Client, "/empty", "GET",
					   [{<<"Authorization">>,
					     ["Basic ", base64:encode(User ++ ":wrong_password")]}],
				    [], 1000),
    ?assertEqual({<<"401">>, <<"Unauthorized">>}, status(Response)),
    ?assertEqual(<<"wrong_auth">>, body(Response)).

get_with_connect_options() ->
    {ok, _, _, Port} = webserver:start(gen_tcp, [fun webserver_utils:empty_body/5]),
    URL = url(Port),
    Options = [{connect_options, [{ip, {127, 0, 0, 1}}, {port, 0}]}],
    {ok, Client} = fusco:connect(URL, Options),
    {ok, Response} = fusco:request(Client, "/empty", "GET", [], [], 1, 1000),
    ?assertEqual({<<"200">>, <<"OK">>}, status(Response)),
    ?assertEqual(<<>>, body(Response)).

no_content_length() ->
    {ok, _, _, Port} = webserver:start(gen_tcp, [fun webserver_utils:no_content_length/5]),
    URL = url(Port),
    {ok, Client} = fusco:connect(URL, []),
    {ok, Response} = fusco:request(Client, "/no_cl", "GET", [], [], 1000),
    ?assertEqual({<<"200">>, <<"OK">>}, status(Response)),
    ?assertEqual(list_to_binary(webserver_utils:default_string()), body(Response)).

no_content_length_1_0() ->
    {ok, _, _, Port} = webserver:start(gen_tcp, [fun webserver_utils:no_content_length_1_0/5]),
    URL = url(Port),
    {ok, Client} = fusco:connect(URL, []),
    {ok, Response} = fusco:request(Client, "/no_cl", "GET", [], [], 1000),
    ?assertEqual({<<"200">>, <<"OK">>}, status(Response)),
    ?assertEqual(list_to_binary(webserver_utils:default_string()), body(Response)).

%% Check the header value is trimming spaces on header values
%% which can cause crash in fusco_client:body_type when Content-Length
%% is converted from list to integer
trailing_space_header() ->
    {ok, _, _, Port} = webserver:start(gen_tcp, [fun webserver_utils:trailing_space_header/5]),
    URL = url(Port),
    {ok, Client} = fusco:connect(URL, []),
    {ok, Response} = fusco:request(Client, "/no_cl", "GET", [], [], 1000),
    Headers = headers(Response),
    ContentLength = fusco_lib:header_value(<<"content-length">>, Headers),
    ?assertEqual(<<"14">>, ContentLength).


options_content() ->
    {ok, _, _, Port} = webserver:start(gen_tcp, [fun webserver_utils:simple_response/5]),
    URL = url(Port),
    {ok, Client} = fusco:connect(URL, []),
    {ok, Response} = fusco:request(Client, "/options_content", "OPTIONS", [], [], 1000),
    ?assertEqual({<<"200">>, <<"OK">>}, status(Response)),
    ?assertEqual(list_to_binary(webserver_utils:default_string()), body(Response)).

server_connection_close() ->
    {ok, _, _, Port} = webserver:start(gen_tcp, [fun webserver_utils:respond_and_close/5]),
    URL = url(Port),
    Body = pid_to_list(self()),
    {ok, Client} = fusco:connect(URL, []),
    {ok, Response} = fusco:request(Client, "/close", "PUT", [], Body, 1000),
    ?assertEqual({<<"200">>, <<"OK">>}, status(Response)),
    ?assertEqual(list_to_binary(webserver_utils:default_string()), body(Response)),
    receive closed -> ok end.

client_connection_close() ->
    {ok, _, _, Port} = webserver:start(gen_tcp, [fun webserver_utils:respond_and_wait/5]),
    URL = url(Port),
    Body = pid_to_list(self()),
    Hdrs = [{<<"Connection">>, <<"close">>}],
    {ok, Client} = fusco:connect(URL, []),
    {ok, _} = fusco:request(Client, "/close", "PUT", Hdrs, Body, 1000),
    % Wait for the server to see that socket has been closed
    receive closed -> ok end.

pre_1_1_server_connection() ->
    {ok, _, _, Port} = webserver:start(gen_tcp, [fun webserver_utils:pre_1_1_server/5]),
    URL = url(Port),
    Body = pid_to_list(self()),
    {ok, Client} = fusco:connect(URL, []),
    {ok, _} = fusco:request(Client, "/close", "PUT", [], Body, 1000),
    % Wait for the server to see that socket has been closed.
    % The socket should be closed by us since the server responded with a
    % 1.0 version, and not the Connection: keep-alive header.
    receive closed -> ok end.

pre_1_1_server_keep_alive() ->
    {ok, _, _, Port} = webserver:start(gen_tcp,
				   [
				    fun webserver_utils:pre_1_1_server_keep_alive/5,
				    fun webserver_utils:pre_1_1_server/5
				   ]),
    URL = url(Port),
    Body = pid_to_list(self()),
    {ok, Client} = fusco:connect(URL, []),
    {ok, Response1} = fusco:request(Client, "/close", "GET", [], [], 1000),
    {ok, Response2} = fusco:request(Client, "/close", "PUT", [], Body, 1000),
    ?assertEqual({<<"200">>, <<"OK">>}, status(Response1)),
    ?assertEqual({<<"200">>, <<"OK">>}, status(Response2)),
    ?assertEqual(list_to_binary(webserver_utils:default_string()), body(Response1)),
    ?assertEqual(list_to_binary(webserver_utils:default_string()), body(Response2)),
    % Wait for the server to see that socket has been closed.
    % The socket should be closed by us since the server responded with a
    % 1.0 version, and not the Connection: keep-alive header.
    receive closed -> ok end.

post_100_continue() ->
    {ok, _, _, Port} = webserver:start(gen_tcp, [fun webserver_utils:copy_body_100_continue/5]),
    URL = url(Port),
    {X, Y, Z} = now(),
    Body = [
        "This is a rather simple post :)",
        integer_to_list(X),
        integer_to_list(Y),
        integer_to_list(Z)
    ],
    {ok, Client} = fusco:connect(URL, []),
    {ok, Response} = fusco:request(Client, "/post", "POST", [], Body, 1000),
    {StatusCode, ReasonPhrase} = status(Response),
    ?assertEqual(<<"200">>, StatusCode),
    ?assertEqual(<<"OK">>, ReasonPhrase),
    ?assertEqual(iolist_to_binary(Body), body(Response)).

persistent_connection() ->
    {ok, _, _, Port} = webserver:start(gen_tcp,
				   [
				    fun webserver_utils:simple_response/5,
				    fun webserver_utils:simple_response/5,
				    fun webserver_utils:copy_body/5
				   ]),
    URL = url(Port),
    {ok, Client} = fusco:connect(URL, []),
    {ok, FirstResponse} = fusco:request(Client, "/persistent", "GET", [], [], 1, 1000),
    Headers = [{"KeepAlive", "300"}], % shouldn't be needed
    {ok, SecondResponse} = fusco:request(Client, "/persistent", "GET", Headers, [], 1, 1000),
    {ok, ThirdResponse} = fusco:request(Client, "/persistent", "POST", [], [], 1, 1000),
    ?assertEqual({<<"200">>, <<"OK">>}, status(FirstResponse)),
    ?assertEqual(list_to_binary(webserver_utils:default_string()), body(FirstResponse)),
    ?assertEqual({<<"200">>, <<"OK">>}, status(SecondResponse)),
    ?assertEqual(list_to_binary(webserver_utils:default_string()), body(SecondResponse)),
    ?assertEqual({<<"200">>, <<"OK">>}, status(ThirdResponse)),
    ?assertEqual(<<>>, body(ThirdResponse)).

request_timeout() ->
    {ok, _, _, Port} = webserver:start(gen_tcp, [fun webserver_utils:very_slow_response/5]),
    URL = url(Port),
    {ok, Client} = fusco:connect(URL, []),
    ?assertEqual({error, timeout}, fusco:request(Client, "/slow", "GET", [], [], 50)).

close_connection() ->
    {ok, _, _, Port} = webserver:start(gen_tcp, [fun webserver_utils:close_connection/5]),
    URL = url(Port),
    {ok, Client} = fusco:connect(URL, []),
    {ok, Response} = fusco:request(Client, "/close", "GET", [], [], 1000),
    ?assertEqual({<<"200">>, <<"OK">>}, status(Response)).

ssl_get() ->
    {ok, _, _, Port} = webserver:start(ssl, [fun webserver_utils:simple_response/5]),
    URL = ssl_url(Port),
    {ok, Client} = fusco:connect(URL, []),
    {ok, Response} = fusco:request(Client, "/simple", "GET", [], [], 1, 1000),
    ?assertEqual({<<"200">>, <<"OK">>}, status(Response)),
    ?assertEqual(list_to_binary(webserver_utils:default_string()), body(Response)).

ssl_get_ipv6() ->
    {ok, _, _, Port} = webserver:start(ssl, [fun webserver_utils:simple_response/5], inet6),
    URL = ssl_url(inet6, Port),
    {ok, Client} = fusco:connect(URL, []),
    {ok, Response} = fusco:request(Client, "/simple", "GET", [], [], 1, 1000),
    ?assertEqual({<<"200">>, <<"OK">>}, status(Response)),
    ?assertEqual(list_to_binary(webserver_utils:default_string()), body(Response)).

ssl_post() ->
    {ok, _, _, Port} = webserver:start(ssl, [fun webserver_utils:copy_body/5]),
    URL = ssl_url(Port),
    Body = "SSL Test <o/",
    BinaryBody = list_to_binary(Body),
    {ok, Client} = fusco:connect(URL, []),
    {ok, Response} = fusco:request(Client, "/simple", "POST", [], Body, 1, 1000),
    ?assertEqual({<<"200">>, <<"OK">>}, status(Response)),
    ?assertEqual(BinaryBody, body(Response)).

invalid_options() ->
    URL = url(5050),
    ?assertError({bad_option, bad_option},
        fusco:connect(URL, [bad_option, {foo, bar}])),
    ?assertError({bad_option, {foo, bar}},
        fusco:connect(URL, [{foo, bar}, bad_option])).

cookies() ->
    {ok, _, _, Port} = webserver:start(gen_tcp, [fun webserver_utils:set_cookie_response/5, fun webserver_utils:expired_cookie_response/5,
            fun webserver_utils:receive_right_cookies/5]),
    URL = url(Port),
    Options = [{use_cookies, true}],
    {ok, Client} = fusco:connect(URL, Options),
    {ok, Response1} = fusco:request(Client, "/cookies", "GET", [], <<>>, 1000),
    ?assertEqual({<<"200">>, <<"OK">>}, status(Response1)),
    {ok, Response2} = fusco:request(Client, "/cookies", "GET", [], <<>>, 1000),
    ?assertEqual({<<"200">>, <<"OK">>}, status(Response2)),
    {ok, Response3} = fusco:request(Client, "/cookies", "GET", [], <<>>, 1000),
    ?assertEqual({<<"200">>, <<"OK">>}, status(Response3)).

url(Port) ->
    url(inet, Port).

url(inet, Port) ->
    "http://localhost:" ++ integer_to_list(Port);
url(inet6, Port) ->
    "http://[::1]:" ++ integer_to_list(Port).

ssl_url(Port) ->
    "https://localhost:" ++ integer_to_list(Port).

ssl_url(inet6, Port) ->
    "https://[::1]:" ++ integer_to_list(Port).

status({Status, _, _, _, _}) ->
    Status.

body({_, _, Body, _, _}) ->
    Body.

headers({_, Headers, _, _, _}) ->
    Headers.
