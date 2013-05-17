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

%%------------------------------------------------------------------------------
%%% @private
%%% @author Oscar Hellström <oscar@hellstrom.st>
%%% @author Ramon Lastres Guerrero <ramon.lastres@erlang-solutions.com>
%%% @doc
%%% This module implements various library functions used in lhttpc
%%------------------------------------------------------------------------------
-module(lhttpc_lib).

-export([parse_url/1,
         format_request/6,
         header_value/2, header_value/3,
         maybe_atom_to_list/1,
         dec/1,
         update_cookies/2,
         to_lower/1,
	 get_value/2,
	 get_value/3,
	 host_header/2,
	 is_close/1,
	 is_keep_alive/1]).

-include("lhttpc_types.hrl").
-include("lhttpc.hrl").

-define(HTTP_LINE_END, <<"\r\n">>).

%%==============================================================================
%% Exported functions
%%==============================================================================

%%------------------------------------------------------------------------------
%% @spec header_value(Header, Headers) -> undefined | term()
%% Header = string()
%% Headers = [{header(), term()}]
%% Value = term()
%% @doc
%% Returns the value associated with the `Header' in `Headers'.
%% `Header' must be a lowercase string, since every header is mangled to
%% check the match.
%% @end
%%------------------------------------------------------------------------------
-spec header_value(string(), headers()) -> undefined | term().
header_value(Hdr, Hdrs) ->
    header_value(Hdr, Hdrs, undefined).

%%------------------------------------------------------------------------------
%% @spec header_value(Header, Headers, Default) -> Default | term()
%% Header = string()
%% Headers = [{string(), term()}]
%% Value = term()
%% Default = term()
%% @doc
%% Returns the value associated with the `Header' in `Headers'.
%% `Header' must be a lowercase string, since every header is mangled to
%% check the match.  If no match is found, `Default' is returned.
%% @end
%%------------------------------------------------------------------------------
-spec header_value(binary(), headers(), term()) -> term().
header_value(Hdr, Hdrs, Default) ->
    %% TODO ensure headers and values are stripped
    case lists:keyfind(Hdr, 1, Hdrs) of
	false ->
	    Default;
	{Hdr, Value} ->
	    Value
    end.

%%------------------------------------------------------------------------------
%% @spec (Item) -> OtherItem
%%   Item = atom() | list()
%%   OtherItem = list()
%% @doc
%% Will make any item, being an atom or a list, in to a list. If it is a
%% list, it is simple returned.
%% @end
%%------------------------------------------------------------------------------
-spec maybe_atom_to_list(atom() | list()) -> list().
maybe_atom_to_list(Atom) when is_atom(Atom) ->
    atom_to_list(Atom);
maybe_atom_to_list(List) ->
    List.

%%------------------------------------------------------------------------------
%% @spec (URL) -> #lhttpc_url{}
%%   URL = string()
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec parse_url(string()) -> #lhttpc_url{}.
parse_url(URL) ->
    % XXX This should be possible to do with the re module?
    {Scheme, CredsHostPortPath} = split_scheme(URL),
    {User, Passwd, HostPortPath} = split_credentials(CredsHostPortPath),
    {Host, PortPath} = split_host(HostPortPath, []),
    {Port, Path} = split_port(Scheme, PortPath, []),
    #lhttpc_url{host = lhttpc_lib:to_lower(Host), port = Port, path = Path,
                user = User, password = Passwd, is_ssl = (Scheme =:= https)}.

%%------------------------------------------------------------------------------
%% @spec (Path, Method, Headers, Host, Port, Body, Cookies) ->
%%    Request
%% Path = iolist()
%% Method = atom() | string()
%% Headers = [{atom() | string(), string()}]
%% Host = string()
%% Body = iolist()
%% Cookies = [#lhttpc_cookie{}]
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec format_request(iolist(), method(), headers(), string(),  iolist(),
                     {boolean(), [#lhttpc_cookie{}]}) -> {boolean(), iolist()}.
format_request(Path, Method, Hdrs, Host, Body, Cookies) ->
    AllHdrs = add_mandatory_hdrs(Path, Method, Hdrs, Host, Body, Cookies),
    Formatted = [[Header, <<": ">>, Value, ?HTTP_LINE_END]
		 || {Header, Value} <- AllHdrs],
    [Method, <<" ">>, Path, <<" HTTP/1.1">>, ?HTTP_LINE_END, Formatted,
     ?HTTP_LINE_END, Body].

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec dec(timeout()) -> timeout().
dec(Num) when is_integer(Num) ->
    Num - 1;
dec(Else) ->
    Else.

%%------------------------------------------------------------------------------
%% @private
%% @doc Updated the state of the cookies. after we receive a response.
%% @end
%%------------------------------------------------------------------------------
-spec update_cookies(headers(), [#lhttpc_cookie{}]) -> [#lhttpc_cookie{}].
update_cookies([], []) ->
    [];
update_cookies([], StateCookies) ->
    delete_expired_cookies(StateCookies);
update_cookies(ReceivedCookies, []) ->
    delete_expired_cookies(ReceivedCookies);
update_cookies(ReceivedCookies, StateCookies) ->
    %% substitute the cookies with the same name, add the others, delete.
    Substituted =
    	lists:foldl(fun(X = #lhttpc_cookie{value = <<"deleted">>}, Acc) ->
			    lists:keydelete(X#lhttpc_cookie.name,
					    #lhttpc_cookie.name, Acc);
		       (X, Acc) ->
    			    lists:keystore(X#lhttpc_cookie.name,
					   #lhttpc_cookie.name, Acc, X)
    		    end, StateCookies, ReceivedCookies),
    %% Delete the cookies that are expired (check max-age and expire fields).
    delete_expired_cookies(Substituted).


%%------------------------------------------------------------------------------
%% @doc Converts characters in a string ro lower case.
%% @end
%%------------------------------------------------------------------------------
-spec to_lower(string()) -> string().
to_lower(String) ->
    [char_to_lower(X) || X <- String].

%%------------------------------------------------------------------------------
%% @doc Compares header values to pre-defined values
%% Faster than string:to_lower and then compare
%% @end
%%------------------------------------------------------------------------------
is_close(<<"close">>) ->
    true;
is_close(<<"Close">>) ->
    true;
is_close(<<"keep-alive">>) ->
    false;
is_close(<<"Keep-Alive">>) ->
    false;
is_close(C) ->
    is_close(C, "close").

is_close(<<C, Rest1/bits>>, [C | Rest2]) ->
    is_close(Rest1, Rest2);
is_close(<<C1, Rest1/bits>>, [C2 | Rest2]) ->
    case close_to_lower(C1) == C2 of
	true ->
	    is_close(Rest1, Rest2);
	false ->
	    false
    end;
is_close(<<>>, _) ->
    false;
is_close(_, []) ->
    false.

close_to_lower($C) ->
    $c;
close_to_lower($L) ->
    $l;
close_to_lower($O) ->
    $o;
close_to_lower($S) ->
    $s;
close_to_lower($E) ->
    $e;
close_to_lower(C) ->
    C.

is_keep_alive(<<"close">>) ->
    false;
is_keep_alive(<<"Close">>) ->
    false;
is_keep_alive(<<"keep-alive">>) ->
    true;
is_keep_alive(<<"Keep-Alive">>) ->
    true;
is_keep_alive(C) ->
    is_keep_alive(C, "keep-alive").

is_keep_alive(<<C, Rest1/bits>>, [C | Rest2]) ->
    is_keep_alive(Rest1, Rest2);
is_keep_alive(<<C1, Rest1/bits>>, [C2 | Rest2]) ->
    case keep_to_lower(C1) == C2 of
	true ->
	    is_keep_alive(Rest1, Rest2);
	false ->
	    false
    end;
is_keep_alive(<<>>, _) ->
    false;
is_keep_alive(_, []) ->
    false.

keep_to_lower($K) ->
    $k;
keep_to_lower($E) ->
    $e;
keep_to_lower($P) ->
    $p;
keep_to_lower($A) ->
    $a;
keep_to_lower($L) ->
    $l;
keep_to_lower($I) ->
    $i;
keep_to_lower($V) ->
    $v;
keep_to_lower(C) ->
    C.

%%------------------------------------------------------------------------------
%% @doc Gets value from tuple list
%% @end
%%------------------------------------------------------------------------------
-spec get_value(Key, List) -> term() when
      Key :: term(),
      List :: [term()].
get_value(Key, List) ->
    case lists:keyfind(Key, 1, List) of
	{Key, Value} ->
	    Value;
	false ->
	    undefined
    end.

%%------------------------------------------------------------------------------
%% @doc Gets value from tuple list. If it is not present, returns default value.
%% @end
%%------------------------------------------------------------------------------
-spec get_value(Key, List, Default) -> term() when
      Key :: term(),
      List :: [term()],
      Default :: term().
get_value(Key, List, Default) ->
    case lists:keyfind(Key, 1, List) of
	{Key, Value} ->
	    Value;
	false ->
	    Default
    end.

%%==============================================================================
%% Internal functions
%%==============================================================================

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
-spec delete_expired_cookies([#lhttpc_cookie{}]) -> [#lhttpc_cookie{}].
delete_expired_cookies([]) ->
    [];
delete_expired_cookies(Cookies) ->
    [ X || X <- Cookies, not expires(X)].

expires(#lhttpc_cookie{value = <<"deleted">>}) ->
    true;
expires(#lhttpc_cookie{max_age = Max,
		       timestamp = T}) when Max =/= undefined ->
    timer:now_diff(os:timestamp(), T) > Max;
expires(#lhttpc_cookie{expires = Exp}) when Exp =/= undefined ->
    calendar:universal_time() > Exp;
expires(_) ->
    false.

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
split_scheme("http://" ++ HostPortPath) ->
    {http, HostPortPath};
split_scheme("https://" ++ HostPortPath) ->
    {https, HostPortPath}.

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
split_credentials(CredsHostPortPath) ->
    case string:tokens(CredsHostPortPath, "@") of
        [HostPortPath] ->
            {"", "", HostPortPath};
        [Creds, HostPortPath] ->
            % RFC1738 (section 3.1) says:
            % "The user name (and password), if present, are followed by a
            % commercial at-sign "@". Within the user and password field, any ":",
            % "@", or "/" must be encoded."
            % The mentioned encoding is the "percent" encoding.
            case string:tokens(Creds, ":") of
                [User] ->
                    % RFC1738 says ":password" is optional
                    {http_uri:decode(User), "", HostPortPath};
                [User, Passwd] ->
                    {http_uri:decode(User), http_uri:decode(Passwd), HostPortPath}
            end
    end.

%%------------------------------------------------------------------------------
%% @private
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec split_host(string(), string()) -> {string(), string()}.
split_host("[" ++ Rest, []) ->
    % IPv6 address literals are enclosed by square brackets (RFC2732)
    case string:str(Rest, "]") of
        0 ->
            split_host(Rest, "[");
        N ->
            {IPv6Address, "]" ++ PortPath0} = lists:split(N - 1, Rest),
            case PortPath0 of
                ":" ++ PortPath ->
                    {IPv6Address, PortPath};
                _ ->
                    {IPv6Address, PortPath0}
            end
    end;
split_host([$: | PortPath], Host) ->
    {lists:reverse(Host), PortPath};
split_host([$/ | _] = PortPath, Host) ->
    {lists:reverse(Host), PortPath};
split_host([$? | _] = Query, Host) ->
    %% The query string follows the hostname, without a slash.  The
    %% path is empty, but for HTTP an empty path is equivalent to "/"
    %% (RFC 3986, section 6.2.3), so let's add the slash ourselves.
    {lists:reverse(Host), "/" ++ Query};
split_host([H | T], Host) ->
    split_host(T, [H | Host]);
split_host([], Host) ->
    {lists:reverse(Host), []}.

%%------------------------------------------------------------------------------
%% @private
%% @doc
%% @end
%%------------------------------------------------------------------------------
split_port(http, [$/ | _] = Path, []) ->
    {80, Path};
split_port(https, [$/ | _] = Path, []) ->
    {443, Path};
split_port(http, [], []) ->
    {80, "/"};
split_port(https, [], []) ->
    {443, "/"};
split_port(_, [], Port) ->
    {list_to_integer(lists:reverse(Port)), "/"};
split_port(_,[$/ | _] = Path, Port) ->
    {list_to_integer(lists:reverse(Port)), Path};
split_port(Scheme, [P | T], Port) ->
    split_port(Scheme, T, [P | Port]).

%%------------------------------------------------------------------------------
%% @private
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec add_mandatory_hdrs(string(), method(), headers(), host(),
                         iolist(), {boolean(), [#lhttpc_cookie{}]}) -> headers().
add_mandatory_hdrs(Path, Method, Hdrs, Host, Body, {UseCookies, Cookies}) ->
    ContentHdrs = add_content_headers(Method, Hdrs, Body),
    case UseCookies of
        true ->
            % only include cookies if the cookie path is a prefix of the request path
            % see RFC http://www.ietf.org/rfc/rfc2109.txt section 4.3.4
            IncludeCookies = lists:filter(
                                fun(#lhttpc_cookie{path = undefined}) ->
                                       true;
                                   (X) ->
                                       IsPrefix = string:str(Path, X#lhttpc_cookie.path),
                                       if (IsPrefix =/= 1) ->
                                           false;
                                       true ->
                                           true
                                      end
                               end, Cookies),
            FinalHdrs = add_cookie_headers(ContentHdrs, IncludeCookies);
        _ ->
            FinalHdrs = ContentHdrs
    end,
    add_host(FinalHdrs, Host).

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
add_cookie_headers(Hdrs, []) ->
    Hdrs;
add_cookie_headers(Hdrs, Cookies) ->
    CookieString = make_cookie_string(Cookies, []),
    [{<<"Cookie">>, CookieString} | Hdrs].

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
make_cookie_string([], Acc) ->
    Acc;
make_cookie_string([Cookie | Rest], []) ->
    make_cookie_string(Rest, cookie_string(Cookie));
make_cookie_string([Cookie | Rest], Acc) ->
    make_cookie_string(Rest,  [cookie_string(Cookie), "; " | Acc]).

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
cookie_string(#lhttpc_cookie{name = Name, value = Value}) ->
    [Name, <<"=">>, Value].

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
-spec add_content_headers(string(), headers(), iolist()) -> headers().
add_content_headers("POST", Hdrs, Body) ->
    add_content_headers(Hdrs, Body);
add_content_headers("PUT", Hdrs, Body) ->
    add_content_headers(Hdrs, Body);
add_content_headers(_, Hdrs, _) ->
    Hdrs.

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
-spec add_content_headers(headers(), iolist()) -> headers().
add_content_headers(Hdrs, Body) ->
    case header_value(<<"content-length">>, Hdrs) of
        undefined ->
            ContentLength = integer_to_list(iolist_size(Body)),
            [{<<"Content-Length">>, ContentLength} | Hdrs];
        _ -> % We have a content length
            Hdrs
    end.

%%------------------------------------------------------------------------------
%% @private
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec add_host(headers(), host()) -> headers().
add_host(Hdrs, Host) ->
    case header_value(<<"host">>, Hdrs) of
        undefined ->
            [{<<"Host">>, Host} | Hdrs];
        _ -> % We have a host
            Hdrs
    end.

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec host_header(host(), port_num()) -> any().
host_header(Host, 80)   -> maybe_ipv6_enclose(Host);
% When proxying after an HTTP CONNECT session is established, squid doesn't
% like the :443 suffix in the Host header.
host_header(Host, 443)  -> maybe_ipv6_enclose(Host);
host_header(Host, Port) -> [maybe_ipv6_enclose(Host), $:, integer_to_list(Port)].

%%------------------------------------------------------------------------------
%% @private
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec maybe_ipv6_enclose(host()) -> host().
maybe_ipv6_enclose(Host) ->
    case inet_parse:address(Host) of
        {ok, {_, _, _, _, _, _, _, _}} ->
            % IPv6 address literals are enclosed by square brackets (RFC2732)
            [$[, Host, $]];
        _ ->
            Host
    end.

%%------------------------------------------------------------------------------
%% @private
%% @doc
%% @end
%%------------------------------------------------------------------------------
char_to_lower($A) -> $a;
char_to_lower($B) -> $b;
char_to_lower($C) -> $c;
char_to_lower($D) -> $d;
char_to_lower($E) -> $e;
char_to_lower($F) -> $f;
char_to_lower($G) -> $g;
char_to_lower($H) -> $h;
char_to_lower($I) -> $i;
char_to_lower($J) -> $j;
char_to_lower($K) -> $k;
char_to_lower($L) -> $l;
char_to_lower($M) -> $m;
char_to_lower($N) -> $n;
char_to_lower($O) -> $o;
char_to_lower($P) -> $p;
char_to_lower($Q) -> $q;
char_to_lower($R) -> $r;
char_to_lower($S) -> $s;
char_to_lower($T) -> $t;
char_to_lower($U) -> $u;
char_to_lower($V) -> $v;
char_to_lower($W) -> $w;
char_to_lower($X) -> $x;
char_to_lower($Y) -> $y;
char_to_lower($Z) -> $z;
char_to_lower(Ch) -> Ch.
