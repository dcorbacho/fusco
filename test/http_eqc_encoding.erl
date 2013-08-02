%%%=============================================================================
%%% @copyright (C) 1999-2013, Erlang Solutions Ltd
%%% @author Diana Corbacho <diana.corbacho@erlang-solutions.com>
%%% @doc
%%% @end
%%%=============================================================================
-module(http_eqc_encoding).
-copyright("2013, Erlang Solutions Ltd.").

-export([add_content_length/2,
	 add_transfer_encoding/2,
	 body/1]).

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
    
remove_transfer_encoding(Headers) ->
    lists:filter(fun({H, _}) -> H =/= <<"Transfer-Encoding">> end, Headers).
    
