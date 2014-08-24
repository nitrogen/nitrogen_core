% vim: sw=4 ts=4 et ft=erlang
% Nitrogen Web Framework for Erlang
% Copyright (c) 2008-2010 Rusty Klophaus
% See MIT-LICENSE for licensing information.

-module (wf_convert).
-export ([
    clean_lower/1,
    to_list/1,
    to_atom/1,
    to_existing_atom/1,
    to_binary/1,
    to_integer/1,
    to_string_list/1,
    to_qs/1,
    encode/2, decode/2,
    html_encode/1, html_encode/2,
    html_decode/1,
    hex_encode/1, hex_decode/1,
    url_encode/1, url_decode/1,
    js_escape/1,
	join/2,
    parse_ip/1
]).

-include_lib ("wf.hrl").

%%% CONVERSION %%%

clean_lower(L) -> string:strip(string:to_lower(to_list(L))).

-spec to_list(term()) -> list().
to_list(undefined) -> [];
to_list(L) when ?IS_STRING(L) -> L;
to_list(L) when is_list(L) ->
    SubLists = [inner_to_list(X) || X <- L],
    lists:flatten(SubLists);
to_list(A) -> inner_to_list(A).

-spec inner_to_list(term()) -> list().
inner_to_list(A) when is_atom(A) -> atom_to_list(A);
inner_to_list(B) when is_binary(B) -> binary_to_list(B);
inner_to_list(I) when is_integer(I) -> integer_to_list(I);
inner_to_list(F) when is_float(F) -> 
	case F == round(F) of
		true -> inner_to_list(round(F));
		false -> nitro_mochinum:digits(F)
	end;
inner_to_list(L) when is_list(L) -> L.

-spec to_atom(term()) -> atom().
to_atom(A) when is_atom(A) -> A;
to_atom(B) when is_binary(B) -> to_atom(binary_to_list(B));
to_atom(I) when is_integer(I) -> to_atom(integer_to_list(I));
to_atom(F) when is_float(F) -> to_atom(nitro_mochinum:digits(F));
to_atom(L) when is_list(L) -> list_to_atom(binary_to_list(iolist_to_binary(L))).

to_existing_atom(A) when is_atom(A) -> A;
to_existing_atom(B) when is_binary(B) -> to_existing_atom(binary_to_list(B));
to_existing_atom(I) when is_integer(I) -> to_existing_atom(integer_to_list(I));
to_existing_atom(L) when is_list(L) -> list_to_existing_atom(binary_to_list(iolist_to_binary(L))).

-spec to_binary(term()) -> binary().
to_binary(A) when is_atom(A) -> to_binary(atom_to_list(A));
to_binary(B) when is_binary(B) -> B;
to_binary(I) when is_integer(I) -> to_binary(integer_to_list(I));
to_binary(F) when is_float(F) -> to_binary(nitro_mochinum:digits(F));
to_binary(L) when is_list(L) -> iolist_to_binary(L).

-spec to_integer(term()) -> integer().
to_integer(A) when is_atom(A) -> to_integer(atom_to_list(A));
to_integer(B) when is_binary(B) -> to_integer(binary_to_list(B));
to_integer(I) when is_integer(I) -> I;
to_integer(L) when is_list(L) -> list_to_integer(L);
to_integer(F) when is_float(F) -> round(F).

%%% TO STRING LIST %%%

%% @doc
%% Convert the following forms into a list of strings...
%% 	- atom
%%  - [atom, atom, ...]
%%  - "String"
%%  - "String, String, ..."
%%  - "String String ..."
%%  - [atom, "String", ...]
to_string_list(L) -> to_string_list(L, []).
to_string_list([], Acc) -> Acc;
to_string_list(undefined, Acc) -> Acc;
to_string_list(L, Acc) when is_atom(L) ->
    [atom_to_list(L)|Acc];
to_string_list(L, Acc) when ?IS_STRING(L) ->
    string:tokens(L, " ,") ++ Acc;
to_string_list(L, Acc) when is_binary(L) ->
    [binary_to_list(L)|Acc];
to_string_list([H|T], Acc) ->
    to_string_list(T, to_string_list(H) ++ Acc).


%%% HTML ENCODE %%%
-spec html_encode(L :: term()) -> iolist().
html_encode(L) -> 
    html_encode(L, normal).

-spec html_encode(L :: term(), EncType :: fun() | boolean() | whites | normal) -> iolist().
html_encode(L,EncType) when is_function(EncType) -> EncType(L);
html_encode(undefined, _) -> [];    %% treat "undefined" as special

html_encode(L,EncType) when is_atom(L) -> html_encode(atom_to_list(L),EncType);
html_encode(L,EncType) when is_integer(L) -> html_encode(integer_to_list(L),EncType);
html_encode(L,EncType) when is_float(L) -> html_encode(nitro_mochinum:digits(L),EncType);

html_encode(L, false) -> L;
html_encode(L, normal) when is_list(L); is_binary(L) -> ihe(L, normal);
html_encode(L, true) when is_list(L); is_binary(L) -> ihe(L, normal);
html_encode(L, whites) when is_list(L); is_binary(L) -> ihe(L, whites);

html_encode(Other, EncType) -> ihe(Other, EncType).

-spec ihe(binary() | list(), whites | normal) -> iolist().
%% @doc ihe means "inner html encode". It's a short version for encoding "ET"
%% will be "encoding type", which usually would be 'whites'
%%
%% TODO for Nitrogen 3.
%% This will remain backwards compatible for Nitrogen 2 releases, in Nitrogen
%% 3, This will very likely be converted to a full binary conversion, with
%% typespec being term() -> binary().
ihe([], _)                              -> [];
ihe(<<>>, _)                            -> <<>>;
ihe(A, ET) when is_atom(A)              -> html_encode(atom_to_list(A), ET);
ihe([Bin|T], ET) when is_binary(Bin)    -> [ihe(Bin, ET) | ihe(T, ET)];
ihe([$<|T], ET)                         -> "&lt;"    ++ ihe(T, ET);
ihe([$>|T], ET)                         -> "&gt;"    ++ ihe(T, ET);
ihe([$"|T], ET)                         -> "&quot;"  ++ ihe(T, ET);
ihe([$'|T], ET)                         -> "&#39;"   ++ ihe(T, ET);
ihe([$&|T], ET)                         -> "&amp;"   ++ ihe(T, ET);
ihe([$\s,$\s|T],whites)                 -> " &nbsp;" ++ ihe(T, whites);
ihe([$\t|T],whites)                     -> "&nbsp; &nbsp; &nbsp;" ++ ihe(T, whites);
ihe([$\n|T],whites)                     -> "<br>"    ++ ihe(T,whites);
ihe([BigNum|T], ET) when is_integer(BigNum)
                         andalso BigNum > 255
                                        -> [$&,$# | integer_to_list(BigNum)] ++ ";" ++ ihe(T, ET);
ihe(<<">", T/binary>>, ET)              -> <<"&gt;",   (ihe(T, ET))/binary>>;
ihe(<<"<", T/binary>>, ET)              -> <<"&lt;",   (ihe(T, ET))/binary>>;
ihe(<<"\"",T/binary>>, ET)              -> <<"&quot;", (ihe(T, ET))/binary>>;
ihe(<<"'", T/binary>>, ET)              -> <<"&#39;",  (ihe(T, ET))/binary>>;
ihe(<<"&", T/binary>>, ET)              -> <<"&amp;",  (ihe(T, ET))/binary>>;
ihe(<<"  ",T/binary>>, whites)          -> <<" &nbsp;",(ihe(T,whites))/binary>>;
ihe(<<"\t",T/binary>>, whites)          -> <<"&nbsp; &nbsp; &nbsp;",(ihe(T,whites))/binary>>;
ihe(<<"\n",T/binary>>, whites)          -> <<"<br>",   (ihe(T,whites))/binary>>;
ihe(<<H:8, T/binary>>, ET)              -> <<H,(ihe(T,ET))/binary>>;
ihe([H|T], ET)                          -> [ihe(H, ET)|ihe(T, ET)];
ihe(U, ET) when is_tuple(U);
                is_pid(U);
                is_reference(U);
                is_port(U)              -> ihe(io_lib:format("~p", [U]), ET);
ihe(Other, _ET)                         -> Other.

html_decode(B) when is_binary(B) -> html_decode(binary_to_list(B));
html_decode([]) -> [];
html_decode("&amp;" ++ T) -> [$&|html_decode(T)];
html_decode("&#39;" ++ T) -> [$'|html_decode(T)];
html_decode("&quot;" ++ T) -> [$"|html_decode(T)];
html_decode("&gt;" ++ T) -> [$>|html_decode(T)];
html_decode("&lt;" ++ T) -> [$<|html_decode(T)];
html_decode("&nbsp;" ++ T) -> [$\s|html_decode(T)];
html_decode([H|T]) -> [H|html_decode(T)].

%%% HEX ENCODE and HEX DECODE

hex_encode(Data) -> encode(Data, 16).
hex_decode(Data) -> decode(Data, 16).

encode(Data, Base) when is_binary(Data) -> encode(binary_to_list(Data), Base);
encode(Data, Base) when is_list(Data) ->
    F = fun(C) when is_integer(C) ->
        case erlang:integer_to_list(C, Base) of
            [C1, C2] -> <<C1, C2>>;
            [C1]     -> <<$0, C1>>;
            _        -> throw("Could not hex_encode the string.")
        end
    end,
    {ok, list_to_binary([F(I) || I <- Data])}.

decode(Data, Base) when is_binary(Data) -> decode(binary_to_list(Data), Base);
decode(Data, Base) when is_list(Data) ->
    {ok, list_to_binary(inner_decode(Data, Base))}.

inner_decode(Data, Base) when is_list(Data) ->
    case Data of
        [C1, C2|Rest] ->
            I = erlang:list_to_integer([C1, C2], Base),
            [I|inner_decode(Rest, Base)];

        [] ->
            [];

        _  ->
            throw("Could not hex_decode the string.")
    end.

%%% URL ENCODE/DECODE %%%

-spec url_encode(term()) -> string().
url_encode(S) -> quote_plus(S).

-spec url_decode(binary() | string()) -> string().
url_decode(S) -> unquote(S).

-spec to_qs(proplist()) -> list().
%% @doc Builds a safely-encoded querystring out of a proplist.
%% Example: build_qs([{a, something}, {b, 123}]),
%% Returns: "a=something&b=123" 
to_qs(undefined) -> []; 
to_qs([]) -> [];
to_qs([{Key, Val}]) ->
    [url_encode(Key),"=",url_encode(Val)];
to_qs([{Key, Val} | Rest]) ->
    [url_encode(Key),"=",url_encode(Val),"&" | to_qs(Rest)].

%%% ESCAPE JAVASCRIPT %%%

js_escape(undefined) -> [];
js_escape(Value) when is_list(Value) -> binary_to_list(js_escape(iolist_to_binary(Value)));
js_escape(Value) -> js_escape(Value, <<>>).
js_escape(<<"\\", Rest/binary>>, Acc) -> js_escape(Rest, <<Acc/binary, "\\\\">>);
js_escape(<<"\r", Rest/binary>>, Acc) -> js_escape(Rest, <<Acc/binary, "\\r">>);
js_escape(<<"\n", Rest/binary>>, Acc) -> js_escape(Rest, <<Acc/binary, "\\n">>);
js_escape(<<"\"", Rest/binary>>, Acc) -> js_escape(Rest, <<Acc/binary, "\\\"">>);
js_escape(<<"'",Rest/binary>>,Acc) -> js_escape(Rest, <<Acc/binary, "\\'">>);
js_escape(<<"<script", Rest/binary>>, Acc) -> js_escape(Rest, <<Acc/binary, "<scr\" + \"ipt">>);
js_escape(<<"script>", Rest/binary>>, Acc) -> js_escape(Rest, <<Acc/binary, "scr\" + \"ipt>">>);
js_escape(<<C, Rest/binary>>, Acc) -> js_escape(Rest, <<Acc/binary, C>>);
js_escape(<<>>, Acc) -> Acc.


%%% JOIN %%%
-spec join([term()], term()) -> [term()].
%% @doc Provides a simple way to join things with other things. Erlang's
%% string:join is not flexible enough for this. For example:
%% join([#span{}, #span{}], #br{}) -> [#span{}, #br{}, #span{}]
join([],_) ->
	[];
join([Item],_Delim) ->
	[Item];
join([Item|Items],Delim) ->
	[Item,Delim | join(Items,Delim)].

%%% CODE BELOW IS FROM MOCHIWEB %%%

%% This is the MIT license.
%%
%% Copyright (c) 2007 Mochi Media, Inc.
%%
%% Permission is hereby granted, free of charge, to any person
%% obtaining a copy of this software and associated documentation
%% files (the "Software"), to deal in the Software without
%% restriction, including without limitation the rights to use, copy,
%% modify, merge, publish, distribute, sublicense, and/or sell copies
%% of the Software, and to permit persons to whom the Software is
%% furnished to do so, subject to the following conditions:
%%
%% The above copyright notice and this permission notice shall be
%% included in all copies or substantial portions of the Software.
%%
%% THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
%% EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
%% MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
%% NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS
%% BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN
%% ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
%% CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
%% SOFTWARE.

-define(PERCENT, 37).  % $\%
-define(FULLSTOP, 46). % $\.
-define(IS_HEX(C), ((C >= $0 andalso C =< $9) orelse
    (C >= $a andalso C =< $f) orelse
    (C >= $A andalso C =< $F))).
-define(QS_SAFE(C), ((C >= $a andalso C =< $z) orelse
    (C >= $A andalso C =< $Z) orelse
    (C >= $0 andalso C =< $9) orelse
    (C =:= ?FULLSTOP orelse C =:= $- orelse C =:= $~ orelse
        C =:= $_))).


hexdigit(C) when C < 10 -> $0 + C;
hexdigit(C) when C < 16 -> $A + (C - 10).

unhexdigit(C) when C >= $0, C =< $9 -> C - $0;
unhexdigit(C) when C >= $a, C =< $f -> C - $a + 10;
unhexdigit(C) when C >= $A, C =< $F -> C - $A + 10.


quote_plus(Atom) when is_atom(Atom) ->
    quote_plus(atom_to_list(Atom));
quote_plus(Int) when is_integer(Int) ->
    quote_plus(integer_to_list(Int));
quote_plus(Bin) when is_binary(Bin) ->
    quote_plus(binary_to_list(Bin));
quote_plus(String) ->
    quote_plus(String, []).

quote_plus([], Acc) ->
    lists:reverse(Acc);
quote_plus([C | Rest], Acc) when ?QS_SAFE(C) ->
    quote_plus(Rest, [C | Acc]);
quote_plus([$\s | Rest], Acc) ->
    quote_plus(Rest, [$+ | Acc]);
quote_plus([C | Rest], Acc) ->
    <<Hi:4, Lo:4>> = <<C>>,
    quote_plus(Rest, [hexdigit(Lo), hexdigit(Hi), ?PERCENT | Acc]).

%% @spec unquote(string() | binary()) -> string()
%% @doc Unquote a URL encoded string.
unquote(Binary) when is_binary(Binary) ->
    unquote(binary_to_list(Binary));
unquote(String) ->
    qs_revdecode(lists:reverse(String)).

qs_revdecode(S) ->
    qs_revdecode(S, []).

qs_revdecode([], Acc) ->
    Acc;
qs_revdecode([$+ | Rest], Acc) ->
    qs_revdecode(Rest, [$\s | Acc]);
qs_revdecode([Lo, Hi, ?PERCENT | Rest], Acc) when ?IS_HEX(Lo), ?IS_HEX(Hi) ->
    qs_revdecode(Rest, [(unhexdigit(Lo) bor (unhexdigit(Hi) bsl 4)) | Acc]);
qs_revdecode([C | Rest], Acc) ->
    qs_revdecode(Rest, [C | Acc]).



parse_ip(IP = {_,_,_,_}) ->
    IP;
parse_ip(IP = {_,_,_,_,_,_,_,_}) ->
    IP;
parse_ip(Binary) when is_binary(Binary) ->
    parse_ip(binary_to_list(Binary));
parse_ip(String) ->
    case parse_address(String) of
        {ok, IP} -> IP;
        {error, _} -> undefined
    end.

%% This should just be inet:parse_address, but because it's so new, older
%% versions of erlang fail on it
parse_address(String) ->
    case parse_ipv4(String) of
        {error, einval} -> parse_ipv6(String);
        {ok, IP} -> {ok, IP}
    end.
   
parse_ipv4(String) ->
    try
        Parts = [_,_,_,_] = re:split(String,"\\.",[{return,list}]),
        IP = list_to_tuple([list_to_integer(Part) || Part <- Parts]),
        {ok, IP}
    catch
        _:_ -> {error, einval}
    end.

parse_ipv6(String) ->
    case parse_ipv6_split_shortened(String) of
        {ok, IP} -> {ok, IP};
        {error, einval} -> parse_ipv6_full(String)
    end.

parse_ipv6_full(String) ->
    try
        Parts = [_,_,_,_,_,_,_,_] = parse_ipv6_chunk_of_parts(String),
        IP = list_to_tuple(Parts),
        {ok, IP}
    catch
        _:_ -> {error, einval}
    end.

parse_ipv6_split_shortened(String) ->
    try
        [Front,Back] = re:split(String,"::",[{return ,list}]),
        ParsedFront = parse_ipv6_chunk_of_parts(Front),
        ParsedBack = parse_ipv6_chunk_of_parts(Back),
        NumZeroBlocks = 8 - length(ParsedFront) - length(ParsedBack),
        FinalIPList = ParsedFront ++ lists:duplicate(NumZeroBlocks, 0) ++ ParsedBack,
        {ok, list_to_tuple(FinalIPList)}
    catch
        _:_  -> {error, einval}
    end.

parse_ipv6_chunk_of_parts(String) ->
    Parts = re:split(String, ":", [{return,list}]),
    [parse_ipv6_part(Part) || Part <- Parts].

parse_ipv6_part("") -> 0;
parse_ipv6_part(List) ->
    parse_ipv6_digits(string:to_lower(lists:reverse(List)),1).

parse_ipv6_digits([], _) -> 0;
parse_ipv6_digits([H | T], Multiplier) ->
    Num = case H of
        $0 -> 0;
        $1 -> 1;
        $2 -> 2;
        $3 -> 3;
        $4 -> 4;
        $5 -> 5;
        $6 -> 6;
        $7 -> 7;
        $8 -> 8;
        $9 -> 9;
        $a -> 10;
        $b -> 11;
        $c -> 12;
        $d -> 13;
        $e -> 14;
        $f -> 15
    end,
    Num * Multiplier + parse_ipv6_digits(T, Multiplier*16).

-include_lib("eunit/include/eunit.hrl").

html_encode_test() ->
    %% Disabled with argument
    ?assertEqual("<i>",html_encode("<i>", false)),

    % Binaries
    ?assertEqual(<<"&lt;">>, html_encode(<<"<">>)),
    ?assertEqual(<<"&gt;">>, html_encode(<<">">>)),
    ?assertEqual(<<"&quot;">>, html_encode(<<"\"">>)),
    ?assertEqual(<<"&#39;">>, html_encode(<<"'">>)),
    ?assertEqual(<<"&amp;">>, html_encode(<<"&">>)),
    ?assertEqual(<<" ">>, html_encode(<<" ">>)),
    ?assertEqual(<<" &nbsp;">>, html_encode(<<"  ">>, whites)),
    ?assertEqual(<<"&nbsp; &nbsp; &nbsp;">>, html_encode(<<"\t">>, whites)),
    ?assertEqual(<<"\n">>, html_encode(<<"\n">>)),
    ?assertEqual(<<"<br>">>, html_encode(<<"\n">>,whites)),
    ?assertEqual(<<"&lt;i&gt;yo&lt;/i&gt;">>, html_encode(<<"<i>yo</i>">>)),

    % Strings
    ?assertEqual("&lt;", html_encode("<")),
    ?assertEqual("&gt;", html_encode(">")),
    ?assertEqual("&quot;", html_encode("\"")),
    ?assertEqual("&#39;", html_encode("'")),
    ?assertEqual("&amp;", html_encode("&")),
    ?assertEqual("&amp;amp;", html_encode("&amp;")),
    ?assertEqual(" ", html_encode(" ")),
    ?assertEqual(" &nbsp;", html_encode("  ", whites)),
    ?assertEqual(" &nbsp; ", html_encode("   ", whites)),
    ?assertEqual("&nbsp; &nbsp; &nbsp;", html_encode("\t", whites)),
    ?assertEqual("\n", html_encode("\n")),
    ?assertEqual("<br>", html_encode("\n",whites)),

    % Binary and String mixes and other random types

    ?assertEqual(<<"&amp;&amp;">>, iolist_to_binary(html_encode(["&", <<"&">>]))),
    ?assertEqual(<<"ab">>, iolist_to_binary(html_encode([a, <<"b">>]))),

    ?assertEqual("12345", html_encode(12345)),
    ?assertEqual("3.14", html_encode(3.14)),
    ?assertEqual("abc&#12345;", html_encode("abc" ++ [12345])),

    ?assertEqual("ok", html_encode(ok)),
    ?assertEqual("&lt;&gt;", html_encode('<>')),
    ?assertEqual(<<"A{}">>,iolist_to_binary(html_encode(["A",{}]))),
    ?assertEqual(<<"A{b,c,d}">>, iolist_to_binary(html_encode(["A",{b,c,d}]))),
    ?assertEqual(<<"{a}">>, iolist_to_binary(html_encode({a}))),
    ok.

to_qs_test() ->
    F = fun lists:flatten/1,
    ?assertEqual("a=a", F(to_qs([{a,a}]))),
    ?assertEqual("a=a&b=b", F(to_qs([{"a",a}, {b,<<"b">>}]))),
    ?assertEqual("a=a&b=b&c=1", F(to_qs([{a,a}, {b,b}, {"c", 1}]))),
    ?assertEqual("a=the+max&b=cho%27gall", F(to_qs([{a,"the max"}, {b,<<"cho'gall">>}]))),
    ?assertEqual("a=%25%26%23%3D%3F", F(to_qs([{"a", "%&#=?"}]))).
