%% vim: ts=4 sw=4 et
-module(debug_crash_handler).
-behaviour(crash_handler).
-include("wf.hrl").
-export([
    init/2,
    finish/2,
    first_request/5,
    postback_request/5
]).

init(_Config,State) ->
    {ok, State}.

finish(_Config, State) ->
    {ok, State}.

first_request(Type, Error, Stacktrace, _Config, _State) ->
    Uri = wf:header(host) ++ wf:uri(),
    ?LOG("~p~n", [{error, first_request, {url, Uri}, {Type, Error, Stacktrace}}]),
    wf:status_code(500),
    %% We use raw HTML here instead of Nitrogen elements in case the error is
    %% internal to nitrogen (like if the element rendering system is broken).
    [
        <<"<h2>&#9888; There was an error processing this page &#9888;</h2>">>,
        <<"<code><pre>">>,format_error(Type, Error, Stacktrace),<<"</pre></code>">>
    ].

postback_request(Type, Error, Stacktrace, _Config, _State) ->
    Uri = wf:header(host) ++ wf:uri(),
    ?LOG("~p~n", [{error, postback_request, {url, Uri}, {Type, Error, Stacktrace}}]),
    ErrorMsg = <<"&#9888; An error occured performing this action. See console for details. &#9888;">>,
    CloseBtn = <<"<a style='float:right; color: #f00' href='javascript:' onclick='Nitrogen.$hide_notice_bar()'>[&times;]</a>">>,
    wf:wire([<<"Nitrogen.$show_notice_bar('error', \"">>, ErrorMsg , CloseBtn, <<"\")">>]),
    wf:console_log(["Error in postback:\n",format_error(Type, Error, Stacktrace)]).

format_error(Type, Error, Stacktrace) ->
    wf:f(<<"~p:~p
--------------------------
~p">>, [Type, Error, Stacktrace]).
