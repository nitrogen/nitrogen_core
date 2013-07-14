% vim: sw=4 ts=4 et ft=erlang
% Nitrogen Web Framework for Erlang
% Copyright (c) 2008-2013 Rusty Klophaus
% See MIT-LICENSE for licensing information.

-module (element_sparkline).
-include("wf.hrl").
-export([
    reflect/0,
    render_element/1
]).

%% @doc
%% Usage: #sparkline { values=[1, 2, 5, 6, 10, 9] }
%% http://willarson.com/code/sparklines/sparklines.html
%% http://omnipotent.net/jquery.sparkline/

reflect() -> record_info(fields, sparkline).

render_element(Record) -> 
    Anchor = Record#sparkline.anchor,
    Values = Record#sparkline.values,
    ValuesS = ["[",wf:join([wf:to_list(X) || X <- Values], ","),"]"],
    OptionsS = options_to_js([{type, Record#sparkline.type}|Record#sparkline.options]),
    wf:wire(#script{
        dependency_js = <<"/nitrogen/jquery.sparkline.js">>,
        script=wf:f(<<"objs('~s').sparkline(~s, ~s);">>, [Anchor, ValuesS, OptionsS])
    }),
    Span = #span {
        html_id = Record#sparkline.html_id,
        id = Record#sparkline.id,
        anchor = Record#sparkline.anchor,
        class = [sparkline, Record#sparkline.class],
        style = Record#sparkline.style
    },
    element_span:render_element(Span).

%% Options is a list of {Key,Value} tuples  
options_to_js(Options) ->
    Options1 = [format_option(X) || X <- Options],
    Options2 = wf:join(Options1, ","),
    wf:f(<<"{ ~s }">>, [Options2]).


format_option({Key, Value}) when is_atom(Value) ->
    format_option({Key, wf:to_list(Value)});
format_option({Key, Value}) when is_binary(Value) ->
    format_option({Key, wf:to_list(Value)});
format_option({Key, Value}) when ?IS_STRING(Value) ->
    wf:f(<<"~s: '~s'">>, [Key, wf:js_escape(Value)]);
format_option({Key, Value}) when Value=:=true, Value=:=false ->
    wf:f(<<"~s: ~s">>, [Key, Value]);
format_option({Key, Value}) ->
    wf:f(<<"~s: ~p">>, [Key, Value]);
format_option(V) ->
    throw({invalid_sparkline_option, V}).
