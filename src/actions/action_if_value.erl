% vim: sw=4 ts=4 et ft=erlang
% Nitrogen Web Framework for Erlang
% Copyright (c) 2018 Jesse Gumm
% See MIT-LICENSE for licensing information.

-module (action_if_value).
-include("wf.hrl").
-export([
	render_action/1
]).

render_action(#if_value{target=Target, map=Map, value=undefined, 'else'=Else}) ->
    [
        wf:f(<<"switch(objs('~s').val()) {">>, [Target]),
            lists:map(fun({Value0, Actions}) ->
                Value = wf:js_escape(wf:to_list(Value0)),
                [
                    wf:f(<<"case('~s'): ">>,[Value]),
                    Actions,
                    <<";break;">>
                ]
            end, Map),
            <<"default: ">>,
            Else,
        <<"}">>
    ];

render_action(#if_value{target=Target, map=undefined, value=Value0, actions=Actions, 'else'=Else}) ->
    Value = wf:js_escape(wf:to_list(Value0)),
    [
        wf:f(<<"if(objs('~s').val()=='~s') {">>, [Target, Value]),
            wf:f("console.log('~s');",[Value]),
            Actions,
        <<"}else{">>,
            Else,
        <<"}">>
    ].
