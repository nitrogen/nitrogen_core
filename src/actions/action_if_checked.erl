% vim: sw=4 ts=4 et ft=erlang
% Nitrogen Web Framework for Erlang
% Copyright (c) 2018 Jesse Gumm
% See MIT-LICENSE for licensing information.

-module (action_if_checked).
-include("wf.hrl").
-export([
	render_action/1
]).

render_action(#if_checked{target=Target, actions=Actions, 'else'=Else}) ->
    [

        wf:f("console.log(obj('~s').checked);", [Target]),
        wf:f(<<"if(obj('~s').checked) {">>, [Target]),
            Actions,
        <<"}else{">>,
            Else,
        <<"}">>
    ].
