% vim: sw=4 ts=4 et ft=erlang
% Nitrogen Web Framework for Erlang
% Copyright (c) 2018-2025 Jesse Gumm
% See MIT-LICENSE for licensing information.

-module(action_if_checked).
-include("wf.hrl").
-export([
	render_action/1
]).

render_action(#if_checked{target=Target, actions=Actions, 'else'=Else}) ->
    [
        <<"(function() { ">>,
            wf:f(<<"if(Nitrogen.$is_checked('~s')) {">>, [Target]),
                Actions,
            <<"}else{">>,
                Else,
            <<"}">>,
        <<"})()">>
    ].
