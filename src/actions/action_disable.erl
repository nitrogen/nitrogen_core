% vim: sw=4 ts=4 et ft=erlang
% Nitrogen Web Framework for Erlang
% Copyright (c) 2012 Milan Svoboda
% See MIT-LICENSE for licensing information.

-module (action_disable).
-include("wf.hrl").
-export([
    render_action/1,
    disable/1,
    disable/2
]).

-spec render_action(#disable{}) -> actions().
render_action(#disable{target=Target}) ->
	#script{script=wf:f(<<"obj('~s').disabled = true;">>, [Target])}.

-spec disable(Target :: id()) -> ok.
disable(Target) ->
    disable(normal, Target).

-spec disable(Priority :: wire_priority(), Target :: id()) -> ok.
disable(Priority, Target) ->
    wf:priority_wire(Priority, Target, #disable{}).
