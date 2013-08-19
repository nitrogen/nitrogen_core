% vim: sw=4 ts=4 et ft=erlang
% Nitrogen Web Framework for Erlang
% Copyright (c) 2012 Milan Svoboda
% See MIT-LICENSE for licensing information.

-module (action_enable).
-include("wf.hrl").
-export([
    render_action/1,
    enable/1,
    enable/2
]).

-spec render_action(#enable{}) -> actions().
render_action(#enable{target=Target}) ->
	#script{script=wf:f(<<"obj('~s').disabled = false;">>, [Target])}.

-spec enable(Target :: id()) -> ok.
enable(Target) ->
    enable(normal, Target).

-spec enable(Priority :: wire_priority(), Target :: id()) -> ok.
enable(Priority, Target) ->
    wf:priority_wire(Priority, Target, #enable{}).
