% vim: sw=4 ts=4 et ft=erlang
% Nitrogen Web Framework for Erlang
% Copyright (c) 2008-2010 Rusty Klophaus
% See MIT-LICENSE for licensing information.

-module (action_set).
-include("wf.hrl").
-export([
	render_action/1,
	set/2,
	set/3
]).

render_action(#set{anchor=Anchor, target=Target, value=Value0}) ->
    Value = wf:js_escape(wf:to_list(Value0)),
    wf:f(<<"Nitrogen.$set_value('~s', '~s', \"~s\");">>, [Anchor, Target, Value]).

set(Element, Value) ->
	set(normal, Element, Value).

set(Priority, Element, Value) when ?IS_ACTION_PRIORITY(Priority) ->
    wf:priority_wire(Priority, Element, #set { value=Value }).
