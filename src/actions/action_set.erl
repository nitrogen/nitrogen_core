% vim: sw=4 ts=4 et ft=erlang
% Nitrogen Web Framework for Erlang
% Copyright (c) 2008-2010 Rusty Klophaus
% See MIT-LICENSE for licensing information.

-module (action_set).
-include_lib ("wf.hrl").
-export([
	render_action/1,
	set/2,
	set/3
]).

render_action(Record) ->
    Anchor = Record#set.anchor,
    Target = Record#set.target,
    Value = wf:js_escape(wf:to_list(Record#set.value)),
    wf:f("Nitrogen.$set_value('~s', '~s', \"~s\");", [Anchor, Target, Value]).

set(Element, Value) ->
	set(normal, Element, Value).

set(Priority, Element, Value) when ?IS_ACTION_PRIORITY(Priority) ->
    wf:priority_wire(Priority, Element, #set { value=Value }).
