% vim: sw=4 ts=4 et ft=erlang
% Nitrogen Web Framework for Erlang
% Copyright (c) 2014 Jesse Gumm
% See MIT-LICENSE for licensing information.

-module (action_set_multiple).
-include("wf.hrl").
-export([
	render_action/1,
	set/2,
	set/3
]).

render_action(#set_multiple{anchor=Anchor, target=Target, values=Values0}) ->
    Values = format_values(Values0),
    wf:f(<<"Nitrogen.$set_values('~s', '~s', [~ts]);">>, [Anchor, Target, Values]).

format_values(Vs) ->
    wf:join([format_value(V) || V <- Vs], <<", ">>).

format_value(V) ->
    wf:f("'~ts'", [wf:js_escape(wf:to_list(V))]).

set(Element, Values) when is_list(Values) ->
	set(normal, Element, Values).

set(Priority, Element, Values) when is_list(Values), ?IS_ACTION_PRIORITY(Priority) ->
    wf:priority_wire(Priority, Element, #set_multiple { values=Values }).
