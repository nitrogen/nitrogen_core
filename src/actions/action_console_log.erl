% vim: sw=4 ts=4 et ft=erlang
% Nitrogen Web Framework for Erlang
% Copyright (c) 2013 Jesse Gumm
% See MIT-LICENSE for licensing information.

-module (action_console_log).
-include("wf.hrl").
-export([
	render_action/1,
    console_log/1,
    console_log/2
]).

-spec render_action(Record :: tuple()) -> binary().
render_action(#console_log{text=Text}) ->
    Text2 = ?WF_IF(?IS_STRING(Text) orelse is_binary(Text),Text,wf:f("~ts",[Text])),
    JsText = wf:js_escape(wf:to_list(Text2)),
    wf:f(<<"Nitrogen.$console_log(\"~ts\");">>, [JsText]).

-spec console_log(Value :: string() | any()) -> ok.
console_log(Value) ->
    console_log(normal, Value).

-spec console_log(Priority :: wire_priority(), Value :: string() | any()) -> ok.
console_log(Priority, Text) when ?IS_ACTION_PRIORITY(Priority) ->
    wf:priority_wire(Priority, #console_log { text=Text }).
