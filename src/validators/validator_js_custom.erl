% vim: sw=4 ts=4 et ft=erlang
% Nitrogen Web Framework for Erlang
% Copyright (c) 2008-2010 Rusty Klophaus
% See MIT-LICENSE for licensing information.

-module (validator_js_custom).
-include("wf.hrl").
-compile(export_all).

render_action(Record) -> 
    Text = wf:js_escape(Record#js_custom.text),
    Function = Record#js_custom.function,
    Args = Record#js_custom.args,
    WhenEmpty = Record#js_custom.when_empty,
    wf:f("v.add(Validate.Custom, { against: ~s, args: ~ts, failureMessage: \"~ts\", displayMessageWhenEmpty: ~ts });", [Function, Args, Text, WhenEmpty]).
