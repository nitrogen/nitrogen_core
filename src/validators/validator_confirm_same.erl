% vim: sw=4 ts=4 et ft=erlang
% Nitrogen Web Framework for Erlang
% Copyright (c) 2008-2010 Rusty Klophaus
% See MIT-LICENSE for licensing information.

-module (validator_confirm_same).
-include_lib ("wf.hrl").
-export([
    render_action/1,
    validate/2
]).

render_action(Record)  ->
    TriggerPath= Record#confirm_same.trigger,
    TargetPath = Record#confirm_same.target,
    Text = wf:js_escape(Record#confirm_same.text),
    ConfirmID = Record#confirm_same.confirm_id,

    validator_custom:render_action(#custom{
        trigger=TriggerPath,
        target=TargetPath,
        function=fun validate/2,
        text=Text,
        tag=Record,
        attach_to=Record#confirm_same.attach_to
    }),

    JSFunction = wf:f("function(value, args) { return (value == obj('~s').value); }", [ConfirmID]),

    validator_js_custom:render_action(#js_custom{
        trigger=TriggerPath,
        target=TargetPath,
        function=JSFunction,
        text=Text,
        attach_to=Record#confirm_same.attach_to
    }).

validate(Record, Value) ->
    Password = wf:q(Record#confirm_same.confirm_id),
    Value == Password.
