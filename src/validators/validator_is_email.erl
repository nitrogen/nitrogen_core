% vim: sw=4 ts=4 et ft=erlang
% Nitrogen Web Framework for Erlang
% Copyright (c) 2008-2010 Rusty Klophaus
% See MIT-LICENSE for licensing information.

-module (validator_is_email).
-include("wf.hrl").
-export([
        render_action/1,
        validate/2
    ]).

-spec render_action(#is_email{}) -> script().
render_action(Record)  ->
    TriggerPath= Record#is_email.trigger,
    TargetPath = Record#is_email.target,
    Text = wf:js_escape(Record#is_email.text),
    validator_custom:render_action(#custom { 
        trigger=TriggerPath, 
        target=TargetPath, 
        function=fun validate/2,
        text=Text,
        tag=Record,
        attach_to=Record#is_email.attach_to
    }),
    validation_handler:js_add_validator(TargetPath, email, Text).

-spec validate(any(), iolist()) -> boolean().
validate(_, Value) ->
    case re:run(wf:to_list(Value), "^[a-zA-Z0-9\\._%+-]+@[a-zA-Z0-9\\.-]+\\.[a-zA-Z]+$", [unicode]) of
        {match, _} -> true;
        _ -> false
    end.
