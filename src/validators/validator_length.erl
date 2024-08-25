% vim: sw=4 ts=4 et ft=erlang
% Nitrogen Web Framework for Erlang
% Copyright (c) 2008-2010 Rusty Klophaus
% Contributions from Torbjorn Tornkvist (tobbe@tornkvist.org)
% See MIT-LICENSE for licensing information.

-module (validator_length).
-include("wf.hrl").
-export([render_action/1, validate/2]).

render_action(Record = #min_length{}) ->
    %% This is just a hack for the sake of convenience.
    Rec2 = setelement(1, Record, max_length),
    render_action(min, Rec2);
render_action(Record = #max_length{}) ->
    render_action(max, Record).

render_action(Type, Record)  ->
    TriggerPath = Record#max_length.trigger,
    TargetPath = Record#max_length.target,
    Text = wf:js_escape(Record#max_length.text),
    Length = wf:to_integer(Record#max_length.length, 0),
    CustomValidatorAction = #custom{
        trigger=TriggerPath,
        target=TargetPath,
        function=fun ?MODULE:validate/2,
        text=Text,
        tag={Type, Record},
        attach_to=Record#max_length.attach_to
    },
    
    Opts = #{Type=>Length},
    ValidatorType = ?WF_IF(Type==min, min_length, max_length),
    [
        validator_custom:render_action(CustomValidatorAction),
        validation_handler:js_add_validator(TargetPath, ValidatorType, Text, Opts)
    ].

validate({max, Record}, Value) ->
    wf:to_float(Record#max_length.length, 0) >= length(Value);
validate({min, Record}, Value) ->
    wf:to_float(Record#max_length.length, 0) =< length(Value).
