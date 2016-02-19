% vim: sw=4 ts=4 et ft=erlang
% Nitrogen Web Framework for Erlang
% Copyright (c) 2008-2013, Rusty Klophaus
% Copyright (c) 2016 Jesse Gumm
% See MIT-LICENSE for licensing information.

-module (validator_is_number).
-include("wf.hrl").
-export([
    render_action/1
]).

render_action(Record) ->
    TriggerPath = Record#is_number.trigger,
    TargetPath = Record#is_number.target,
    Text = wf:js_escape(Record#is_number.text),
    Min = Record#is_number.min,
    Max = Record#is_number.max,
    AllowBlank = Record#is_number.allow_blank,

    ValidationFun = fun(_, Value) -> validate(AllowBlank, Value, Min, Max) end,

    CustomValidatorAction =  #custom {
        trigger=TriggerPath,
        target=TargetPath,
        function=ValidationFun,
        text = Text,
        tag=Record,
        attach_to=Record#is_number.attach_to
    },

    Script = wf:f("v.add(Validate.Numericality, { notANumberMessage: \"~ts\", onlyInteger: false });", [Text]),
    [CustomValidatorAction, Script].

validate(_AllowBlank=true, "", _Min, _Max) ->
    true;
validate(_AllowBlank, Value, Min, Max) ->
    try
        validate_range(wf:to_float(Value), Min, Max)
    catch 
        _ : _ -> false
    end.

validate_range(_Int, undefined, undefined) ->
    true;
validate_range(Int, Min, undefined) ->
    Int >= Min;
validate_range(Int, undefined, Max) ->
    Int =< Max;
validate_range(Int, Min, Max) ->
    Int >= Min andalso Int =< Max.
