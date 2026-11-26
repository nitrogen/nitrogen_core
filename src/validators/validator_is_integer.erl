% vim: sw=4 ts=4 et ft=erlang
% Nitrogen Web Framework for Erlang
% Copyright (c) 2008-2013 Rusty Klophaus
% See MIT-LICENSE for licensing information.

-module (validator_is_integer).
-include("wf.hrl").
-export([
    render_action/1
]).

render_action(Record) ->
    TriggerPath = Record#is_integer.trigger,
    TargetPath = Record#is_integer.target,
    Text = wf:js_escape(Record#is_integer.text),
    Min = Record#is_integer.min,
    Max = Record#is_integer.max,
    AllowBlank = Record#is_integer.allow_blank,

    ValidationFun = fun(_, Value) -> validate(AllowBlank, Value, Min, Max) end,

    CustomValidatorAction =  #custom {
        trigger=TriggerPath,
        target=TargetPath,
        function=ValidationFun,
        text = Text,
        tag=Record,
        attach_to=Record#is_integer.attach_to
    },

    %% A little silly that we have to set both notAnInteger and notANumber
    %% message, but apparently if it's not a number, Livevalidation shows "Not a
    %% Number" before it checks if it's an integer.  This is an easy fix, but I
    %% want to replace livevalidation completely
    Script = wf:f("v.add(Validate.Numericality, { notAnIntegerMessage: \"~ts\", notANumberMessage: \"~ts\", onlyInteger: true });", [Text, Text]),
    [CustomValidatorAction, Script].

validate(_AllowBlank=true, "", _Min, _Max) ->
    true;
validate(_AllowBlank, Value, Min, Max) ->
    try
        validate_range(wf:to_integer(Value), Min, Max)
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
