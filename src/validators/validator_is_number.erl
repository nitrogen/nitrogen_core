% vim: sw=4 ts=4 et ft=erlang
% Nitrogen Web Framework for Erlang
% Copyright (c) 2008-2013 Rusty Klophaus
% Copyright (c) 2016-2024 Jesse Gumm
% See MIT-LICENSE for licensing information.

-module (validator_is_number).
-include("wf.hrl").
-export([
    render_action/1,
    validate/2
]).

render_action(R = #is_integer{}) ->
    render_action(#is_number{
        trigger=R#is_integer.trigger,
        target=R#is_integer.target,
        text = R#is_integer.text,
        min = R#is_integer.min,
        max = R#is_integer.max,
        allow_blank = R#is_integer.allow_blank,
        attach_to = R#is_integer.attach_to,
        type = integer
    });
render_action(Record = #is_number{}) ->
    TriggerPath = Record#is_number.trigger,
    TargetPath = Record#is_number.target,
    Text = wf:js_escape(Record#is_number.text),
    Type = Record#is_number.type,

    %% This part is for server-side validation
    ValidationFun = fun ?MODULE:validate/2,

    CustomValidatorAction =  #custom {
        trigger=TriggerPath,
        target=TargetPath,
        function=ValidationFun,
        text=Text,
        tag=Record,
        attach_to=Record#is_number.attach_to
    },
    Script = validation_handler:js_add_validator(TargetPath, Type, Text),
    [CustomValidatorAction, Script].

validate(#is_number{allow_blank=AllowBlank, min=Min, max=Max, type=Type}, Value) ->
    validate(AllowBlank, Type, Min, Max, Value).

validate(_AllowBlank=true, _Type, _Min, _Max, Value) when ?WF_BLANK(Value) ->
    true;
validate(_AllowBlank, Type, Min, Max, Value) ->
    validate_type(Type, Value) andalso validate_range(Type, Value, Min, Max).

validate_type(integer, X) when is_integer(X) ->
    true;
validate_type(integer, X) when is_float(X) ->
    %% We can allow a float if it's equal to a ==
    X == wf:to_integer(X);
validate_type(integer, X) when ?IS_STRING(X) orelse is_binary(X) ->
    lists:all(fun(C) ->
        %% So it's only integer characters and nothing else
        C >= $0 andalso C =< $9
    end, wf:to_list(X));
validate_type(number, X) when is_integer(X) orelse is_float(X) ->
    true;
validate_type(number, X) when ?IS_STRING(X) orelse is_binary(X) ->
    validate_float(wf:to_list(X)).

validate_float(X) ->
    validate_float(X, false).

validate_float([$.|T], false = _FoundDot) ->
    %% We've encountered our first dot. We'll continue and record that we've done so
    validate_float(T, true);
validate_float([$.|_], true = _FoundDot) ->
    %% We've already found a dot, so finding a second dot means this isn't a viable float. Time to return false.
    false;
validate_float([H|T], FoundDot) when H >= $0 andalso H =< $9 ->
    validate_float(T, FoundDot);
validate_float([], _FoundDot) ->
    %% We've looked at every character. If we're here, it's a valid float.
    true;
validate_float(_, _) ->
    false.

validate_range(Type, X, Min, Max) ->
    Fun = case Type of
        integer -> fun(Y) -> wf:to_integer(Y, undefined) end;
        number -> fun(Y) -> wf:to_float(Y, undefined) end
    end,
    validate_range_inner(Fun(X), Fun(Min), Fun(Max)).

validate_range_inner(undefined, _Min, _Max) ->
    %% This should probably never happen, but as a safeguard if the type conversion doesn't work - then fail
    false;
validate_range_inner(_, undefined, undefined) ->
    true;
validate_range_inner(X, Min, undefined) ->
    X >= Min;
validate_range_inner(X, undefined, Max) ->
    X =< Max;
validate_range_inner(X, Min, Max) ->
    X >= Min andalso X =< Max.
