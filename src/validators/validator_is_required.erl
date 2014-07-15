% vim: sw=4 ts=4 et ft=erlang
% Nitrogen Web Framework for Erlang
% Copyright (c) 2008-2010 Rusty Klophaus
% Copyright (c) 2014 Jesse Gumm
% See MIT-LICENSE for licensing information.

-module (validator_is_required).
-include("wf.hrl").
-export([render_action/1]).


render_action(Record = #is_required{unless_has_value=undefined}) ->
    render_action(Record#is_required{unless_has_value=[]});
render_action(Record = #is_required{unless_has_value=UHV}) when ?IS_STRING(UHV);
                                                                not(is_list(UHV)) ->
    render_action(Record#is_required{unless_has_value=[UHV]});
render_action(Record) ->
    TriggerPath = Record#is_required.trigger,
    TargetPath = Record#is_required.target,
    Text = wf:js_escape(Record#is_required.text),
    CustomValidator = #custom{
        trigger=TriggerPath,
        target=TargetPath,
        text=Text,
        tag=Record,
        function=fun validate/2,
        attach_to=Record#is_required.attach_to
    },

    case Record#is_required.unless_has_value of
        [] ->
            Script = wf:f("v.add(Validate.Presence, { failureMessage: \"~s\" });", [Text]),
            [CustomValidator, Script];
        Otherfields ->
            JSValidatorFun = build_js_function(Otherfields),
            JSValidation = #js_custom{
                trigger=TriggerPath,
                target=TargetPath,
                function=JSValidatorFun,
                text=Text,
                attach_to=Record#is_required.attach_to,
                when_empty=true
            },
            [CustomValidator, JSValidation]
    end.

build_js_function(OtherFields) ->
    FieldChecks = [wf:f("(obj('~s').value != '')", [F]) || F <- OtherFields],
    OredFieldChecks = wf:join(FieldChecks, " || "),
    [
        "function(value, args) {",
            "return (value != '') || ", OredFieldChecks,
        "}"
    ].

validate(Rec, undefined) ->
    % provided value is undefined, convert to empty string and try again
    validate(Rec, "");
validate(#is_required{unless_has_value=Otherfields}, "") when Otherfields =/= [] ->
    % provided value is blank, and we're evaluating the other fields, so let's
    % evaluate them
    lists:any(fun(Field) ->
        Val = wf:q(Field),
        io:format("~p=~p~n", [Field, Val]),
        Val =/= undefined andalso Val =/= ""
    end, Otherfields);
validate(_, "") ->
    % Provided field is blank, and we're not checking other fields (we know
    % this because the previous clause would have caught it).
    false;
validate(_, _) ->
    % Finally, if the field is non-blank, then return true, the field
    % validates, and we don't even have to check the others.
    true.
