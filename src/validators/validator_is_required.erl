% vim: sw=4 ts=4 et ft=erlang
% Nitrogen Web Framework for Erlang
% Copyright (c) 2008-2010 Rusty Klophaus
% Copyright (c) 2014 Jesse Gumm
% See MIT-LICENSE for licensing information.

-module (validator_is_required).
-include("wf.hrl").
-export([render_action/1, validate/2]).

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
        function=fun ?MODULE:validate/2,
        attach_to=Record#is_required.attach_to
    },

    case Record#is_required.unless_has_value of
        [] ->
            %% Because where are no unless_has_value fields defined, we must
            %% validate this for every postback
            Script = validation_handler:js_add_validator(TargetPath, not_blank, Text),
            [CustomValidator, Script];
        Otherfields ->
            %% There are other fields to test.  In which case, we augment the
            %% client-side validation to also check the other fields, and if
            %% *they* have value, we don't have to validate this field.
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
    FieldChecks = [wf:f(<<"(obj('~s').value != '')">>, [F]) || F <- OtherFields],
    OredFieldChecks = wf:join(FieldChecks, " || "),
    [
        <<"function(value, args) {">>,
            <<"return (value != '') || ">>, OredFieldChecks,
        <<"}">>
    ].

validate(#is_required{unless_has_value=Otherfields}, V) when Otherfields =/= [], ?WF_BLANK(V) ->
    % provided value is blank, and we're evaluating the other fields, so let's
    % evaluate them
    lists:any(fun(Field) ->
        OtherVal = wf:q(Field),
        not(?WF_BLANK(OtherVal))
    end, Otherfields);
validate(_, V) when ?WF_BLANK(V) ->
    % Provided field is blank, and we're not checking other fields (we know
    % this because the previous clause would have matched on OtherFields being
    % a non-empty list).
    false;
validate(_, _) ->
    % Finally, if the field is non-blank, then return true, the field
    % validates, and we don't even have to check the others.
    true.
