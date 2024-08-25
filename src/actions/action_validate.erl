% vim: sw=4 ts=4 et ft=erlang
% Nitrogen Web Framework for Erlang
% Copyright (c) 2008-2010 Rusty Klophaus
% See MIT-LICENSE for licensing information.

-module (action_validate).
-include("wf.hrl").
-export([
    reflect/0,
    render_action/1
]).

-spec reflect() -> [atom()].
reflect() -> record_info(fields, validate).

-spec render_action(#validate{}) -> script().
render_action(Record) -> 
    % Some values...
    TriggerPath = Record#validate.trigger,
    TargetPath = Record#validate.target,
    ValidationGroup = case Record#validate.group of
        undefined -> TriggerPath;
        Other -> Other
    end,
    ValidMessage = Record#validate.success_text,
    On = Record#validate.on,
    AttachTo = Record#validate.attach_to,

    %InsertAfterNode = case Record#validate.attach_to of
    %    undefined -> "";
    %    Node -> wf:f(<<", insertAfterWhatNode : obj(\"~s\")">>, [Node])
    %end,

    % Create the validator Javascript...
    ConstructorJS = validation_handler:js_constructor(TargetPath, ValidationGroup, ValidMessage, On, AttachTo),

    % Update all child validators with TriggerPath and TargetPath...
    F = fun(X) ->
        Base = wf_utils:get_validatorbase(X),
        Base1 = Base#validatorbase{
            trigger=TriggerPath,
            target=TargetPath,
            attach_to=Record#validate.attach_to
        },
        wf_utils:replace_with_base(Base1, X)
    end,
    Validators = lists:flatten([Record#validate.validators]),
    Validators1 = [F(X) || X <- Validators],

    % Use #script element to create the final javascript to send to the browser...
    [
        ConstructorJS,
        %TriggerJS,
        Validators1
    ].	
