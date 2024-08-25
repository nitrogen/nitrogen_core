% vim: sw=4 ts=4 et ft=erlang
% Nitrogen Web Framework for Erlang
% Copyright (c) 2024 Jesse Gumm
% See MIT-LICENSE for licensing information.

-module (wf_validation).
-include ("wf.hrl").
-export ([validate/0, register_validator/3]).

-define(VALIDATOR_STATE_KEY, '$NITROGEN_VALIDATORS').

get_validators() ->
    state_handler:get_state(?VALIDATOR_STATE_KEY, []).

set_validators(Validators) ->
    state_handler:set_state(?VALIDATOR_STATE_KEY, Validators).

%% NOTE: This function is called from validator_custom.  It was originally
%% implemented there as well, but it was confusing where and how server-side
%% validations were established.
register_validator(TriggerPath, TargetPath, Record) ->
    V = {TriggerPath, TargetPath, Record},
    Validators = get_validators(),
    NewValidators = (Validators -- [V]) ++ [V],
    set_validators(NewValidators).


validate() ->
    % Some values...
    ValidationGroup = wf_context:event_validation_group(),
    Validators = get_validators(),

    % Get all validators that match the validation group.
    % ValidationGroup is a string.
    Validators1 = [X || X={VG, _, _} <- Validators, ValidationGroup == VG],

    % Now, run through each matching validator.
    % Stop validating a TargetPath when it has failed.
    F2 = fun({_, TargetPath, Record}, FailedPaths) ->
        case lists:member(TargetPath, FailedPaths) of
            true -> 
                FailedPaths;
            false ->
                Function = Record#custom.function,
                Text = Record#custom.text,
                Value = case wf:qs(TargetPath) of
                    [V | _] -> V;
                    [] -> undefined
                end,
                case Function(Record#custom.tag, Value) of
                    true -> 
                        FailedPaths;
                    false ->
                        wf:wire(#validation_error{target=TargetPath, text=Text, attach_to=Record#custom.attach_to}),
                        [TargetPath|FailedPaths]
                end
        end
    end,

    FailedPaths1 = lists:foldl(F2, [], Validators1),
    {ok, FailedPaths1 == []}.
