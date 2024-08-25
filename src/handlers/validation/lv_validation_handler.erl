% vim: sw=4 ts=4 et ft=erlang
% Nitrogen Web Framework for Erlang
% See MIT-LICENSE for licensing information.

-module(lv_validation_handler).
-behaviour(validation_handler).
-include("wf.hrl").
-export([
    init/2,
    finish/2,
%    attach/5,
%    validate/4,
    js_constructor/7,
    js_add_validator/6,
    required_js/2
%    required_css/2
]).


init(_Config, State) ->
    {ok, State}.

finish(_Config, State) ->
    {ok, State}.

%validate(Field, Value, _Config, State) ->
%    true.
%
%attach(Targetid, Field, Validators, _Config, State) ->
%    State.

-define(JS_SCRIPT, <<"/nitrogen/nitro_livevalidation.js">>).

required_js(undefined, _State) ->
    ?JS_SCRIPT;
required_js(Config, _State) ->
    ds:get(Config, required_js, ?JS_SCRIPT).

%required_css(Config, _State) ->
%    ds:get(Config, required_css, undefined).

js_constructor(TargetPath, ValidationGroup, ValidMessage, On, AttachTo, Config, State) ->
    %ValidMessage = wf:js_escape(Record#validate.success_text),
    OnlyOnBlur = lists:member(blur, On),
    OnlyOnSubmit = lists:member(submit, On),
    InsertAfterNode = case AttachTo of
        undefined -> "";
        Node -> wf:f(<<", insertAfterWhatNode : obj(\"~s\")">>, [Node])
    end,
    % Create the validator Javascript...
    
    ConstructorJS = wf:f(<<"var v = Nitrogen.$init_validation(obj('~s'), '~s', { validMessage: \"~ts\", onlyOnBlur: ~s, onlyOnSubmit: ~s ~s});">>, [TargetPath, ValidationGroup, wf:js_escape(ValidMessage), OnlyOnBlur, OnlyOnSubmit, InsertAfterNode]),
    %GroupJS = ?WF_IF(ValidationGroup, wf:f(<<"v.group = '~s';">>, [ValidationGroup])),
    
    CombinedJS = [
        ConstructorJS
    ],
    {NewState, NewScript} = maybe_dependency_wrap(CombinedJS, Config, State),
    {ok, NewScript, NewState}.



%% NOTE: This needs to be reworked to be put into the validation_handler
%% module, but in order to do so, it'll need toe rely on the normal wf:state
%% instead of its internal state - UNLESS wf_core is modified to add the
%% validation_handler to the list of handlers that are serialized and
%% deserialized
maybe_dependency_wrap(Script, Config, State) ->
    case ds:get(State, live_validation_prewrapped, false) of
        true ->
            {State, Script};
        false ->
            JS = required_js(Config, State),
            NewState = ds:set(State, live_validation_prewrapped, true),
            NewScript = [
                #script{
                    dependency_js=JS,
                    script=#console_log{text="JS is loaded: " ++ JS}
                },
                Script
            ],
            {NewState, NewScript}
    end.

   
%% Maybe rename ValidationGroup to Trigger?

-spec js_add_validator(Target :: id(), validator_type(), FM :: text(), Opts :: proplist() | map(), Config :: any(), State :: any()) -> text().
js_add_validator(Target, Type, FM, Opts, Config, State) ->
    [
        wf:f(<<"Nitrogen.$add_validation('~s', function(v) {">>, [wf:js_escape(Target)]),
        js_add_validator_inner(Type, FM, Opts, Config, State),
        <<"});">>
    ].

%% FM=FailureMessage
js_add_validator_inner(integer, FM, _Opts, _Config, _State) ->
    wf:f(<<"v.add(Validate.Numericality, {notAnIntegerMessage: \"~ts\", notANumberMessage: \"~ts\", onlyInteger: true});">>, [FM, FM]);
js_add_validator_inner(number, FM, _Opts, _Config, _State) ->
    wf:f(<<"v.add(Validate.Numericality, {notANumberMessage: \"~ts\", onlyInteger: false});">>, [FM]);
js_add_validator_inner(not_blank, FM, _Opts, _Config, _State) ->
    wf:f(<<"v.add(Validate.Presence, {failureMessage: \"~ts\"});">>, [FM]);
js_add_validator_inner(email, FM, _Opts, _Config, _State) ->
    wf:f(<<"v.add(Validate.Email, {failureMessage: \"~ts\"});">>, [FM]);
js_add_validator_inner(max_length, FM, Opts, _Config, _State) ->
    Max = ds:get(Opts, max),
    wf:f(<<"v.add(Validate.Length, {maximum: ~p, tooLongMessage: \"~ts\"});">>, [Max, FM]);
js_add_validator_inner(min_length, FM, Opts, _Config, _State) ->
    Min = ds:get(Opts, min),
    wf:f(<<"v.add(Validate.Length, {minimum: ~p, tooShortMessage: \"~ts\"});">>, [Min, FM]);
js_add_validator_inner(custom, FM, Opts, _Config, _State) ->
    [Function, Args, WhenEmpty0] = ds:get_list(Opts, [function, args, when_empty]),
    WhenEmpty = wf:to_bool(WhenEmpty0),
    wf:f(<<"v.add(Validate.Custom, {against: ~s, args: ~ts, failureMessage: \"~ts\", displayMessageWhenEmpty:~p});">>, [Function, Args, FM, WhenEmpty]).
