% vim: sw=4 ts=4 et ft=erlang
% Nitrogen Web Framework for Erlang
% Copyright (c) 2008-2010 Rusty Klophaus
% See MIT-LICENSE for licensing information.

-module (action_event).
-include("wf.hrl").
-export([
        render_action/1,
        maybe_wire_next/2
    ]).

render_action(#event { 
        postback=Postback,
        actions=Actions,
        anchor=Anchor,
        trigger=Trigger,
        target=Target,
        validation_group=ValidationGroup,
        handle_invalid=HandleInvalid,
        on_invalid=OnInvalid,
        type=Type,
        keycode=KeyCode,
        shift_key=ShiftKey,
        delay=Delay,
        delegate=Delegate, 
        extra_param=ExtraParam
    }) -> 

    ValidationGroup1 = wf:coalesce([ValidationGroup, Trigger]),
    AnchorScript = wf_render_actions:generate_anchor_script(Anchor, Target), 
    PostbackScript = wf_event:generate_postback_script(Postback, Anchor, ValidationGroup1, HandleInvalid, OnInvalid, Delegate, ExtraParam),
    SystemPostbackScript = wf_event:generate_system_postback_script(Postback, Anchor, ValidationGroup1, HandleInvalid, Delegate),
    {EffectiveType, EffectiveKeyCode} = effective_type_and_keycode(Type, KeyCode),
    WireAction = #wire { trigger=Trigger, target=Target, actions=Actions },

    Script = case Type of

        %% SYSTEM EVENTS %%%
        % Trigger a system postback immediately...
        system when Delay == 0 ->
            [
                AnchorScript, SystemPostbackScript, WireAction
            ];

        % Trigger a system postback after some delay...
        system ->
            TempID = wf:temp_id(),
            [
                AnchorScript,
                wf:f("document.~s = function() {", [TempID]), SystemPostbackScript, WireAction, "};",
                wf:f("setTimeout(\"document.~s(); document.~s=null;\", ~p);", [TempID, TempID, Delay])
            ];

        %% USER EVENTS %%%

        % Handle keypress, keydown, or keyup when a keycode is defined...
        _ when ((EffectiveType==keypress orelse EffectiveType==keydown orelse EffectiveType==keyup) andalso (EffectiveKeyCode /= undefined)) ->
            [
                wf:f("Nitrogen.$observe_event('~s', '~s', '~s', function anonymous(event) {", [Anchor, Trigger, EffectiveType]),
                wf:f("if (Nitrogen.$is_key_code(event, ~p, ~p)) { ", [EffectiveKeyCode, ShiftKey]),
                AnchorScript, PostbackScript, WireAction, 
                "return false; }});"
            ];

        % Run the event after a specified amount of time
        timer ->
            TempID = wf:temp_id(),
            [
                wf:f("document.~s = function() {", [TempID]), 
                AnchorScript, PostbackScript, WireAction, 
                "};",
                wf:f("setTimeout(\"document.~s(); document.~s=null;\", ~p);", [TempID, TempID, Delay])
            ];

        default ->
            [
                AnchorScript, PostbackScript, WireAction
            ];

        % Run some other Javascript event (click, mouseover, mouseout, etc.)
        _ ->
            [
                wf:f("Nitrogen.$observe_event('~s', '~s', '~s', function anonymous(event) {", [Anchor, Trigger, Type]), 
                AnchorScript, PostbackScript, WireAction, 
                "});"
            ]

    end,
    Script.

%% Simple conversion of convenience events enterkey and tabkey
effective_type_and_keycode(enterkey, _) -> {keydown, 13};
effective_type_and_keycode(tabkey, _) -> {keydown, 9};
effective_type_and_keycode(Type, KeyCode) -> {Type, KeyCode}.

maybe_wire_next(_Anchor, undefined) -> do_nothing;
maybe_wire_next(Anchor, Next) ->
    Next1 = wf_render_actions:normalize_path(Next),
    wf:defer(Anchor, #event{ type=tabkey,   actions=wf:f("Nitrogen.$go_next('~s');", [Next1])}),
    wf:defer(Anchor, #event{ type=enterkey, actions=wf:f("Nitrogen.$go_next('~s');", [Next1])}).
