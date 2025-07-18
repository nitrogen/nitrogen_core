% vim: sw=4 ts=4 et ft=erlang
% Nitrogen Web Framework for Erlang
% Copyright (c) 2008-2010 Rusty Klophaus
% See MIT-LICENSE for licensing information.

-module(action_confirm).
-include("wf.hrl").
-export([render_action/1]).
-export([reflect/0]).

reflect() -> record_info(fields, confirm).

render_action(Record = #confirm{basic=true}) -> 
    TriggerPath = Record#confirm.trigger,
    TargetPath = Record#confirm.target,
    Delegate = Record#confirm.delegate,
    [
        wf:f("if (confirm(\"~ts\")) {", [wf:js_escape(Record#confirm.text)]),
        #event{
            postback=Record#confirm.postback,
            vessel=Record#confirm.vessel,
            trigger=TriggerPath,
            target=TargetPath,
            delegate=Delegate
        },
        Record#confirm.actions,
        "}"
    ];
render_action(Confirm = #confirm{basic=false}) ->
    OKButtonList = make_ok_button_list(Confirm),
    Confirm2 = maybe_set_close_text(Confirm),
    Modal = wf_utils:copy_fields(Confirm2, #modal{}),
    Buttons = Modal#modal.buttons ++ OKButtonList,
    Modal2 = Modal#modal{buttons=Buttons},
    Modal2.

maybe_set_close_text(C = #confirm{close_text=T, close_body=B})
        when ?WF_BLANK(T) andalso ?WF_BLANK(B) ->
    %% if the close button info isn't specified, override #modal's default of
    %% "Close" with "Cancel" (to more accurately match the way javascript's
    %% confirm() does it)"
    C#confirm{close_text="Cancel"};
maybe_set_close_text(C) ->
    C.

make_ok_button_list(#confirm{postback=Postback, vessel=Vessel, delegate=Delegate, actions=Actions}) when not(?WF_BLANK(Postback)) ->
    [#button{
        text="OK",
        postback=Postback,
        vessel=Vessel,
        delegate=Delegate,
        %trigger=TriggerPath,
        %target=TargetPath,
        click=[Actions,#close_modal{}]
    }];
make_ok_button_list(_) ->
    [].
