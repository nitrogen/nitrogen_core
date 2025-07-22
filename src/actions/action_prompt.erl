% vim: sw=4 ts=4 et ft=erlang
% Nitrogen Web Framework for Erlang
% Copyright (c) 2025 Jesse Gumm
% See MIT-LICENSE for licensing information.

-module(action_prompt).
-include("wf.hrl").
-export([render_action/1]).
-export([reflect/0]).
-export([event/1]).

%-record(postback_wrapper, {tag, delegate, field_ids}).

reflect() -> record_info(fields, prompt).

render_action(Record = #prompt{basic=true}) -> 
    TriggerPath = Record#prompt.trigger,
    TargetPath = Record#prompt.target,
    Delegate = Record#prompt.delegate,
    Default = Record#prompt.default,
    Text = Record#prompt.text,
    Tag = Record#prompt.tag,
    ID = wf:temp_id(),
    PB = build_postback(Tag, Delegate, [ID]),
    PromptCall1 = #js_fun{function='window.prompt', args=[Text, Default]},
    PromptCall2 = action_js_fun:render_action(PromptCall1),
    [
        <<"(function() {
            var promptVal;
            promptVal = (">>,PromptCall2,<<");
            if(promptVal) {">>,
                #event{
                    postback=PB,
                    vessel=Record#prompt.vessel,
                    trigger=TriggerPath,
                    target=TargetPath,
                    delegate=?MODULE,
                    extra_param=wf:f("{~s: promptVal}", [ID])
                },
                Record#prompt.actions,
        <<"}
        })();">>
    ];
render_action(Prompt = #prompt{basic=false, fields=[], default=Default}) ->
    ID = wf:temp_id(),
    Field = {ID, ""},
    Fields = [Field],
    Data = [{ID, Default}],
    render_action(Prompt#prompt{fields=Fields, default=Data});
render_action(Prompt = #prompt{basic=false, fields=Fields}) ->
    FieldIDs = element_quickform:field_ids(Fields),
    OKButtonList = make_ok_button_list(Prompt, FieldIDs),
    Buttons0 = Prompt#prompt.buttons ++ OKButtonList,
    Prompt2 = maybe_set_close_text(Prompt),
    Modal = wf_utils:copy_fields(Prompt2, #modal{}),
    Buttons = preprocess_buttons(Buttons0, FieldIDs),
    Default = Prompt#prompt.default,
    Body = #quickform{fields=Fields, data=Default},
    Modal2 = Modal#modal{buttons=Buttons, body=Body},
    Modal2.


preprocess_buttons(Buttons, FieldIDs) ->
    [preprocess_button(B, FieldIDs) || B <- Buttons].

preprocess_button({Text, Tag}, FieldIDs) ->
    {Text, build_postback(Tag, undefined, FieldIDs), ?MODULE};
preprocess_button({Text, Tag, Delegate}, FieldIDs) ->
    {Text, build_postback(Tag, Delegate, FieldIDs), ?MODULE};
preprocess_button(X, _FieldIDs) ->
    X.

maybe_set_close_text(P = #prompt{close_text=T, close_body=B})
        when ?WF_BLANK(T) andalso ?WF_BLANK(B) ->
    %% if the close button info isn't specified, override #modal's default of
    %% "Close" with "Cancel" (to more accurately match the way javascript's
    %% prompt() does it)"
    P#prompt{close_text="Cancel"};
maybe_set_close_text(P) ->
    P.

build_postback(Tag, Delegate, Fields) ->
    #{
        tag=>Tag,
        delegate=>Delegate,
        fields=>Fields
    }.

make_ok_button_list(#prompt{tag=Tag, vessel=Vessel, delegate=Delegate,
                            actions=Actions, basic=false}, FieldIDs) ->
    %    when not(?WF_BLANK(Tag)) ->
    PBMap = build_postback(Tag, Delegate, FieldIDs),
    [#button{
        text="OK",
        postback=PBMap,
        vessel=Vessel,
        delegate=?MODULE,
        %trigger=TriggerPath,
        %target=TargetPath,
        click=[Actions]
    }];
make_ok_button_list(_, _) ->
    [].

event(#{tag:=Tag, delegate:=Delegate0, fields:=Fields}) ->
    Delegate = wf:coalesce([Delegate0, wf:page_module()]),
    ValueMap = wf:q_pl(Fields),
    Result = Delegate:prompt_event(Tag, ValueMap),
    case handle_result(Result) of
        true -> wf:wire(#close_modal{});
        {false, Reason} -> wf:wire(#alert{text=Reason});
        false -> ok
    end,
    Result.

handle_result(ok) -> true;
handle_result(true) -> true;
handle_result({false, Reason}) -> {false, Reason};
handle_result({error, Reason}) -> {false, Reason};
handle_result(_) -> false.
