% vim: sw=4 ts=4 et ft=erlang
% Nitrogen Web Framework for Erlang
% Copyright (c) 2025 Jesse Gumm
% See MIT-LICENSE for licensing information.

-module(default_modal_handler).
-include("wf.hrl").
-behaviour(modal_handler).
-export ([
    init/2,
    finish/2,
    process_open_action/3,
    process_close_action/3
]).

%-record(state, {}).

init(_Config, _State) ->
    {ok, []}.

finish(_Config, _State) ->
    {ok, []}.

process_open_action(Rec = #modal{}, _Config, _State) ->
    ID = wf:coalesce([Rec#modal.id, wf:temp_id()]),
    Body = build_body(ID, Rec),

    %% Rendering Body and capturing the Actions, otherwise the button actions will get wired to the page before the buttons exist
    {ok, RenderedBody, BodyActions} = wf:render_isolated(Body),

    Action = [
        #insert_top{
            trigger = Rec#modal.trigger,
            anchor = Rec#modal.anchor,
            target="body",
            elements=[
                #lightbox{id=ID, body=[
                    #panel{class=lightbox_form, body=RenderedBody}
                ]}
            ]
        },
        BodyActions,
        %% TODO: replace with scrollTo when it's added
        #js_fun{function="Nitrogen.$scroll_to_modal", args=[ID]},
        #js_fun{function="Nitrogen.$add_modal", args=[ID]},
        #js_fun{function="Nitrogen.$add_modal_zindex", args=[ID]}
    ],

    {ok, ID, Action}.

build_body(ID, #modal{text=Text, body=Body,
                 title_text=TitleT, title_body=TitleB,
                 close_text=CloseT, close_body=CloseB,
                 show_close_button=ShowCloseButton,
                 buttons=Buttons0, options=_Opts}) ->
    Buttons = process_buttons(Buttons0),
    [
        title(TitleT, TitleB),
        body(Text, Body),
        #panel{class=lightbox_buttons, body=[
            Buttons,
            close_button(ShowCloseButton, ID, CloseT, CloseB)
        ]}
    ].

title(Text, Body) when ?WF_BLANK(Text) andalso ?WF_BLANK(Body) ->
    [];
title(Text, Body) ->
    #h3{class=modal_title, text=Text, body=Body}.

body(Text, Body) when ?WF_BLANK(Text) andalso ?WF_BLANK(Body) ->
    [];
body(Text, Body) ->
    #panel{class=modal_body, text=Text, body=Body}.

process_buttons([X | Rest]) when ?WF_BLANK(X) ->
    process_buttons(Rest);
process_buttons([{Text, Postback} | Rest]) ->
    [#button{text=Text, postback=Postback} | process_buttons(Rest)];
process_buttons([{Text, Postback, Delegate} | Rest]) ->
    [#button{text=Text, postback=Postback, delegate=Delegate} | process_buttons(Rest)];
process_buttons([Button | Rest]) when ?IS_ELEMENT(Button) ->
    [Button | process_buttons(Rest)];
process_buttons([Other | Rest]) ->
    [Other | process_buttons(Rest)];
process_buttons([]) ->
    [].

close_button(false, _, _, _) ->
    [];
close_button(true, ID, Text, Body) when ?WF_BLANK(Text) andalso ?WF_BLANK(Body) ->
    #button{text="Close", click=#close_modal{id=ID}};
close_button(true, ID, Text, Body) ->
    #button{text=Text, body=Body, click=#close_modal{id=ID}}.

process_close_action(#close_modal{id=ID}, _Config, _State) ->
    RemoveModalAndGetID = #js_fun{function="Nitrogen.$remove_modal", args=[ID]},
    Script = #js_fun{function="Nitrogen.$remove", args=["body", RemoveModalAndGetID]},
    {ok, ID, Script}.
