% vim: sw=4 ts=4 et
% Nitrogen Web Framework for Erlang
% Copyright (c) 2008-2010 Rusty Klophaus
% See MIT-LICENSE for licensing information.

-module (element_textbox).
-include_lib ("wf.hrl").
-export([
    reflect/0,
    render_element/1
]).

reflect() -> record_info(fields, textbox).

render_element(Record) -> 
    ID = Record#textbox.id,
    Anchor = Record#textbox.anchor,
    Delegate = Record#textbox.delegate,
    Postback = Record#textbox.postback,
    Disabled = Record#textbox.disabled,
    Readonly = Record#textbox.readonly,
    HandleInvalid = Record#textbox.handle_invalid,
    OnInvalid = Record#textbox.on_invalid,

    wire_next(Anchor, Record#textbox.next),
    wire_postback(Anchor, ID, HandleInvalid, OnInvalid, Delegate, Postback),

    Value = wf:html_encode(Record#textbox.text, Record#textbox.html_encode),
    Placeholder  = wf:html_encode(Record#textbox.placeholder, true),

    Attributes = [
        {id, Record#textbox.html_id},
        {type, Record#textbox.type}, 
        {class, [textbox, Record#textbox.class]},
        {maxlength, Record#textbox.maxlength},
        {size, Record#textbox.size},
        {style, Record#textbox.style},
        {name, Record#textbox.html_name},
        {placeholder, Placeholder},
        ?WF_IF(Disabled,disabled,undefined),
        ?WF_IF(Readonly,readonly,undefined),
        {value, Value},
        {data_fields, Record#textbox.data_fields}
    ],

    wf_tags:emit_tag(input, Attributes).

wire_next(_, undefined) ->
    do_nothing;
wire_next(Anchor, Next) ->
    Next1 = wf_render_actions:normalize_path(Next),
    wf:wire(Anchor, #event { type=enterkey, actions=wf:f("Nitrogen.$go_next('~s');", [Next1]) }).

wire_postback(_, _, _, _, _, undefined) ->
    do_nothing;
wire_postback(Anchor, ID, HandleInvalid, OnInvalid, Delegate, Postback) ->
    wf:wire(Anchor, #event {
        type=enterkey,
        postback=Postback,
        validation_group=ID,
        handle_invalid=HandleInvalid,
        on_invalid=OnInvalid,
        delegate=Delegate
    }).
