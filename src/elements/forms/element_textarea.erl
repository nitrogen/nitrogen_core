% vim: sw=4 ts=4 et ft=erlang
% Nitrogen Web Framework for Erlang
% Copyright (c) 2008-2010 Rusty Klophaus
% See MIT-LICENSE for licensing information.

-module (element_textarea).
-include_lib ("wf.hrl").
-compile(export_all).

reflect() -> record_info(fields, textarea).

render_element(Record) -> 
    Anchor = Record#textarea.anchor,
    Text = wf:html_encode(Record#textarea.text, Record#textarea.html_encode),
    Placeholder = wf:html_encode(Record#textarea.placeholder, true),

    action_event:maybe_wire_next(Anchor, Record#textarea.next),
    maybe_trap_tabs(Anchor, Record#textarea.trap_tabs),

    wf_tags:emit_tag(textarea, Text, [
        {class, ?ADD_ELEMENT_CLASS(textarea, Record#textarea.class)},
        {id, Record#textarea.html_id},
        {style, Record#textarea.style},
        {title, Record#textarea.title},
        {name, Record#textarea.html_name},
        {cols, Record#textarea.columns},
        {rows, Record#textarea.rows},
        ?WF_IF(Record#textarea.disabled,disabled,undefined),
        ?WF_IF(Record#textarea.readonly,readonly,undefined),
        {placeholder, Placeholder},
        {data_fields, Record#textarea.data_fields},
        {aria, Record#textarea.aria}
    ]).

maybe_trap_tabs(_Anchor, false) -> ok;
maybe_trap_tabs(Anchor, true) ->
    wf:defer(wf:f("Nitrogen.$trap_tabs('~s');", [Anchor])).
