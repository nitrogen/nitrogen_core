% vim: sw=4 ts=4 et ft=erlang
% Nitrogen Web Framework for Erlang
% Copyright (c) 2008-2010 Rusty Klophaus
% See MIT-LICENSE for licensing information.

-module (element_checkbox).
-include("wf.hrl").
-export([
    reflect/0,
    render_element/1
]).

-spec reflect() -> [atom()].
reflect() -> record_info(fields, checkbox).

-spec render_element(#checkbox{}) -> body().
render_element(Record) -> 
    ID = Record#checkbox.id,
    Anchor = case Record#checkbox.anchor of
        "." ++ AnchorNoDot -> AnchorNoDot;
        A -> A
    end,
    CheckedOrNot = case Record#checkbox.checked of
        true -> checked;
        _ -> not_checked
    end,
    case Record#checkbox.postback of
        undefined -> ignore;
        Postback -> wf:wire(Anchor, #event {
                    type=change,
                    postback=Postback,
                    validation_group=ID,
                    handle_invalid=Record#checkbox.handle_invalid,
                    on_invalid=Record#checkbox.on_invalid,
                    delegate=Record#checkbox.delegate })
    end,

    Text = wf:html_encode(Record#checkbox.text, Record#checkbox.html_encode),
    [
        % Checkbox...
        wf_tags:emit_tag(input, [
            {name, Record#checkbox.html_name},
            {id,   Anchor},
            {type, checkbox},
            {class, [checkbox, Record#checkbox.class]},
            {style, Record#checkbox.style},
            {value, Record#checkbox.value},
            {CheckedOrNot, true},
            {data_fields, Record#checkbox.data_fields}
        ]),

        % Label for Checkbox...
        wf_tags:emit_tag(label, Text, [
            {for, Anchor}
        ])
    ].
