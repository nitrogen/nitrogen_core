% vim: ts=4 sw=4 et
% Nitrogen Web Framework for Erlang
% Copyright (c) 2008-2010 Rusty Klophaus
% See MIT-LICENSE for licensing information.

-module (element_radio).
-include("wf.hrl").
-export([
    reflect/0,
    render_element/1
]).

-spec reflect() -> [atom()].
reflect() -> record_info(fields, radio).

-spec render_element(#radio{}) -> body().
render_element(Record) -> 
    ID = Record#radio.id,
    Anchor = case Record#radio.anchor of
        "." ++ AnchorNoDot -> AnchorNoDot;
        A -> A
    end,
    CheckedOrNot = case Record#radio.checked of
        true -> checked;
        _ -> not_checked
    end,

    case Record#radio.postback of
        undefined -> ignore;
        Postback -> wf:wire(Anchor, #event {
                    type=change,
                    postback=Postback,
                    validation_group=ID,
                    handle_invalid=Record#radio.handle_invalid,
                    on_invalid=Record#radio.on_invalid,
                    delegate=Record#radio.delegate })
    end,

    action_event:maybe_wire_next(Record#radio.anchor, Record#radio.next),

    Content = wf:html_encode(Record#radio.text, Record#radio.html_encode),
    Body = Record#radio.body,
    LabelDisabledClass = ?WF_IF(Record#radio.disabled, disabled),
    LabelClass = wf:coalesce([Record#radio.label_class, ""]),

    [
        %% Checkbox...
        wf_tags:emit_tag(input, [
            {id, Anchor},
            {value, Record#radio.value},

            %% the emitted name gives priority to html_name, but if it's
            %% undefined, then we fall back to the name attribute.
            %% Note, this might seem a bit hacky to have html_name and name
            %% that do essentially the same thing, but they have their own
            %% semantic meanings.  'html_name' is generally reserved for
            %% RESTful forms, while 'name' will be the more commonly used
            %% attribute.
            {name, wf:coalesce([Record#radio.html_name,Record#radio.name])},
            {type, radio},
            {class, [radio, Record#radio.class]},
            {title, Record#radio.title},
            {style, Record#radio.style},
            ?WF_IF(Record#radio.disabled, disabled),
            {data_fields, Record#radio.data_fields},
            {CheckedOrNot, true}
        ]),

        %% Label for Radio...
        wf_tags:emit_tag(label, [Body, Content], [
            {for, Anchor},
            {class, [radio_label, LabelDisabledClass, LabelClass]},
            {title, Record#radio.title}
        ])
    ].
