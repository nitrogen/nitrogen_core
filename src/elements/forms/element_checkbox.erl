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
    Anchor = format_anchor(Record#checkbox.anchor),
    maybe_wire_postback(Anchor, Record),
    action_event:maybe_wire_next(Record#checkbox.anchor, Record#checkbox.next),
    render_checkbox(Anchor, Record).

render_checkbox(Anchor, Record) ->
    Checkbox = render_checkbox_tag(Anchor, Record),
    LabelPosition = Record#checkbox.label_position,
    Text = Record#checkbox.text,
    HtmlEncode = Record#checkbox.html_encode,
    finish_checkbox(Anchor, Checkbox, LabelPosition, Text, HtmlEncode).

finish_checkbox(_Anchor, Checkbox, none, _Text, _HtmlEncode) ->
    Checkbox;
finish_checkbox(Anchor, Checkbox, LabelPosition, Text0, HtmlEncode) ->
    Text = wf:html_encode(Text0, HtmlEncode),

    LabelBody = position_label(LabelPosition, Text, Checkbox),
    %% Contain the Checkbox within the label element itself. This ensures that
    %% clicking the label will also toggle the checkbox.
    wf_tags:emit_tag(label, LabelBody, [{for, Anchor}]).
    
position_label('after', Text, Checkbox) ->
    [Checkbox, Text];
position_label('before', Text, Checkbox) ->
    [Checkbox, Text].

render_checkbox_tag(Anchor, Record) ->
    wf_tags:emit_tag(input, [
        {name, Record#checkbox.html_name},
        {id,   Anchor},
        {type, checkbox},
        {class, [checkbox, Record#checkbox.class]},
        {title, Record#checkbox.title},
        {style, Record#checkbox.style},
        {value, Record#checkbox.value},
        ?WF_IF(Record#checkbox.checked, checked),
        ?WF_IF(Record#checkbox.disabled, disabled),
        {data_fields, Record#checkbox.data_fields}
    ]).

format_anchor(Anchor) ->
    string:strip(Anchor, left, $.).

maybe_wire_postback(_Anchor, #checkbox{postback=undefined}) ->
    ok;
maybe_wire_postback(Anchor, Record=#checkbox{}) ->
    Action = #event {
        type=change,
        postback=Record#checkbox.postback,
        vessel=Record#checkbox.vessel,
        validation_group=Record#checkbox.id,
        handle_invalid=Record#checkbox.handle_invalid,
        on_invalid=Record#checkbox.on_invalid,
        delegate=Record#checkbox.delegate
    },
    wf:wire(Anchor, Action).
