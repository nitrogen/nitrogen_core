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
    Vessel = Record#textbox.vessel,
    Disabled = Record#textbox.disabled,
    Readonly = Record#textbox.readonly,
    HandleInvalid = Record#textbox.handle_invalid,
    OnInvalid = Record#textbox.on_invalid,
    Type0 = Record#textbox.type,
    Type = case wf:to_binary(Type0) of
        <<"datetime">> -> <<"datetime-local">>;
        X -> X
    end,

    action_event:maybe_wire_next(Anchor, Record#textbox.next),
    wire_postback(Anchor, ID, HandleInvalid, OnInvalid, Delegate, Postback, Vessel),

    Value = process_value(Type, Record#textbox.text, Record#textbox.html_encode),
    Min = process_value(Type, Record#textbox.min, Record#textbox.html_encode),
    Max = process_value(Type, Record#textbox.max, Record#textbox.html_encode),
    Step = wf:to_list(Record#textbox.step),
        
    Placeholder  = wf:html_encode(Record#textbox.placeholder, true),

    Attributes = [
        {id, Record#textbox.html_id},
        {type, Record#textbox.type}, 
        {autocomplete,Record#textbox.autocomplete},
        {class, [textbox, Record#textbox.class]},
        {title, Record#textbox.title},
        {maxlength, Record#textbox.maxlength},
        {size, Record#textbox.size},
        {style, Record#textbox.style},
        {step, Step},
        {name, Record#textbox.html_name},
        {placeholder, Placeholder},
        ?WF_IF(Disabled,disabled,undefined),
        ?WF_IF(Readonly,readonly,undefined),
        {value, Value},
        {min, Min},
        {max, Max},
        {data_fields, Record#textbox.data_fields}
    ],

    wf_tags:emit_tag(input, Attributes).

process_value(<<"datetime-local">>, Value, HtmlEncode) ->
    ?WF_SAFE(qdate:to_string("Y-m-d\\TH:i", Value), process_value(<<"text">>, Value, HtmlEncode));
process_value(<<"date">>, Value, HtmlEncode) ->
    ?WF_SAFE(qdate:to_string("Y-m-d", Value), process_value(<<"text">>, Value, HtmlEncode));
process_value(<<"time">>, Value, HtmlEncode) ->
    ?WF_SAFE(qdate:to_string("H:i", Value), process_value(<<"text">>, Value, HtmlEncode));
process_value(_, Value, HtmlEncode) ->
    wf:html_encode(Value, HtmlEncode).
    


wire_postback(_, _, _, _, _, undefined, _) ->
    do_nothing;
wire_postback(Anchor, ID, HandleInvalid, OnInvalid, Delegate, Postback, Vessel) ->
    wf:wire(Anchor, #event {
        type=enterkey,
        postback=Postback,
        vessel=Vessel,
        validation_group=ID,
        handle_invalid=HandleInvalid,
        on_invalid=OnInvalid,
        delegate=Delegate
    }).
