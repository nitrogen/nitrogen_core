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
    #textbox{
        id=ID,
        anchor=Anchor,
        delegate=Delegate,
        postback=Postback,
        vessel=Vessel,
        disabled=Disabled,
        readonly=Readonly,
        handle_invalid=HandleInvalid,
        on_invalid=OnInvalid,
        type=Type0,
        pattern=Pattern,
        html_encode=HtmlEncode,
        text=Text,
        min=Min0,
        max=Max0,
        step=Step0,
        placeholder=Placeholder0,
        autocomplete=Autocomplete,
        html_id=HtmlID,
        class=Class,
        title=Title,
        maxlength=Maxlength,
        html_name=HtmlName,
        size=Size,
        style=Style,
        data_fields=DataFields,
        aria=Aria,
        next=Next
    } = Record,
    Type = case wf:to_binary(Type0) of
        <<"datetime">> -> <<"datetime-local">>;
        X -> X
    end,

    action_event:maybe_wire_next(Anchor, Next),
    wire_postback(Anchor, ID, HandleInvalid, OnInvalid, Delegate, Postback, Vessel),

    Value = process_value(Type, Text, HtmlEncode),
    Min = process_value(Type, Min0, HtmlEncode),
    Max = process_value(Type, Max0, HtmlEncode),
    Step = wf:to_list(Step0),
        
    Placeholder  = wf:html_encode(Placeholder0, true),

    Attributes = [
        {id, HtmlID},
        {type, Type}, 
        {autocomplete,Autocomplete},
        {class, ?ADD_ELEMENT_CLASS(textbox, Class)},
        {title, Title},
        {maxlength, Maxlength},
        {pattern, Pattern},
        {size, Size},
        {style, Style},
        {step, Step},
        {name, HtmlName},
        {placeholder, Placeholder},
        ?WF_IF(Disabled,disabled,undefined),
        ?WF_IF(Readonly,readonly,undefined),
        {value, Value},
        {min, Min},
        {max, Max},
        {data_fields, DataFields},
        {aria, Aria}
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
