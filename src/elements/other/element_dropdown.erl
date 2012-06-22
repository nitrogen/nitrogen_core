% vim: ts=4 sw=4 et
% Nitrogen Web Framework for Erlang
% Copyright (c) 2008-2010 Rusty Klophaus
% See MIT-LICENSE for licensing information.

-module (element_dropdown).
-include_lib ("wf.hrl").
-compile(export_all).

reflect() -> record_info(fields, dropdown).

render_element(Record) -> 

    wire_postback(Record),
    set_dropdown_value(Record#dropdown.anchor,Record#dropdown.value),

    Options = format_options(Record#dropdown.options,Record#dropdown.html_encode),

    Multiple = case Record#dropdown.multiple of
        false -> [];
        true -> [{multiple}]
    end,

    wf_tags:emit_tag(select, Options, [
        {id, Record#dropdown.html_id},
        {class, [dropdown, Record#dropdown.class]},
        {style, Record#dropdown.style},
        {name, Record#dropdown.html_name},
        {data, Record#dropdown.data_fields}
    ] ++ Multiple).

set_dropdown_value(_,undefined) -> 
    ok;
set_dropdown_value(Anchor,Value) -> 
    wf:set(Anchor,Value).

wire_postback(Dropdown) when Dropdown#dropdown.postback==undefined ->
    ignore;
wire_postback(Dropdown) ->
    wf:wire(Dropdown#dropdown.anchor, #event { 
        type=change, 
        postback=Dropdown#dropdown.postback,
        validation_group=Dropdown#dropdown.id,
        delegate=Dropdown#dropdown.delegate 
    }).

format_options(undefined,_) -> 
    "";
format_options(Opts,HtmlEncode) ->
    [create_option(Opt, HtmlEncode) || Opt <- Opts,Opt#option.show_if==true].

create_option(X, HtmlEncode) ->
    SelectedOrNot = case X#option.selected of
        true -> selected;
        _ -> not_selected
    end,

    Content = wf:html_encode(X#option.text, HtmlEncode),

    Props = [{SelectedOrNot, true}],

    %% if value property is 'undefined', then we don't want to emit it at all
    %% This keeps it consistent with the behavior of HTML forms
    Props1 = case X#option.value of
        undefined -> Props;
        V -> [ {value,wf:html_encode(V,HtmlEncode)} | Props]
    end,

    wf_tags:emit_tag(option, Content, Props1).
