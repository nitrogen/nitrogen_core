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
    Options = format_options(Record),

    Multiple = case Record#dropdown.multiple of
        true -> [{multiple}];
        false -> []
    end,

    Disabled = case Record#dropdown.disabled of
        true -> [{disabled}];
        false -> []
    end,

    wf_tags:emit_tag(select, Options, [
        {id, Record#dropdown.html_id},
        {class, [dropdown, Record#dropdown.class]},
        {style, Record#dropdown.style},
        {name, Record#dropdown.html_name},
        {data_fields, Record#dropdown.data_fields}
    ] ++ Multiple ++ Disabled).

wire_postback(Dropdown) when Dropdown#dropdown.postback==undefined ->
    ignore;
wire_postback(Dropdown) ->
    wf:wire(Dropdown#dropdown.anchor, #event { 
        type=change, 
        postback=Dropdown#dropdown.postback,
        validation_group=Dropdown#dropdown.id,
        delegate=Dropdown#dropdown.delegate 
    }).

format_options(Dropdown) when Dropdown#dropdown.options==undefined ->
    "";
format_options(#dropdown{options=Opts, value=Value, html_encode=HtmlEncode}) ->
    create_options(wf:to_list(Value), HtmlEncode, Opts).

create_options(_,_,[]) ->
    [];

create_options(Selected,HtmlEncode, [{Text, Value} | Rest]) ->
    Option = #option{text=Text, value=Value},
    create_options(Selected,HtmlEncode,[Option|Rest]);
create_options(Selected,HtmlEncode,
        [#option_group{text=Text,options=Options,disabled=Disabled,show_if=true} | Rest]) ->

    OptionTags = create_options(Selected,HtmlEncode,Options),

    LabelProp = [{label,wf:html_encode(Text)}],
    DisabledProp = case Disabled of
        true -> [{disabled,disabled}];
        false -> []
    end,

    Props = LabelProp ++ DisabledProp,

    [wf_tags:emit_tag(optgroup, OptionTags, Props) | create_options(Selected,HtmlEncode,Rest)];

create_options(Selected,HtmlEncode,[#option_group{show_if=false} | Rest]) ->
    create_options(Selected,HtmlEncode,Rest);
create_options(Selected,HtmlEncode,[X=#option{show_if=true} | Rest]) ->

    SelectedOrNot = selected_or_not(Selected,X),
    Content = wf:html_encode(X#option.text, HtmlEncode),

    SelectedProp = [{SelectedOrNot, true}],
    DisabledProp = case X#option.disabled of
        true -> [{disabled,disabled}];
        false -> []
    end,

    %% if value property is 'undefined', then we don't want to emit it at all
    %% This keeps it consistent with the behavior of HTML forms
    ValueProp = case X#option.value of
        undefined -> [];
        V -> [{value,wf:html_encode(V,HtmlEncode)}]
    end,

    Props = SelectedProp ++ DisabledProp ++ ValueProp,

    [wf_tags:emit_tag(option, Content, Props) | create_options(Selected,HtmlEncode,Rest)];
create_options(Selected,HtmlEncode,[#option{show_if=false} | Rest]) ->
    create_options(Selected,HtmlEncode,Rest);
create_options(_,_,[Other | _]) ->
    throw({unknown_option_provided_to_dropdown_element,Other}).


selected_or_not(Selected,X) ->
    case (Selected =/= undefined andalso wf:to_list(X#option.value) == Selected)
            orelse X#option.selected == true of
        true -> selected;
        false -> not_selected
    end.
