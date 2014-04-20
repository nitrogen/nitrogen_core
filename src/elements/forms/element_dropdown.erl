% vim: ts=4 sw=4 et
% Nitrogen Web Framework for Erlang
% Copyright (c) 2008-2010 Rusty Klophaus
% See MIT-LICENSE for licensing information.

-module (element_dropdown).
-include("wf.hrl").
-export([
    reflect/0,
    render_element/1
]).

-define(DEFAULT_MULTISELECT_SIZE, 5).
-define(DEFAULT_SINGLESELECT_SIZE, 1).

-spec reflect() -> [atom()].
reflect() -> record_info(fields, dropdown).

-spec render_element(#dropdown{}) -> body().
render_element(Record) -> 

    wire_postback(Record),
    action_event:maybe_wire_next(Record#dropdown.anchor, Record#dropdown.next),

    Options = format_options(Record),

    MultipleAttribute = case Record#dropdown.multiple of
        true -> [{multiple}];
        false -> []
    end,

    DisabledAttribute = case Record#dropdown.disabled of
        true -> [{disabled}];
        false -> []
    end,

    Size = case {Record#dropdown.size, Record#dropdown.multiple} of
        {auto, true} -> ?DEFAULT_MULTISELECT_SIZE;
        {auto, false} -> ?DEFAULT_SINGLESELECT_SIZE;
        {Num, _} -> Num
    end,

    wf_tags:emit_tag(select, Options, [
        {id, Record#dropdown.html_id},
        {class, [dropdown, Record#dropdown.class]},
        {title, Record#dropdown.title},
        {style, Record#dropdown.style},
        {name, Record#dropdown.html_name},
        {size, Size},
        MultipleAttribute,
        DisabledAttribute,
        {data_fields, Record#dropdown.data_fields}
    ]).


wire_postback(Dropdown) when Dropdown#dropdown.postback==undefined ->
    ignore;
wire_postback(Dropdown) ->
    wf:wire(Dropdown#dropdown.anchor, #event { 
        type=change, 
        postback=Dropdown#dropdown.postback,
        handle_invalid=Dropdown#dropdown.handle_invalid,
        on_invalid=Dropdown#dropdown.on_invalid,
        validation_group=Dropdown#dropdown.id,
        delegate=Dropdown#dropdown.delegate 
    }).

format_options(Dropdown) when Dropdown#dropdown.options==undefined ->
    "";
format_options(#dropdown{options=Opts, value=Value, html_encode=HtmlEncode}) ->
    create_options(wf:to_list(Value), HtmlEncode, Opts).

create_options(_,_,[]) ->
    [];

create_options(Selected,HtmlEncode, [{Value, Text} | Rest]) ->
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

    Props = [
        {SelectedOrNot},
        ?WF_IF(X#option.disabled, disabled, undefined),
        ?WF_IF(X#option.value=:=undefined,[],{value, wf:html_encode(X#option.value,HtmlEncode)})
    ],

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
