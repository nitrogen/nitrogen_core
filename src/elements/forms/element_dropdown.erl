% vim: ts=4 sw=4 et
% Nitrogen Web Framework for Erlang
% Copyright (c) 2008-2010 Rusty Klophaus
% Copyright (c) 2014 Jesse Gumm
% See MIT-LICENSE for licensing information.

-module (element_dropdown).
-include("wf.hrl").
-export([
    reflect/0,
    render_element/1,
    create_option/1
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

    MultipleAttribute = ?WF_IF(Record#dropdown.multiple,multiple,undefined),
    DisabledAttribute = ?WF_IF(Record#dropdown.disabled,disabled,undefined),
    Size = provided_or_default_size(Record),

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

provided_or_default_size(#dropdown{size=auto, multiple=true}) ->
    ?DEFAULT_MULTISELECT_SIZE;
provided_or_default_size(#dropdown{size=auto, multiple=false}) ->
    ?DEFAULT_SINGLESELECT_SIZE;
provided_or_default_size(#dropdown{size=Size}) ->
    Size.

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
format_options(#dropdown{options=Opts, value=Value0, html_encode=HtmlEncode}) ->
    Value = binary_or_undefined(Value0),
    create_options(Value, HtmlEncode, Opts).

binary_or_undefined(undefined) ->
    undefined;
binary_or_undefined(V) ->
    wf:to_binary(V).


create_option(Option) ->
    create_options(undefined, true, [Option]).

create_options(_,_,[]) ->
    [];
create_options(Selected,HtmlEncode, [{Value, Text} | Rest]) ->
    [create_option_from_tuple(Selected, HtmlEncode, {Value, Text}) | create_options(Selected, HtmlEncode, Rest)];
create_options(Selected,HtmlEncode, [OG = #option_group{show_if=true} | Rest]) ->
    [create_option_group(Selected, HtmlEncode, OG) | create_options(Selected, HtmlEncode, Rest)];
create_options(Selected,HtmlEncode,[#option_group{show_if=false} | Rest]) ->
    create_options(Selected,HtmlEncode,Rest);
create_options(Selected,HtmlEncode,[X=#option{show_if=true} | Rest]) ->
    [create_option_full(Selected, HtmlEncode, X) | create_options(Selected,HtmlEncode,Rest)];
create_options(Selected,HtmlEncode,[#option{show_if=false} | Rest]) ->
    create_options(Selected,HtmlEncode,Rest);
create_options(Selected,HtmlEncode,[Other | Rest]) when ?IS_STRING(Other); is_binary(Other); is_integer(Other); is_atom(Other) ->
    [create_option_simple(Selected, HtmlEncode, Other) | create_options(Selected,HtmlEncode, Rest)];
create_options(_,_,[Other | _]) ->
    throw({unknown_option_provided_to_dropdown_element,Other}).

create_option_group(Selected, HtmlEncode, #option_group{text=Text, options=Options, disabled=Disabled}) ->
    OptionTags = create_options(Selected, HtmlEncode, Options),
    LabelProp = {label, wf:html_encode(Text, HtmlEncode)},
    DisabledProp = ?WF_IF(Disabled, disabled, undefined),
    Props = [
        LabelProp,
        DisabledProp
    ],
    wf_tags:emit_tag(optgroup, OptionTags, Props).

create_option_simple(Selected, HtmlEncode, Value) ->
    create_option_from_tuple(Selected, HtmlEncode, {Value, Value}).

create_option_from_tuple(Selected, HtmlEncode, {Value, Text}) ->
    Option = #option{text=Text, value=Value},
    create_option_full(Selected, HtmlEncode, Option).

create_option_full(Selected, HtmlEncode, Opt = #option{text=Text, value=Value, disabled=Disabled}) ->
    Content = wf:html_encode(Text, HtmlEncode),
    SelectedProp = ?WF_IF(is_selected(Selected, Opt), selected, undefined),
    DisabledProp = ?WF_IF(Disabled, disabled, undefined),
    ValueProp = ?WF_IF(Value =:= undefined, [], {value, wf:html_encode(Value, HtmlEncode)}),
    Props = [
        SelectedProp,
        DisabledProp,
        ValueProp
    ],
    wf_tags:emit_tag(option, Content, Props).

-spec is_selected(DropDownValue :: binary()|undefined, X :: #option{}) -> boolean().
is_selected(_DropDownValue, #option{selected=true})  ->
    %% If the #option.selected=true, then short-circuit and return true.
    true;
is_selected(DropDownValue, #option{selected=OptSelected, value=OptValue})
        when OptSelected=:=false;
             OptValue=:=undefined;
             DropDownValue=:=undefined ->
    false;
is_selected(DropDownValue, #option{value=OptValue}) ->
    %% Finally, if none of the above short-circuits trip, then we can convert
    %% #option.value to binary and compare directly. If they match, then it's
    %% selected.
    wf:to_binary(OptValue) =:= DropDownValue.

-include_lib("eunit/include/eunit.hrl").

html_encode_test() ->
    ?assertEqual(true, is_selected(<<"anything">>, #option{selected=true})),
    ?assertEqual(false, is_selected(<<"anything">>, #option{selected=false})),
    ?assertEqual(true, is_selected(<<"apple">>, #option{value=apple})),
    ?assertEqual(false, is_selected(<<"other_thing">>, #option{value=apple})),
    ?assertEqual(true, is_selected(<<"apple">>, #option{value=apple, selected=true})),
    ?assertEqual(false, is_selected(<<"apple">>, #option{value=apple, selected=false})).
