% vim: sw=4 ts=4 et ft=erlang
% Nitrogen Web Framework for Erlang
% Copyright (c) 2008-2010 Rusty Klophaus
% See MIT-LICENSE for licensing information.

-module (element_button).
-include("wf.hrl").
-export([
    reflect/0,
    render_element/1
]).

-spec reflect() -> [atom()].
reflect() -> record_info(fields, button).

-spec render_element(#button{}) -> body().
render_element(Record) ->
    ID = Record#button.id,
    Anchor = Record#button.anchor,
    case Record#button.postback of
        undefined -> ignore;
        Postback -> wf:wire(Anchor, #event {
                    type=click,
                    validation_group=ID,
                    postback=Postback,
                    handle_invalid=Record#button.handle_invalid,
                    on_invalid=Record#button.on_invalid,
                    delegate=Record#button.delegate })
    end,

	case Record#button.click of
		undefined -> ignore;
		ClickActions -> wf:wire(Anchor, #event { type=click, actions=ClickActions })
	end,

    Text = wf:html_encode(Record#button.text, Record#button.html_encode), 
  
    Image = format_image(Record#button.image),
    Body = case {Image,Record#button.body} of
        {[], []} -> [];
        {I, B} -> [I, B]
    end,


    UniversalAttributes = [
        {id, Record#button.html_id},
        {class, [button, Record#button.class]},
        {title, Record#button.title},
        {style, Record#button.style},
        {data_fields, Record#button.data_fields},
        ?WF_IF(Record#button.disabled, disabled)
    ],

    case Body of
        [] ->
            wf_tags:emit_tag(input, [
                {type, button},
                {value, Text}
                | UniversalAttributes
            ]);
        _ ->
            wf_tags:emit_tag(button, [Body, Text], UniversalAttributes)
    end.

format_image(undefined) -> [];
format_image([]) -> [];
format_image(Path) -> [#image{image=Path}," "].
