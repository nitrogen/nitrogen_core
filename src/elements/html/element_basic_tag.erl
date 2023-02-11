% vim: sw=4 ts=4 et ft=erlang
% Nitrogen Web Framework for Erlang
% See MIT-LICENSE for licensing information.

-module(element_basic_tag).
-include("wf.hrl").
-export([
    reflect/0,
    render_element/1
]).

-spec reflect() -> [atom()].
reflect() -> record_info(fields, basic_tag).

-spec render_element(tuple()) -> body().
render_element(Rec0) ->
    ElementName = element(1, Rec0),
    Tag = tagname(ElementName),
    Rec = setelement(1, Rec0, basic_tag),

    case is_record(Rec, basic_tag) of
        true -> ok;
        false -> throw({unable_to_convert_to_basic_tag, Rec0})
    end,

    Body = [
        wf:html_encode(Rec#basic_tag.text, Rec#basic_tag.html_encode),
        Rec#basic_tag.body
    ],
    wf_tags:emit_tag(Tag, Body, [
        {id, Rec#basic_tag.html_id},
        {class, Rec#basic_tag.class},
        {title, Rec#basic_tag.title},
        {data_fields, Rec#basic_tag.data_fields},
        {style, Rec#basic_tag.style}
    ]).


%% For the most part, the name of the HTML tag name and the record will be the same.
%% For example, #span{} will become <span>.
%% But for some, we may change the tag name from the element name.
%% The most common example would be #panel{} becomes <div> (since div is a keyword in Erlang)
tagname(panel) ->
    'div';
tagname(Element) ->
    Element.
