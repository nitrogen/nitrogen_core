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

    #basic_tag{
        text=Text,
        body=Body0,
        html_encode=HtmlEncode,
        html_id=HtmlID,
        class=Class,
        title=Title,
        data_fields=DataFields,
        aria=Aria,
        role=Role,
        style=Style
    } = Rec,

    Body = [
        wf:html_encode(Text, HtmlEncode),
        Body0
    ],
    wf_tags:emit_tag(Tag, Body, [
        {id, HtmlID},
        {class, ?ADD_ELEMENT_CLASS(backcompat_element_class(ElementName), Class)},
        {title, Title},
        {data_fields, DataFields},
        {aria, Aria},
        {role, Role},
        {style, Style}
    ]).

-ifdef(DO_ELEMENT_CLASSES).
%% We just wrap this to prevent the error that the function is not used if
%% we're not using the backcompat element classes
backcompat_element_class(label) ->
    nitrogen_label;
backcompat_element_class(X) ->
    X.
-endif.

%% For the most part, the name of the HTML tag name and the record will be the same.
%% For example, #span{} will become <span>.
%% But for some, we may change the tag name from the element name.
%% The most common example would be #panel{} becomes <div> (since div is a keyword in Erlang)
tagname(panel) ->
    'div';
tagname(html5_header) ->
    'header';
tagname(html5_footer) ->
    'footer';
tagname(Element) ->
    Element.
