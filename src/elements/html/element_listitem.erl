% vim: ts=4 sw=4 et
% Nitrogen Web Framework for Erlang
% Copyright (c) 2008-2010 Rusty Klophaus
% See MIT-LICENSE for licensing information.

-module (element_listitem).
-include("wf.hrl").
-export([
    reflect/0,
    render_element/1
]).

-spec reflect() -> [atom()].
reflect() -> record_info(fields, listitem).

-spec render_element(#listitem{}) -> body().
render_element(Record) -> 
    Body = [
        wf:html_encode(Record#listitem.text, Record#listitem.html_encode),
        Record#listitem.body
    ],

    wf_tags:emit_tag(li, Body, [
        {id, Record#listitem.html_id},
        {class, ?ADD_ELEMENT_CLASS(listitem, Record#listitem.class)},
        {title, Record#listitem.title},
        {role, Record#listitem.role},
        {style, Record#listitem.style},
        {data_fields, Record#listitem.data_fields}
    ]).
