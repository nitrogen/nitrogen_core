% vim: sw=4 ts=4 et ft=erlang
% Nitrogen Web Framework for Erlang
% Copyright (c) 2008-2010 Rusty Klophaus
% See MIT-LICENSE for licensing information.

-module (element_tablecell).
-include("wf.hrl").
-export([
    reflect/0,
    render_element/1
]).

-spec reflect() -> [atom()].
reflect() -> record_info(fields, tablecell).

-spec render_element(#tablecell{}) -> body().
render_element(Record) -> 
    Body = [
        wf:html_encode(Record#tablecell.text, Record#tablecell.html_encode),
        Record#tablecell.body
    ],
    Type = case Record#tablecell.isheader of true -> th; false -> td end,
    wf_tags:emit_tag(Type, Body, [
        {id, Record#tablecell.html_id},
        {class, [tablecell, Record#tablecell.class]},
        {title, Record#tablecell.title},
        {style, Record#tablecell.style},
        {align, Record#tablecell.align},
        {valign, Record#tablecell.valign},
        {colspan, Record#tablecell.colspan},
        {rowspan, Record#tablecell.rowspan},
        {data_fields, Record#tablecell.data_fields}
    ]).
