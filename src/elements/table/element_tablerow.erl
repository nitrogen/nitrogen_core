% vim: sw=4 ts=4 et ft=erlang
% Nitrogen Web Framework for Erlang
% Copyright (c) 2008-2010 Rusty Klophaus
% See MIT-LICENSE for licensing information.

-module (element_tablerow).
-include("wf.hrl").
-export([
    reflect/0,
    render_element/1
]).

-spec reflect() -> [atom()].
reflect() -> record_info(fields, tablerow).

-spec render_element(#tablerow{}) -> body().
render_element(Record) -> 
    Cells = Record#tablerow.cells,
    wf_tags:emit_tag(tr, Cells, [
        {id, Record#tablerow.html_id},
        {class, [tablerow, Record#tablerow.class]},
        {title, Record#tablerow.title},
        {style, Record#tablerow.style},
        {data_fields, Record#tablerow.data_fields}
    ]).
