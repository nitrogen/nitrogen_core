% vim: sw=4 ts=4 et ft=erlang
% Nitrogen Web Framework for Erlang
% Copyright (c) 2008-2010 Rusty Klophaus
% See MIT-LICENSE for licensing information.

-module (element_singlerow).
-include("wf.hrl").
-export([
    reflect/0,
    render_element/1
]).

-spec reflect() -> [atom()].
reflect() -> record_info(fields, singlerow).

-spec render_element(#singlerow{}) -> body().
render_element(Record) -> 
    Table = #table {
        html_id=Record#singlerow.html_id,
        id=Record#singlerow.id,
        anchor=Record#singlerow.anchor,
        class=[singlerow, Record#singlerow.class],
        title=Record#singlerow.title,
        style=Record#singlerow.style,
        data_fields=Record#singlerow.data_fields,
        aria=Record#singlerow.aria,
        rows=#tablerow { cells=Record#singlerow.cells }
    },
    element_table:render_element(Table).
