% vim: sw=4 ts=4 et ft=erlang
% Nitrogen Web Framework for Erlang
% Copyright (c) 2008-2010 Rusty Klophaus
% See MIT-LICENSE for licensing information.

-module (element_table).
-include("wf.hrl").
-export([
    reflect/0,
    render_element/1
]).

-spec reflect() -> [atom()].
reflect() -> record_info(fields, table).

-spec render_element(#table{}) -> body().
render_element(Record) -> 

    Header = case Record#table.header of
      [] -> "";
      _ -> wf_tags:emit_tag(thead, Record#table.header, [])
    end,

    Footer = case Record#table.footer of
      [] -> "";
      _ -> wf_tags:emit_tag(tfoot, Record#table.footer, [])
    end,

    Body = wf_tags:emit_tag(tbody, Record#table.rows, []),
    Content = [Header, Footer, Body ],

    wf_tags:emit_tag( table, Content, [
        {id, Record#table.html_id},
        {border, 0},
        {class, [table, Record#table.class]},
        {style, Record#table.style},
        {data_fields, Record#table.data_fields}
    ]).
