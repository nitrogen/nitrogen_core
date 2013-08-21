% vim: sw=4 ts=4 et ft=erlang
% Nitrogen Web Framework for Erlang
% Copyright (c) 2008-2010 Rusty Klophaus
% See MIT-LICENSE for licensing information.

-module (element_value).
-include("wf.hrl").
-export([
    reflect/0,
    render_element/1
]).

-spec reflect() -> [atom()].
reflect() -> record_info(fields, value).

-spec render_element(#value{}) -> body().
render_element(Record) -> 
    Text = wf:html_encode(Record#value.text, Record#value.html_encode),
    wf_tags:emit_tag(span, Text, [
        {id, Record#value.html_id},
        {class, [value, Record#value.class]},
        {style, Record#value.style},
        {data_fields, Record#value.data_fields}
    ]).
