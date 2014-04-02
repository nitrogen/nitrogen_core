% vim: sw=4 ts=4 et ft=erlang
% Nitrogen Web Framework for Erlang
% Copyright (c) 2008-2010 Rusty Klophaus
% See MIT-LICENSE for licensing information.

-module (element_span).
-include("wf.hrl").
-export([
    reflect/0,
    render_element/1
]).

-spec reflect() -> [atom()].
reflect() -> record_info(fields, span).

-spec render_element(#span{}) -> body().
render_element(Record) -> 
    Body = [
        wf:html_encode(Record#span.text, Record#span.html_encode),
        Record#span.body
    ],

    wf_tags:emit_tag(span, Body, [
        {id, Record#span.html_id},
        {class, Record#span.class},
        {title, Record#span.title},
        {style, Record#span.style},
        {title, wf:html_encode(Record#span.title, Record#span.html_encode)},
        {data_fields, Record#span.data_fields}
    ]).
