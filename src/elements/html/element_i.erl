% vim: sw=4 ts=4 et ft=erlang
% Nitrogen Web Framework for Erlang
% See MIT-LICENSE for licensing information.

-module (element_i).
-include("wf.hrl").
-export([
    reflect/0,
    render_element/1
]).

-spec reflect() -> [atom()].
reflect() -> record_info(fields, i).

-spec render_element(#i{}) -> body().
render_element(Record) -> 
    Body = [
        wf:html_encode(Record#i.text, Record#i.html_encode),
        Record#i.body
    ],
    wf_tags:emit_tag(i, Body, [
        {id, Record#i.html_id},
        {class, [i, Record#i.class]},
        {title, Record#i.title},
        {style, Record#i.style},
        {data_fields, Record#i.data_fields}
    ]).

