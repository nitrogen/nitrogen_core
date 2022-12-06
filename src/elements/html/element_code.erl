% vim: sw=4 ts=4 et ft=erlang
% Nitrogen Web Framework for Erlang
% See MIT-LICENSE for licensing information.

-module (element_code).
-include("wf.hrl").
-export([
    reflect/0,
    render_element/1
]).

-spec reflect() -> [atom()].
reflect() -> record_info(fields, code).

-spec render_element(#code{}) -> body().
render_element(Record) ->
    Body = [
        wf:html_encode(Record#code.text, Record#code.html_encode),
        Record#code.body
    ],
    wf_tags:emit_tag(code, Body, [
        {class, [code, Record#code.class]},
        {title, Record#code.title},
        {data_fields, Record#code.data_fields},
        {style, Record#code.style}
    ]).
