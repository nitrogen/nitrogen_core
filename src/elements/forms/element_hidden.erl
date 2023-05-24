% vim: sw=4 ts=4 et ft=erlang
% Nitrogen Web Framework for Erlang
% Copyright (c) 2008-2010 Rusty Klophaus
% See MIT-LICENSE for licensing information.

-module (element_hidden).
-include("wf.hrl").
-export([
    reflect/0,
    render_element/1
]).

-spec reflect() -> [atom()].
reflect() -> record_info(fields, hidden).

-spec render_element(#hidden{}) -> body().
render_element(Record) -> 
    Value = wf:html_encode(Record#hidden.text, Record#hidden.html_encode),
    wf_tags:emit_tag(input, [
        {id, Record#hidden.html_id},
        {class, Record#hidden.class},
        {type, hidden},
        {name, Record#hidden.html_name},
        {value, Value},
        {data_fields, Record#hidden.data_fields},
        {aria, Record#hidden.aria},
        ?WF_IF(Record#hidden.disabled, disabled, undefined)
    ]).
