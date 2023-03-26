% vim: sw=4 ts=4 et ft=erlang
% Nitrogen Web Framework for Erlang
% Copyright (c) 2008-2010 Rusty Klophaus
% See MIT-LICENSE for licensing information.

-module (element_br).
-include("wf.hrl").
-export([
    reflect/0,
    render_element/1
]).

-spec reflect() -> [atom()].
reflect() -> record_info(fields, br).

-spec render_element(#br{}) -> body().
render_element(Record) -> 
    wf_tags:emit_tag(br, [
        {id, Record#br.html_id},
        {class, ?ADD_ELEMENT_CLASS(br, Record#br.class)},
        {title, Record#br.title},
        {style, Record#br.style},
        {data_fields, Record#br.data_fields}
    ]).



