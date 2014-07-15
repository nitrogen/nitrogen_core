% vim: ts=4 sw=4 et
% Nitrogen Web Framework for Erlang
% Copyright (c) 2008-2011 Rusty Klophaus
% Copyright (c) 2014 Jesse Gumm
% See MIT-LICENSE for licensing information.

-module (element_panel).
-include("wf.hrl").
-export([
    reflect/0,
    render_element/1
]).

-spec reflect() -> [atom()].
reflect() -> record_info(fields, panel).

-spec render_element(Record :: #panel{}) -> body().
render_element(Record) -> 
    Body = [
        wf:html_encode(Record#panel.text, Record#panel.html_encode),
        Record#panel.body
    ],
    wf_tags:emit_tag('div', Body, [
        {id, Record#panel.html_id},
        {class, Record#panel.class},
        {title, Record#panel.title},
        {style, Record#panel.style},
        {data_fields, Record#panel.data_fields}
    ]).
