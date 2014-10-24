% vim: sw=4 ts=4 et ft=erlang
% Nitrogen Web Framework for Erlang
% Copyright (c) 2008-2010 Rusty Klophaus
% See MIT-LICENSE for licensing information.

-module (element_hr).
-include("wf.hrl").
-export([
    reflect/0,
    render_element/1
]).

-spec reflect() -> [atom()].
reflect() -> record_info(fields, hr).

-spec render_element(#hr{}) -> body().
render_element(Record) -> 
    wf_tags:emit_tag(hr, [
        {id, Record#hr.html_id},
        {size, 1},
        {class, [hr, Record#hr.class]},
        {title, Record#hr.title},
        {style, Record#hr.style},
        {data_fields, Record#hr.data_fields}
    ]).
