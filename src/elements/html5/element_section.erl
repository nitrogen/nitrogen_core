% vim: sw=4 ts=4 et ft=erlang
% Nitrogen Web Framework for Erlang
% See MIT-LICENSE for licensing information.

-module (element_section).
-include("wf.hrl").
-export([
    reflect/0,
    render_element/1
]).

-spec reflect() -> [atom()].
reflect() -> record_info(fields, section).

-spec render_element(#section{}) -> body().
render_element(Record) ->
    wf_tags:emit_tag(section, Record#section.body, [
        {id, Record#section.html_id},
        {class, ["section", Record#section.class]},
        {title, Record#section.title},
        {style, Record#section.style},
        {data_fields, Record#section.data_fields}
    ]).
