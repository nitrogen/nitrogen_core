% vim: sw=4 ts=4 et ft=erlang
% Nitrogen Web Framework for Erlang
% See MIT-LICENSE for licensing information.

-module (element_aside).
-include("wf.hrl").
-export([
    reflect/0,
    render_element/1
]).

-spec reflect() -> [atom()].
reflect() -> record_info(fields, aside).

-spec render_element(#aside{}) -> body().
render_element(Record) ->
    wf_tags:emit_tag(aside, Record#aside.body, [
        {id, Record#aside.html_id},
        {class, ["aside", Record#aside.class]},
        {title, Record#aside.title},
        {style, Record#aside.style},
        {role, Record#aside.role},
        {data_fields, Record#aside.data_fields}
    ]).
