% vim: sw=4 ts=4 et ft=erlang
% Nitrogen Web Framework for Erlang
% See MIT-LICENSE for licensing information.

-module (element_main).
-include("wf.hrl").
-export([
    reflect/0,
    render_element/1
]).

-spec reflect() -> [atom()].
reflect() -> record_info(fields, main).

-spec render_element(#main{}) -> body().
render_element(Record) ->
    wf_tags:emit_tag(main, Record#main.body, [
        {id, Record#main.html_id},
        {class, ["main", Record#main.class]},
        {title, Record#main.title},
        {style, Record#main.style},
        {role, Record#main.role},                                              
        {data_fields, Record#main.data_fields}
    ]).
