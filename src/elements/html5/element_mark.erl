% vim: sw=4 ts=4 et ft=erlang
% Nitrogen Web Framework for Erlang
% See MIT-LICENSE for licensing information.

-module (element_mark).
-include("wf.hrl").
-export([
    reflect/0,
    render_element/1
]).

-spec reflect() -> [atom()].
reflect() -> record_info(fields, mark).

-spec render_element(#mark{}) -> body().
render_element(Record) ->
    wf_tags:emit_tag(mark, Record#mark.body, [
        {id, Record#mark.html_id},
        {class, ["mark", Record#mark.class]},
        {style, Record#mark.style},
        {data_fields, Record#mark.data_fields}
    ]).
